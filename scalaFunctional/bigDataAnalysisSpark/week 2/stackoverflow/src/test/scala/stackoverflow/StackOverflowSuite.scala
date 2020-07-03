package stackoverflow

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import StackOverflow.{rawPostings, scoredPostings, _}
import org.apache.spark.rdd.RDD

import scala.collection.SortedSet

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {
  def initializeStackOverflow: Boolean = {
    try {
      testObject
      true
    } catch {
      case ex: Throwable =>
        println(ex.getMessage)
        ex.printStackTrace()
        false
    }
  }

  override def afterAll(): Unit = {
    assert(initializeStackOverflow, " -- did you fill in all the values in StackOverflow (conf, sc)?")
    sc.stop()
  }


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    override def langSpread = 50000

    override def kmeansKernels = 45

    override def kmeansEta: Double = 20.0D

    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("groupedPostings") {
    //    assert(initializeStackOverflow, " -- did you fill in all the values in WikipediaRanking (conf, sc, wikiRdd)?")

    val testCSV = Array(
      "1,9002525,,,2,C++",
      "2,9003401,,9002525,4,",
      "2,9003942,,9002525,1,",
      "2,9005311,,9002525,0,",
      "1,28903923,,,0,PHP",
      "2,28904080,,28903923,0,")

    val rp = rawPostings(sc.parallelize(testCSV))
    val rpList = rp.collect().toList

    val groupdReturn = groupedPostings(rp).collect().toMap

    assert(groupdReturn.size == 2)
    assert(groupdReturn(9002525).size == 3)
    assert(groupdReturn(28903923).size == 1)
  }

  test("score  simple") {
    //    assert(initializeStackOverflow, " -- did you fill in all the values in WikipediaRanking (conf, sc, wikiRdd)?")

    val testCSV = Array(
      "1,9002525,,,2,C++",
      "2,9003401,,9002525,4,",
      "2,9003942,,9002525,1,",
      "2,9005311,,9002525,0,",
      "1,28903923,,,0,PHP",
      "2,28904080,,28903923,0,")

    val rp = rawPostings(sc.parallelize(testCSV))
    val rpList = rp.collect().toList
    val scoredReturn = scoredPostings(groupedPostings(rp)).collect.toMap

    assert(scoredReturn.size == 2)
  }

  test("score all the data") {
    assert(initializeStackOverflow, " -- did you fill in all the values in WikipediaRanking (conf, sc, wikiRdd)?")

    val rp = rawPostings(sc.textFile("src/main/resources/stackoverflow.csv"))
    val sp = scoredPostings(groupedPostings(rp))

    val testScores = List(
      (Posting(postingType = 1, id = 6, acceptedAnswer = None, parentId = None, score = 140, tags = Some("CSS")), 67),
      (Posting(postingType = 1, id = 42, acceptedAnswer = None, parentId = None, score = 155, tags = Some("PHP")), 89),
      (Posting(postingType = 1, id = 72, acceptedAnswer = None, parentId = None, score = 16, tags = Some("Ruby")), 3),
      (Posting(postingType = 1, id = 126, acceptedAnswer = None, parentId = None, score = 33, tags = Some("Java")), 30),
      (Posting(postingType = 1, id = 174, acceptedAnswer = None, parentId = None, score = 38, tags = Some("C#")), 20)
    )

    val ts = sc.parallelize(testScores)

    val theResult = ts.join(sp).collect.toMap

    testScores.foreach {res =>
      assert(theResult(res._1)._1 == res._2)
    }

  }

  test("get vectors") {

    def printGrouped(g: Array[(Int, Iterable[(Posting, Posting)])]): Unit = {
      g.foreach {entry =>
        println(s"qid : ${entry._1}, Question: ${entry._2.head}")
        entry._2.foreach{value =>
          println(s"........> ${value._2}")
        }
      }
    }

    def printGroupedElement(gp: RDD[(Int, Iterable[(Posting, Posting)])], lang: String, score: Int): Unit = {
      val pearl: Array[(Int, Iterable[(Posting, Posting)])] = gp.filter{ x =>
        val valIterator: Iterable[(Posting, Posting)] = x._2
        val question: Posting = valIterator.head._1
        question.tags.contains(lang) && (valIterator.count(_._2.score == score) > 0)
      }.collect()
      printGrouped(pearl)
    }

    assert(initializeStackOverflow, " -- did you fill in all the values in WikipediaRanking (conf, sc, wikiRdd)?")

    val gp: RDD[(Int, Iterable[(Posting, Posting)])] = groupedPostings(rawPostings(sc.textFile("src/main/resources/stackoverflow.csv"))).persist

    val vp = vectorPostings(scoredPostings(gp))

    val testFiles = List((100000,58), (450000,1), (200000,53), (550000,22), (450000,8))
    val tf = sc.parallelize(testFiles)

    val testJoin: Array[(Int, Int)] = vp.intersection(tf).collect
    testJoin.foreach {x =>
      println(s"testFile -- (${x._1}, ${x._2})")
      printGroupedElement(gp, StackOverflow.langs(x._1 / StackOverflow.langSpread), x._2)
    }

    assert(testJoin.nonEmpty, " -- Found unexpected elements")
  }

  test("kmeans sampled single iteration") {

    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw     = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val scored  = scoredPostings(grouped)
    val vectors = vectorPostings(scored)
    assert(vectors.count() == 2121822, "Incorrect number of vectors: " + vectors.count())

    val means   = kmeans(sampleVectors(vectors), vectors, iter = kmeansMaxIterations, debug = true)
    val results = clusterResults(means, vectors)
    printResults(results)

    println("------------------------------------------------------\n")

    results.filter(_._1 == "C#").foreach {
      case (lang, percent, size, score) =>
        println(f"$score%7d  $lang%-17s ($percent%-5.1f%%)      $size%7d")
    }

    results.filter(_._1 == "Groovy").foreach {
      case (lang, percent, size, score) =>
        println(f"$score%7d  $lang%-17s ($percent%-5.1f%%)      $size%7d")
    }

    val testResults = Array(("C#",100,9172,0), ("C#",100,7292,3), ("C#",100,1336,15))

    val expectedResultes = results.intersect(testResults)
    assert(expectedResultes.length == 3)

  }

  test("kmeans sampled") {
    val sp = scoredPostings(groupedPostings(rawPostings(sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv"))))
    //
    assert(sp.count() == 2121822, "Incorrect number of vectors: " + sp.count())

    val vectors: RDD[(Int, Int)] = vectorPostings(sp)
    assert(vectors.count() == 2121822, "Incorrect number of vectors: " + vectors.count())

    val sampleVecs = sampleVectors(vectors)

    val means   = kmeans(sampleVecs, vectors, debug = true)
    val results = clusterResults(means, vectors)
    printResults(results)

    println("------------------------------------------------------\n")

    results.filter(_._1 == "C#").foreach {
      case (lang, percent, size, score) =>
        println(f"$score%7d  $lang%-17s ($percent%-5.1f%%)      $size%7d")
    }

  }


  test("cluster") {
    val vectors = StackOverflow.sc.parallelize(List( (450000, 39),(500000, 31),(150000,1),(150000,10),(500000, 55),(150000,2) ,(150000,22)))
    val means = Array((500000, 13),(150000,10))
    var results: Array[(String, Double, Int, Int)] = testObject.clusterResults(means, vectors)

    testObject.printResults(results)
    println(results(0))
    println(results(1))
    assert(results.contains("Python", 100.0, 4, 6)) //I like python~!
    assert(results.contains("Scala", 66.66666666666666, 3,39))
  }

  test("Shouldn't get unexpected centroids") {
    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw     = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val scored  = scoredPostings(grouped)
    val vectors = vectorPostings(scored)
    assert(vectors.count() == 2121822, "Incorrect number of vectors: " + vectors.count())

    val means   = kmeans(sampleVectors(vectors), vectors, debug = true)

    val expected   = List((0,458), (0,2), (0,115), (50000,2), (50000,929), (50000,141), (100000,42), (100000,385), (100000,1), (150000,70))
    val unexpected = List((100000,55), (250000,155), (200000,50), (450000,1), (550000,22))

    means.sortBy(_._1).foreach {x =>
      println(s".... (${x._1}, ${x._2})")
    }

    unexpected.foreach {x =>
      println(s"the unexpected tests (${x._1}, ${x._2})")
      assert(!means.contains(x))
    }

    expected.foreach{x =>
      println(s"the expected tests (${x._1}, ${x._2})")
      means.contains(x)
    }
  }
}
