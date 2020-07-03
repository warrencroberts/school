from bokeh.plotting import figure
from bokeh.io import output_file, show

p = figure()
'''
(1,2)
(2,3)
(3,4)
(4,7)
'''
#p.circle(x= [1,2,3,4], y =[2,3,4,7], size = [10, 20, 30, 40])
#p.triangle(x= [1,2,3,4], y =[2,3,4,7], size = [10, 20, 30, 40])
p.square(x= [1,2,3,4], y =[2,3,4,7], size = [10, 20, 30, 40])

output_file('scatter.html')

show(p)
