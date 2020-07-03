from bokeh.plotting import figure
from bokeh.io import output_file, show

p = figure()
'''
(1,2)
(2,3)
(3,4)
(4,7)
'''
#p.circle(x= [1,2,3,4], y =[2,3,4,7])
#p.line(x= [1,2,3,4], y =[2,3,4,7], line_color = '#FF00FF', line_width = 4)

#p.rect(1, 5 , 5, 7)
#p.ellipse(1, 5 , 5, 20)

p.patches(xs = [[1,1,2,2]], ys =[[2,5,5,3]], fill_color = ['red'])

output_file('line.html')

show(p)
