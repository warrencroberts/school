from bokeh.plotting import figure
from bokeh.io import output_file, show

from bokeh.layouts import row, column, gridplot

p1 = figure(title = 'P1', plot_width = 300, plot_height = 300)
p2 = figure(title = 'P2', plot_width = 300, plot_height = 300)
p3 = figure(title = 'P3', plot_width = 300, plot_height = 300)

p1.circle(x= [1,2], y =[2,3])
p2.circle(x= [1,2], y =[2,3])
p3.circle(x= [1,2], y =[2,3])

#layout = row(p1, p2)
#layout = column(p1, p2)
#layout = row(column(p1, p2), p3)
layout = gridplot([ [None, p1] , [p2, p3] ])

output_file('layout.html')

show(layout)
