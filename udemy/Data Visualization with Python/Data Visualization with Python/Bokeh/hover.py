from bokeh.plotting import figure
from bokeh.io import output_file, show
from bokeh.models import HoverTool

p = figure()

p.circle(x= [1,2,3,4], y =[2,3,4,7], size = [10, 20, 30, 40], alpha = 0.6 , fill_color ='green',
        hover_fill_color = 'red', hover_alpha = 0.5)

hover = HoverTool(tooltips = None)

p.add_tools(hover)

output_file('hover.html')

show(p)
