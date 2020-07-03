from bokeh.plotting import figure
from bokeh.io import output_file, show

from bokeh.sampledata.iris import flowers

p = figure()

print(flowers.columns)

p.circle(x= flowers['petal_width'], y = flowers['petal_length'], color ='red', size = 10, alpha = 0.5)

output_file('real-data.html')

show(p)
