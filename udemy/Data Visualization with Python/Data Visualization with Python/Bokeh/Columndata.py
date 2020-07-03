from bokeh.plotting import figure, ColumnDataSource
from bokeh.io import output_file, show

p = figure()

import pandas as pd
df = pd.read_csv('literacy_birth_rate.csv')
print(df.columns)

source = ColumnDataSource(df)

p.circle(source = source , x = 'population', y = 'fertility', size = 10, color = 'red')

output_file('ColumnDataSource.html')

show(p)
