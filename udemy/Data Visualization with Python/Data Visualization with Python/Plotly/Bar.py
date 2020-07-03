import plotly.offline as pyo
import plotly.graph_objs as go

import numpy as np
'''
Jan  - 15
Feb - 24
March - 10
'''

trace1 = go.Bar(x = ['Jan', 'Feb', 'March'], y = [15, 24, -10],
                text= ['Jan Sale', 'Feb Sale', 'March Sale'])

trace2 = go.Bar(x = ['Jan', 'Feb', 'March'], y = [25, 4, 12],
                text= ['Jan Sale', 'Feb Sale', 'March Sale'])


data = [trace1, trace2]

pyo.plot(data, filename='Bar.html')
