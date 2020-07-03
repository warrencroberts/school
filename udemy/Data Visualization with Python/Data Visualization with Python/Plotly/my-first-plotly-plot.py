import plotly.offline as pyo
import plotly.graph_objs as go

import numpy as np

x1 = np.random.randn(10000)
x2 = np.random.rand(10000)

trace1 = go.Histogram(x = x1, histnorm= 'probability')
trace2 = go.Histogram(x = x2, histnorm= 'probability')

data = [trace1, trace2]

pyo.plot(data, filename='my-first-plotly-plot.html')
