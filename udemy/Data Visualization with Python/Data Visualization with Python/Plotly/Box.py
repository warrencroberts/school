import plotly.offline as pyo
import plotly.graph_objs as go

import numpy as np

y1 = np.random.randn(30) + 5
y2 = np.random.randn(30) - 5

trace1 = go.Box(y = y1)
trace2 = go.Box(y = y2)

data = [trace1, trace2]

pyo.plot(data, filename='Box.html')
