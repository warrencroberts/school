import plotly.offline as pyo
import plotly.graph_objs as go

import numpy as np

x = np.linspace(0, 10, 30)
y = np.random.randn(30)

trace1 = go.Scatter(x = x, y = y, mode='markers+lines')

data = [trace1]

pyo.plot(data, filename='Scatter.html')
