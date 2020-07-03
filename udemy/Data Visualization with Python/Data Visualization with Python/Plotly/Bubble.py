import plotly.offline as pyo
import plotly.graph_objs as go

import numpy as np

x = np.linspace(0, 10, 5)
y = np.random.randn(5)

trace1 = go.Scatter(x = x, y = y, mode='markers',
                marker=dict(size = [10, 20, 100, 50, 32] , showscale = True))

data = [trace1]

pyo.plot(data, filename='Bubble.html')
