

import sys
sys.path.append('../python/')

import pandas as pd

file = 'data/milano-grid.geojson'
with open(file) as f:
    grid = pd.read_json(f, typ='Series')


grid= grid.groupby('properties.cellId').agg({
                        'properties.cellId': 'first', 
                        'geometry.coordinates': 'first',
                        'geometry.type': 'first',
                       })