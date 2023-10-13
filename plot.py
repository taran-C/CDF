import numpy as np
#import json
import matplotlib.pyplot as plt
from netCDF4 import Dataset

input_file = "a.nc"

nc_file = Dataset(input_file, "r")

x = nc_file.variables["x(t)"][:,1,1,1]
y = nc_file.variables["y(t)"][:,1,1,1]
z = nc_file.variables["z(t)"][:,1,1,1]

ax = plt.axes(projection='3d')

ax.plot(x,y,z, lw = 0.5)

plt.show()

