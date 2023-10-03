import numpy as np
import json
import matplotlib.pyplot as plt

input_file = "res.txt"

# data = np.array([line.rstrip().split() for line in open(input_file).readlines()])

# data = data.astype(np.float)

# ax = plt.axes(projection='3d')

# ax.plot(*data.T, lw = 0.5)

# plt.show()

data = np.array([json.loads(line) for line in open(input_file).readlines()])

my_data = data[:,0,2,1,:]

ax = plt.axes(projection='3d')

ax.plot(*my_data.T, lw = 0.5)

plt.show()

