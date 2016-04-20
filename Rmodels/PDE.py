# -*- coding: utf-8 -*-
"""
Created on Thu Apr 14 20:33:59 2016

@author: ellis
"""

import numpy as np
import matplotlib.pyplot as plt
#%matplotlib inline

#parameters
a = 2.8e-4
b = 5e-3
tau = .1
k = -.005

size = 100  # size of the 2D grid
dx = 2./size  # space step

T = 10.0  # total time
dt = .9 * dx**2/2  # time step
n = int(T/dt)

U = np.random.rand(size, size)
V = np.random.rand(size, size)

def laplacian(Z):
    Ztop = Z[0:-2,1:-1]
    Zleft = Z[1:-1,0:-2]
    Zbottom = Z[2:,1:-1]
    Zright = Z[1:-1,2:]
    Zcenter = Z[1:-1,1:-1]
    return (Ztop + Zleft + Zbottom + Zright - 4 * Zcenter) / dx**2
