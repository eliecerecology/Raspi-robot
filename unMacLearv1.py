#http://www.python-course.eu/matrix_arithmetic.php
import numpy as np
import matplotlib.pyplot as plt
#from matplotlib import style
from sklearn.cluster import KMeans


x = [1, 2, 3, 2, 1.5]
y = [3, 5, 6, 3, 3.5]

plt.scatter(x,y)

plt.show()

X = np.array([[1,3],
             [2,5],
             [3,6],
             [2,3],
             [1.5,3.5]])
kmeans = KMeans(n_clusters=2)
kmeans.fit(X)

centroids = kmeans.cluster_centers_
labels = kmeans.labels_

print(centroids)
print(labels)

colors = ["g.","r."]

for i in range(len(X)):
    print("coordinate:",X[i], "label:", labels[i])
    plt.plot(X[i][0],X[i][1], colors[labels[i]], markersize = 10)

plt.scatter(centroids[:,0],centroids[:,1], marker = "x", s=150, linewidths = 5, zorder = 10)
plt.show()
    
