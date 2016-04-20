import RPi.GPIO as GPIO
import time
from sensor1 import distance
import math
import matplotlib.pyplot as plt

#hola
f = 90 # frequency
ri = 62
le = 70
spin = 0.4
turn = 4
torque = spin/turn
v = 28 # cm/s I have to measure it again!

def init():
    GPIO.setmode(GPIO.BOARD)
    GPIO.setup(7,GPIO.OUT)
    GPIO.setup(11,GPIO.OUT)
    GPIO.setup(13,GPIO.OUT)
    GPIO.setup(15,GPIO.OUT)

def stop():
    GPIO.output(7,False)
    GPIO.output(11,False)
    GPIO.output(13,False)
    GPIO.output(15,False)

def right(tf):
    init()
    r = GPIO.PWM(7,f)
    l = GPIO.PWM(13,f)
    for repeat in range (1,6):
        for i,j in zip(range(52,53),range(60,66)):      # 
            r.start(i)
            l.start(j)
            time.sleep(tf) #tf

def left(tf):
    init()
    r = GPIO.PWM(15,f)
    l = GPIO.PWM(11,f)
    for repeat in range (1,6):
        for i,j in zip(range(52,53),range(60,66)):      # 
            r.start(i)
            l.start(j)
            time.sleep(tf) #tf

def forward(tf):
    r = GPIO.PWM(7,f)
    l = GPIO.PWM(15,f)
    for repeat in range (1,6):
        for i,j in zip(range(52,53),range(60,66)):      # 
            r.start(i)
            l.start(j)
#            print i,j # prints only once
            time.sleep(1) #tf
#        print 'time', 'right', 'left'  
#        print repeat, i, j # this always print same numbers

def reverse(tf):
    init()
    r = GPIO.PWM(11,f)
    l = GPIO.PWM(13,f)
    for repeat in range (1,6):
        for i,j in zip(range(52,53),range(60,66)):      # 
            r.start(i)
            l.start(j)
            time.sleep(tf) #tf

#Detection of obstacles

angles = [0, 45, 90, 135, 180, 225, 270, 315]
angles = [math.radians(x) for x in angles]
distances = []

for angle in angles:
    dist = distance('cm')
    print 'distance at', math.degrees(angle), ':', dist
    distances.append(dist)
    right(torque)
    time.sleep(1)

angles.append(angles[0])
distances.append(distances[0])
    
ax = plt.subplot(111, polar=True)
ax.plot(angles, distances, color='r', linewidth=3)
ax.grid(True)
plot.show()

