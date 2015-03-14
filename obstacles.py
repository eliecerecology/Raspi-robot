import RPi.GPIO as GPIO
import time
from time import sleep
import datetime
from sensor1 import distance

f = 90 # frequency
ri = 52
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

#PARAMETERS
dist = []

for item in range (0,4):
    distance('cm') #front
    time.sleep(1)
    print (distance('cm')), item, "distance"
    dist.append(distance('cm'))
    time.sleep(0.5)
    right(torque) # y/4
    time.sleep(0.5)

if (dist[0] > dist[1]) and (dist[0] > dist[2]) and (dist[0] > dist[3]):
    print "dist 1st, front"
    dk0 = dist[0] - 5 # distance - 5 cm
    tc = dk0/v
    forward(tc)
    print (tc), dk0, v

elif (dist[1] > dist[2]) and (dist[1] > dist[3]):
    print "dist 2nd, going right"
    right(torque)
    dk1 = dist[1] - 5 # distance - 5 cm
    tc = dk1/v
    forward(tc)
    print (tc), dk1, v

elif (dist[2] > dist[3]):
    init()
    print "dist 3rd, back"
    right(torque)
    dk2 = dist[2] - 5 # distance - 5 cm
    tc = dk2/v
    forward(tc)
    print (tc), dk2, v

else:
    print "dist 4rd, going left"
    left(torque)
    dk3 = dist[3] - 5 # distance - 5 cm
    tc = dk3/v
    forward(tc)
    print (tc), dk3, v
