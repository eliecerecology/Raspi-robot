from __future__ import division  # 2.2+-only
import RPi.GPIO as GPIO
import time
import sensor1
from sensor1 import distance 

GPIO.setmode(GPIO.BOARD)
GPIO.setup(22, GPIO.OUT)


right = 0.5 # ms
left  = 2.5 # ms
neutral = 1.5 # ms
freq = 40
Pulse = (1/freq)*1000 #mili second

a = GPIO.PWM(22
,freq)
Pos = [right,neutral,left]
a.start(7.5)

time.sleep(1)

dist = []

for item in Pos:#[0:3]:
    distance('cm')
    dist.append(distance('cm'))
    time.sleep(1)
    dc = (item / Pulse) * 100  #percentage of duty cycle
    a.ChangeDutyCycle(dc)
    print item, dc
    time.sleep(1)

print dist
a.stop()
GPIO.cleanup()
import stop



'''
try:
        while True:
            a.ChangeDutyCycle(7.5) # neutral
            time.sleep(2)
            a.ChangeDutyCycle(9.5)
            time.sleep(2)

            a.ChangeDutyCycle(2.5)
            time.sleep(2)


except KeyboardInterrupt:
      GPIO.cleanup()
'''



