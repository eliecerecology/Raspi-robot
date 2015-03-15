import RPi.GPIO as GPIO
import time
GPIO.setmode(GPIO.BOARD)
GPIO.setup(7, GPIO.OUT)

a = GPIO.PWM(7,50)
a.start(7.5)



try:
        while True:
            a.ChangeDutyCycle(7.5) # neutral
            time.sleep(2)
            a.ChangeDutyCycle(9.5)
            time.sleep(2)
            '''
            a.ChangeDutyCycle(2.5)
            time.sleep(2)
            '''

except KeyboardInterrupt:
      GPIO.cleanup()
