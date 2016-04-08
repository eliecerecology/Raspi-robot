import pygame
#import RPi.GPIO as GPIO
import time
import sys
from pygame.locals import *
import RPi.GPIO as GPIO

pwm = {}

RIGHT = 7
RIGHT_BACK = 11
LEFT_BACK = 13
LEFT = 15
pins = [RIGHT, LEFT, LEFT_BACK, RIGHT_BACK]

def init():
    GPIO.setmode(GPIO.BOARD)
    for pin in pins:
        GPIO.setup(pin, GPIO.OUT)
        GPIO.output(pin, False)
        pwm[pin] = GPIO.PWM(pin, 100)

def off():
    for pin in pins:
        pwm[pin].stop()

def move(pins=[], speed=50):
    for pin in pins:
        realspeed = speed
        if pin in [RIGHT, RIGHT_BACK]:
            realspeed = speed * 0.50
        pwm[pin].start(realspeed)

def left(tf):
    move([LEFT])
    time.sleep(tf)
    
def right(tf):
    move([RIGHT])
    time.sleep(tf)
    
def forward(tf):
    move([RIGHT, LEFT])
    time.sleep(tf)
    
def fast(tf):
    move([RIGHT, LEFT], 100)
    time.sleep(tf)
def reverse(tf):
    move([RIGHT_BACK, LEFT_BACK])
    time.sleep(tf)
pygame.init()

white = (255,255,255)
black = (0,0,0)
red = (255,0,0)
yellow = (255,255,0)
cyan = (0,255,255)
purple = (255,0,255)


setDisplay = pygame.display.set_mode((1000,1000))
pygame.display.set_caption('hola')


#singlePixil = pygame.PixelArray(setDisplay)
#singlePixil[3][3] = yellow

setDisplay.fill(white)
clock = pygame.time.Clock()
#pygame.draw.line(setDisplay, yellow,(30,30),(100,100),4)
#pygame.draw.circle(setDisplay, red,(200,150), 100, 5)
#pygame.draw.rect(setDisplay, purple,(100,100,200,100))
img1 = pygame.image.load('arrow.png')
img = pygame.transform.scale(img1, (60, 160))
#def imagen(x,y):
#setDisplay.blit(img,(x,y))
x = 50
y = 50
#pygame.draw.polygon(setDisplay, yellow,((80,60),(100,100),(80,120),(80,60)))
x_change = 0
y_change = 0
init()

while True:
    for event in pygame.event.get():
        print event
        if event.type == QUIT:
            pygame.quit()
            sys.exit()
            
        if event.type ==  pygame.KEYDOWN:
            if event.key == pygame.K_SPACE:
	       	img = pygame.transform.rotate(img,10)
	       	left(0.1)
	       	
            if event.key == pygame.K_LEFT:
                img = pygame.transform.rotate(img,10)
                x_change = -5
                
                
            elif event.key == pygame.K_RIGHT:
                x_change = 5
                
        if event.type ==  pygame.KEYDOWN: #keydown means keypress
            if event.key == pygame.K_UP:
                y_change = -5
            elif event.key == pygame.K_DOWN:
                y_change = 5

        if event.type == pygame.KEYUP:
            if event.key == pygame.K_UP or event.key == pygame.K_DOWN:
                y_change = 0
                off(0.2)
                init()


        if event.type == pygame.KEYUP:
            if event.key == pygame.K_LEFT or event.key == pygame.K_RIGHT:
                x_change = 0
                off(0.2)
                init()

        #print x,y  
        #print(event)
    x += x_change
    y += y_change
    #gameDisplay.fill(white)
    setDisplay.blit(img,(x,y))
    clock.tick(60)
    #imagen(x,y) # SET THE IMAGE
    pygame.display.update()    