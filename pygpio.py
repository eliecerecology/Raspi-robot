import pygame
import RPi.GPIO as GPIO
import time
import sys

GPIO.setmode(GPIO.BOARD)
GPIO.setup(7, GPIO.OUT)
GPIO.setup(11,GPIO.OUT)
GPIO.setup(13,GPIO.OUT)
GPIO.setup(15,GPIO.OUT)

def init():
    GPIO.setmode(GPIO.BOARD)
    GPIO.setup(7, GPIO.OUT)
    GPIO.setup(11,GPIO.OUT)
    GPIO.setup(13,GPIO.OUT)
    GPIO.setup(15,GPIO.OUT)

def forward(tf):
    GPIO.output(7,True)
    GPIO.output(15,True)
    GPIO.output(11,False)
    GPIO.output(13,False)
    time.sleep(tf)
    

def reverse(tf):
    GPIO.output(11, True)
    GPIO.output(13,True)
    GPIO.output(7,False)
    GPIO.output(15,False)
    time.sleep(tf)
    

def left(tf):
    GPIO.output(15,True)
    GPIO.output(7,False)
    GPIO.output(11,False)
    GPIO.output(13,False)
    time.sleep(tf)
    
def right(tf):
    GPIO.output(7,True)
    GPIO.output(11,False)
    GPIO.output(13,False)
    GPIO.output(15,False)
    time.sleep(tf)
    

def stop(tf):
    GPIO.output(7,False)
    GPIO.output(11,False)
    GPIO.output(13,False)
    GPIO.output(15,False)    
    GPIO.cleanup()

pygame.init()


display_width = 800
display_height = 600

black = (0,0,0) # no color
white = (255,255,255)
red = (255,0,0)

gameDisplay = pygame.display.set_mode((display_width, display_height))
pygame.display.set_caption('SuavisGame')
clock = pygame.time.Clock()

robImg = pygame.image.load('Robot2.png')

#colors
white = (255,255,255)
black = (0,0,0)
red =(255,0,0)
green = (0,255,0)
blue = (0,0,255)

#so where to display or location
def robot(x,y):
    gameDisplay.blit(robImg,(x,y))

def game_loop():
    init()
    sleep_time = 5

    x = (display_width *0.800) #0.45
    y = (display_height *0.600) # 0.40

    x_change = 0

    crashed = False

    while not crashed:

        for event in pygame.event.get(): 
            if event.type == pygame.QUIT:
                crashed = True

             #aca empieza el control
            if event.type ==  pygame.KEYDOWN:
                if event.key == pygame.K_LEFT:
                    x_change = -sleep_time
                    forward(sleep_time)
                    GPIO.cleanup()
                elif event.key == pygame.K_RIGHT:
                    x_change = 5

            if event.type == pygame.KEYUP:
                if event.key == pygame.K_LEFT or event.key == pygame.K_RIGHT:
                    x_change = 0
            '''             
            if event.type ==  pygame.KEYDOWN:
                if event.key == pygame.K_UP:
                    x_change = -5
                elif event.key == pygame.K_DOWN:
                    x_change = 5
             '''
                   
        #    print(event)
        x += x_change      
        gameDisplay.fill(white)    
        robot(x,y) #position of the car    
        pygame.draw.rect(gameDisplay, black,(display_width*0,display_height*0,50,50))
        pygame.draw.rect(gameDisplay, green,(display_width*0.5,display_height*0.5,50,50))

        pygame.display.flip() #pygame.display.flip()
        clock.tick(60) #69 frams per second
game_loop()
pygame.quit()
quit()
