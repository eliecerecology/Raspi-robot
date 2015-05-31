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


display_width = 1000
display_height = 1000

black = (0,0,0) # no color
white = (255,255,255)
red = (255,0,0)

gameDisplay = pygame.display.set_mode((display_width, display_height))
pygame.display.set_caption('SuavisGame')
clock = pygame.time.Clock()
robImg = pygame.image.load('Robot2.png')

#def rotate(a):
   # where = 200,200
   # blitrobot= screen.blit(surf, where)
   # surf =  pygame.Surface((100, 100))
   # surf.fill((255, 255, 255))
 #   robImg = pygame.transform.rotate(robImg, a)
    
#colors
white = (255,255,255)
black = (0,0,0)
red =(255,0,0)
green = (0,255,0)
#blue = (0,0,255)

#so where to display or location

def robot(a,x,y):
    robImg1 = pygame.transform.rotate(robImg,a)
    gameDisplay.blit(robImg1,(x,y))

def game_loop():
    init()
    sleep_time = 1
       
    x = (display_width / 4) #0.45
    y = (display_height / 4) # 0.40

    x_change = 0
    y_change = 0
    crashed = False
    a = 0
    a_change = 0

    while not crashed:
        
        for event in pygame.event.get(): 
            if event.type == pygame.QUIT:
                crashed = True

             #aca empieza el control
            if event.type ==  pygame.KEYDOWN:
                if event.key == pygame.K_LEFT:
                    #x_change = -5
                    a_change = 20
                    x_change = -20

                if event.key == pygame.K_RIGHT:  
                    a_change = 0
                    x_change = 5
                    right(0.3)
                    forward(0.3)
                    
                if event.key == pygame.K_UP:
                    a_change = 0
                    y_change = -5
                    forward(0.3)
    
                elif event.key == pygame.K_DOWN:
                    a_change = 0
                    y_change = 5
                    reverse(0.3)

            if event.type == pygame.KEYUP:
                if event.key == pygame.K_LEFT or event.key == pygame.K_RIGHT:
                    a_change = 0
                    x_change = 0
                    stop(0.2)
                    init()
                elif event.key == pygame.K_UP or event.key == pygame.K_DOWN:
                    a_change = 0
                    y_change = 0
                    stop(0.2)
                    init()      
        
        #    print(event)
        x += x_change
        y += y_change      
        a += a_change
        gameDisplay.fill(white)    
        #rotate(a)
        robot(a, x, y) #position of the car
    
        #robImg = pygame.transform.rotate(robImg, 90)
        #pygame.draw.rect(gameDisplay, black,(display_width*0,display_height*0,50,50))
        #####pygame.draw.rect(gameDisplay, green,(display_width*0.5,display_height*0.5,50,50))
        pygame.display.flip() #pygame.display.flip()
        clock.tick(60) #69 frams per second
game_loop()
pygame.quit()
quit()
