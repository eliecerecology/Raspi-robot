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


pygame.init() # initiates the game
screen = pygame.display.set_mode((680,384),0,32) # screen settings
clock = pygame.time.Clock()
speed = 60 # puts and upper boandary for the speed of the game

blue = 0,0,255

protagonist0=pygame.transform.scale(pygame.image.load('Robot2.png').convert_alpha(),(25,25)) # load, resize and name protagonist image

x,y = 340,192
newx,newy = 0,0
angle = 90 # initial angle, if 90 protagonist is facing upward
newangle = 0
turn_speed = 0.5 # the amount of degrees to rotate when pressing left or right key
walk_speed = 2 # the speed in which the protagonist moves over the map

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

while True:
        for event in pygame.event.get():
                keystate = pygame.key.get_pressed() # enbles sensing multiple key strokes
                if event.type == QUIT: # enables exiting the game
                        pygame.quit()
                        sys.exit()
                elif event.type == KEYDOWN: # all keydowns in this conditional statement
                        if keystate[K_ESCAPE]:
                                pygame.quit()
                                sys.exit()
                        elif keystate[K_UP] and keystate[K_LEFT]:
                                newx = +math.cos(math.radians(angle))*walk_speed
                                newy = -math.sin(math.radians(angle))*walk_speed
                                forward(0.3)                          
                                newangle = +turn_speed
                                

                        elif keystate[K_DOWN] and keystate[K_LEFT]:
                                newx = -math.cos(math.radians(angle))*walk_speed
                                newy = +math.sin(math.radians(angle))*walk_speed
                                newangle = +turn_speed
                                forward(0.3)

                        elif keystate[K_UP] and keystate[K_RIGHT]:
                                newx = +math.cos(math.radians(angle))*walk_speed
                                newy = -math.sin(math.radians(angle))*walk_speed
                                newangle = -turn_speed
                                forward(0.3)
                        elif keystate[K_DOWN] and keystate[K_RIGHT]:
                                newx = -math.cos(math.radians(angle))*walk_speed
                                newy = +math.sin(math.radians(angle))*walk_speed
                                newangle = -turn_speed
                                forward(0.3)
                        elif keystate[K_RIGHT]:
                                newangle = -turn_speed
                        elif keystate[K_LEFT]:
                                newangle = +turn_speed
                        elif keystate[K_UP]:
                                newx = +math.cos(math.radians(angle))*walk_speed
                                newy = -math.sin(math.radians(angle))*walk_speed
                        elif keystate[K_DOWN]:
                                newx = -math.cos(math.radians(angle))*walk_speed
                                newy = +math.sin(math.radians(angle))*walk_speed
                elif event.type == KEYUP: # all keyups in this conditional statement
                        if event.key == K_LEFT:
                                newangle = 0
                        elif event.key == K_RIGHT:
                                newangle = 0
                        elif event.key == K_UP:
                                newx = 0
                                newy = 0
                        elif event.key == K_DOWN:
                                newx = 0
                                newy = 0
        x += newx # enables moving the protagonist
        y += newy # enables moving the protagonist
        angle += newangle # enables rotation of protagonist
        screen.fill(blue) # sets the background color
        protagonist1 = pygame.transform.rotate(protagonist0,angle-90) # rotates the protagonist image
        screen.blit(protagonist1,(x,y)) # insert the image of the protagonist at position x,y
        clock.tick(speed) # limits the speed of the game, where speed is a variable set outside of the loop
        pygame.display.flip() # prints/updates the game
        pygame.display.update() # makes the game run smoothly

