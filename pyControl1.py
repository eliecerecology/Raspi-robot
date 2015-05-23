import pygame
import time
import random
import RPi.GPIO as gpio

#Associate pin numbers with direction
rightForward  = 11
rightBackward = 13
leftForward   = 15
leftBackward  = 16

gpio.setmode(gpio.BOARD)
gpio.setup(7, True)
gpio.setup(11, True)
gpio.setup(13, True)
gpio.setup(15, True)

def init():
    #setup GPIO using board numbering
    gpio.setmode(gpio.BOARD)
    gpio.setup(7, True)
    gpio.setup(11, True)
    gpio.setup(13, True)
    gpio.setup(15, True)

def forward():
    init()
    gpio.output(7, True)
    gpio.output(15, True)

def backward():
    init()
    gpio.output(11, True)
    gpio.output(13, True)
    
def left():
    init()
    gpio.output(7, True)
    

def right():
    init()
    gpio.output(15, True)
    
def stop():
    init()
    gpio.output(7, False)
    gpio.output(11, False)
    gpio.output(13, False)
    gpio.output(15, False)

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
robot_width = 50
robot_height= 50
#so where to display or location

def robot(x,y):
    gameDisplay.blit(robImg,(x,y))

def game_intro():
    intro = True

    while intro:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
               pygame.quit()
               quit()

        gameDisplay.fill(white)
        largeText = pygame.font.Font('freesansbold.ttf',60)
        TextSurf, TextRect = text_objects("Welcome to FionaGame", largeText)
        TextRect.center = (display_width/2,display_height/2)
        gameDisplay.blit(TextSurf,TextRect)
        pygame.display.update()
        clock.tick(1)
        game_loop()
def things(thingx, thingy, thingw, thingh, color):
    #defin rectangule
    pygame.draw.rect(gameDisplay, color, [thingx,thingy,thingw,thingh])

def things_dodged(count): #score system
    font = pygame.font.SysFont(None,25)
    text = font.render("Dodged: "+str(count), True,black)
    gameDisplay.blit(text,(0,0)) # what and where

def text_objects(text,font):
    textSurface = font.render(text,True,black)
    return textSurface, textSurface.get_rect()

def message_display(text):
    largeText = pygame.font.Font('freesansbold.ttf',90)
    TextSurf, TextRect = text_objects(text, largeText)
    TextRect.center = (display_width/2,display_height/2)
    gameDisplay.blit(TextSurf,TextRect)

    pygame.display.update()

    time.sleep(2)

    game_loop()


def crash():
    message_display('GAME OVER')
    
def game_loop():
    x = (display_width  *  0.45)
    y = (display_height * 0.40)

    x_change = 0
    y_change = 0
    #position of the object thing
    thing_startx = random.randrange(0, display_width)
    thing_starty = random.randrange(0, display_height)#-600
    thing_speed = 2 #7 pixels
    thing_width =  50
    thing_height = 50                  


    dodged = 0
    gameExit = False

    while not gameExit:

        for event in pygame.event.get(): 
            if event.type == pygame.QUIT:
                pygame.quit()
                quit()

            #aca empieza el control
                
            if event.type ==  pygame.KEYDOWN: #keydown means keypress
                if event.key == pygame.K_UP:
                    forward()
                    y_change = -5
                elif event.key == pygame.K_DOWN:
                    backward()
                    y_change = 5

            if event.type == pygame.KEYUP:
                if event.key == pygame.K_UP or event.key == pygame.K_DOWN:
                    y_change = 0


            if event.type ==  pygame.KEYDOWN:
                if event.key == pygame.K_LEFT:
                    x_change = -5
                elif event.key == pygame.K_RIGHT:
                    x_change = 5

            if event.type == pygame.KEYUP:
                if event.key == pygame.K_LEFT or event.key == pygame.K_RIGHT:
                    x_change = 0

            #print x,y  
            #print(event)
        x += x_change
        y += y_change
        gameDisplay.fill(white)

        #drawing boxes:
        #things(thingx, thingy, thingw, thingh, color)
        things(thing_startx, thing_starty, thing_width, thing_height, black) #thing_startx =position
        thing_starty += thing_speed#random.randrange(7) # everytime we run the loop we add 7 pix
        #thing_startx += random.randrange(thing_speed
        
        robot(x,y) #position of the car    
        things_dodged(dodged)

        #boundary
        
        if x >= display_width - robot_width or x < 0:
            crash()

        #create random generations of blocks
        if thing_starty > display_height: #of the screen
            thing_starty = 0 - thing_height
            thing_startx = random.randrange(0,display_width)
            dodged += 1
            #making difficult the gamr
            thing_speed +=1
            thing_width += (dodged * 1.2)
            
        # make them to crash
        if y < thing_starty +  thing_height:
            print 'step1'
            if x > thing_startx and x < thing_startx + thing_width or x + robot_width > thing_startx and x + robot_width < thing_startx + thing_width:
                print ("x crossover")
                crash()
             

        
        pygame.display.update() #pygame.display.flip()
        clock.tick(60) #69 frams per second
game_intro()
game_loop()
pygame.quit()
quit()
