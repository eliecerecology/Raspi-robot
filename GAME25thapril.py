import pygame

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

#so where to display or location

def robot(x,y):
    gameDisplay.blit(robImg,(x,y))


def game_loop():
    x = (display_width *0.45)
    y = (display_height *0.40)

    x_change = 0

    crashed = False

    while not crashed:

        for event in pygame.event.get(): 
            if event.type == pygame.QUIT:
                crashed = True

             #aca empieza el control
            if event.type ==  pygame.KEYDOWN:
                if event.key == pygame.K_LEFT:
                    x_change = -5
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

        pygame.display.flip() #pygame.display.flip()
        clock.tick(60) #69 frams per second
game_loop()
pygame.quit()
quit()
