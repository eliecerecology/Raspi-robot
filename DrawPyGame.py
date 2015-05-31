import pygame

pygame.init()

white = (255,255,255)
black = (0,0,0)
red =(255,0,0)
green = (0,255,0)
blue = (0,0,255)

disp_width = 1000
disp_height = 1000

gameDisplay = pygame.display.set_mode((disp_width,disp_height))
gameDisplay.fill(black)
y = [1,2,3] #dist = []
x = [5,4,6] #dist = []

#for i in range(2):
#    y.append(i)
 #   print y

#A DOT
array = pygame.PixelArray(gameDisplay)
array[100][100] = red
#A line
pygame.draw.line(gameDisplay, blue, (y[1],x[1]),(200,100),5)
#A rectangle
pygame.draw.rect(gameDisplay, green,(disp_width/2,disp_height/2,50,50))
#a circle
while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            quit()

    pygame.display.update()    


 #   y.append(distance('cm'))
    
