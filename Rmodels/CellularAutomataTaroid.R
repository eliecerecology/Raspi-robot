library(plotrix)
#rm(list=ls(all=TRUE))
#install.packages("plotrix")

row = 60; col = 60; span = 200; delay = 0.2
#delay = 0.3
#re = sample(seq(from = 0, to = 1, by = 1), size = row*col, replace = TRUE)
j <- sapply(1:span, function(x) {
  matrix(0, nrow = row, ncol = col) 
}, simplify = FALSE)

j[[1]] <- matrix(0, nrow= row, ncol =col)
#j[[1]] <- edit(j[[1]])

j[[1]][45, 46] <- 1; j[[1]][46, 47] <- 1; j[[1]][47, 47] <- 1; j[[1]][47, 46] <- 1; j[[1]][47, 45] <- 1
#edit(j[[1]])
#Taroidal properties
j[[1]][1:col, 1]   <- 0
j[[1]][1, 1: row]  <- 0
j[[1]][1:row, col] <- 0
j[[1]][row, 1:col] <- 0

#color2D.matplot(j[[2]],c(0,1),c(0,0),c(0,0),show.legend=TRUE, show.values=F)

h <- sapply(1:span, function(x) {
  matrix(0, nrow = row-2, ncol = col-2) 
}, simplify = FALSE)

h_1 <- sapply(1:span, function(x) {
  matrix(0, nrow = row-2, ncol = col-2) 
}, simplify = FALSE)

#a <- matrix(0, nrow = row -2)
#b <- matrix(0, ncol = col)
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

#h <- matrix(NaN, row - 2 , col - 2)
for (k in 1:span){
  for (t in 1: (row-2)){
    for (i in 1:(col-2)){
      h_1[[k]][t, i] <- sum(j[[k]][t: (2 + t) , i : (2 + i) ] == 1) - j[[k]][t + 1, 1 + i] #sum(j[[k]][t + 1, 1 + i] == 1) 
      ##1. Any live cell with fewer than two live neighbours dies, as if by needs caused by underpopulation.
      if (j[[k]][t + 1, 1 + i] == 1 & h_1[[k]][t, i] < 2) {
        h[[k]][t, i] <- 0} 
      ##2. Any live cell with more than three live neighbours dies, as if by overcrowding.
      else if (j[[k]][t + 1, 1 + i] == 1 & h_1[[k]][t, i] > 3) {
        h[[k]][t, i] <- 0} 
      ##3. Any live cell with two or three live neighbours lives, unchanged, to the next generation.
      else if (j[[k]][t + 1, 1 + i] == 1 & h_1[[k]][t, i] == 2 | h_1[[k]][t, i] == 3 ) {
        h[[k]][t, i] <- 1} 
      #4. Any dead cell with exactly three live neighbours will come to life.
      else if (j[[k]][t + 1, 1 + i] == 0 & h_1[[k]][t, i] == 3 ) {
        h[[k]][t, i] <- 1} 
      #else {h[[k]][t, i] <- 0}
      
    }
    print("2")
  }
  h[[k]] <- cbind(h[[k]][1: (row - 2), (col - 2)],    h[[k]],      h[[k]][1: (row - 2), 1])
  h[[k]] <- rbind(h[[k]][(col - 2), 1: col], h[[k]], h[[k]][1, 1: col])
  
  corner1 <- h[[k]][1, 1] 
  corner2 <- h[[k]][nrow(h[[k]]), ncol(h[[k]])] 
  corner3 <- h[[k]][1, ncol(h[[k]])]
  corner4 <- h[[k]][nrow(h[[k]]), 1]
  
  h[[k]][1, 1] <- corner2
  h[[k]][nrow(h[[k]]), ncol(h[[k]])] <- corner1
  h[[k]][1, ncol(h[[k]])] <- corner4
  h[[k]][nrow(h[[k]]), 1] <- corner3
  
  j[[k + 1]] <- h[[k]]
  print("3")
  #testit(0.3)
  color2D.matplot(h[[k]],c(0,1),c(0,0),c(0,0),show.legend=TRUE, show.values=F)
  testit(delay)
}

j[[1]][1:10, 1:10]
j[[3]]
cat("\014")

#to check
#eeeeeeee
h[[1]][1: (row - 2), (col - 2)] <- 58 #not necessary
h[[1]][1: (row - 2), 1] <- 1 #not necessary
h[[1]] <- cbind(h[[1]][1: (row - 2), (col - 2)],    h[[1]],      h[[1]][1: (row - 2), 1])

b1 <- matrix(h[[k]][nrow(h[[k]]), ncol(h[[k]]) - 1])

b2 <- matrix(h[[k]][ nrow(h[[k]]) , 2])

h[[1]][(col - 2), 1: col] <- 59 #not necessary
h[[1]][1, 1: col] <- 2          # not necessary

h[[1]] <- rbind(h[[1]][(col - 2), 1: col], h[[1]], h[[1]][1, 1: col])

corner1 <- h[[k]][1, 1] 
corner2 <- h[[k]][nrow(h[[k]]), ncol(h[[k]])] 
corner3 <- h[[k]][1, ncol(h[[k]])]
corner4 <- h[[k]][nrow(h[[k]]), 1]

h[[k]][1, 1] <- corner2
h[[k]][nrow(h[[k]]), ncol(h[[k]])] <- corner1
h[[k]][1, ncol(h[[k]])] <- corner4
h[[k]][nrow(h[[k]]), 1] <- corner3

j[[k + 1]] <- h[[k]]


