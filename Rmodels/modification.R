install.packages("plotrix")
library(plotrix)
rm(list=ls(all=TRUE))
#install.packages("plotrix")

row = 6; col = 6; span = 3; delay = 0.1

#re = sample(seq(from = 0, to = 1, by = 1), size = row*col, replace = TRUE)
j <- sapply(1:span, function(x) {
  matrix(0, nrow = row, ncol = col) 
}, simplify = FALSE)


j[[1]] <- matrix(0, nrow= row, ncol =col)
j[[1]] <- edit(j[[1]])

#j[[1]][35, 36] <- 1; j[[1]][36, 37] <- 1; j[[1]][37, 37] <- 1; j[[1]][37, 36] <- 1; j[[1]][37, 35] <- 1

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

a <- matrix(0, nrow = row -2)
b <- matrix(0, ncol = col)
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
      h_1[[k]][t, i] <- sum(j[[k]][t: (2 + t) , i : (2 + i) ]) - j[[k]][t + 1, 1 + i] #outside
      
      
      
      ##Rule 1
      if (j[[k]][t + 1, 1 + i] == 0 & h_1[[k]][t, i] < 10) {
        h[[k]][t, i] <-0.3682*j[[k]][t + 1, 1 + i] + 4.89} 
      ##Rule 2
      else if (j[[k]][t + 1, 1 + i] == 0 | j[[k]][t + 1, 1 + i] <= 10  & h_1[[k]][t, i] > 10 & h_1[[k]][t, i] <= 20) {
        h[[k]][t, i] <- 5} 
      ##Rule 3
      else if (j[[k]][t + 1, 1 + i] == 0 | j[[k]][t + 1, 1 + i] > 0 & h_1[[k]][t, i] > 20 & h_1[[k]][t, i] < 30 ) {
        h[[k]][t, i] <- 8} 
      #4. Any dead cell with exactly three live neighbours will come to life.
      else if (j[[k]][t + 1, 1 + i] == 0 | j[[k]][t + 1, 1 + i] > 0 & h_1[[k]][t, i] > 30 ) {
        h[[k]][t, i] <- 80} 
      #else {h[[k]][t, i] <- 0}
      
    }
    print("2")
  }
  h[[k]] <- cbind(a, h[[k]], a)
  h[[k]] <- rbind(b, h[[k]], b)
  j[[k + 1]] <- h[[k]]
  print("3")
  #testit(0.3)
  #color2D.matplot(h[[k]],c(0,1),c(0,0),c(0,0),show.legend=TRUE, show.values=F)
  testit(delay)
}

h[[1]]

