rm(list=ls(all=TRUE))
#install.packages("plotrix")
library(plotrix)

row = 10; col = 10; span = 25

re = sample(seq(from = 0, to = 1, by = 1), size = row*col, replace = TRUE)
j <- matrix(re, nrow= row, ncol =col)

j[1:col, 1]   <- 0
j[1, 1: row]  <- 0
j[1:row, col] <- 0
j[row, 1:col] <- 0

j <- sapply(1:span, function(x) {
  matrix(j, nrow = row, ncol = col) 
}, simplify = FALSE)

h <- sapply(1:span, function(x) {
  matrix(0, nrow = row-2, ncol = col-2) 
}, simplify = FALSE)

a <- matrix(0, nrow = row -2)
b <- matrix(0, ncol = col)

#h <- matrix(NaN, row - 2 , col - 2)
for (k in 1:span){
  for (t in 1: (row-2)){
    for (i in 1:(col-2)){
      h[[k]][t, i] <- sum(j[[k]][t: (2 + t) , i : (2 + i) ]) - j[[k]][t + 1, 1 + i]
      if (h[[k]][t, i] >= 3){ 
        h[[k]][t, i] = 1} else {
        h[[k]][t, i] = 0}
        print("1")
    }
    print("2")
  }
  h[[k]] <- cbind(a, h[[k]], a)
  h[[k]] <- rbind(b, h[[k]], b)
  j[[k + 1]] <- h[[k]]
  print("3")
  color2D.matplot(h[[k]],c(0,1),c(0,0),c(0,0),show.legend=TRUE, show.values=F)
  
}

k = 3; 
h[[k]] <- cbind(a, h[[k]], a)
h[[k]] <- rbind(b, h[[k]], b)
j[[k + 1]] <- h[[k]]
h[[2]]

a <- h[[1]] 
j[1:col, 1]   <- 0
j[1, 1: row]  <- 0
j[1:row, col] <- 0
j[row, 1:col] <- 0





colnames(h) <- c("X1", "X2", "X3", "X4")
rownames(h) <- c("Y1", "Y2", "Y3", "Y4")
color2D.matplot(h[[k]],c(0,1),c(0,0),c(0,0),show.legend=TRUE, show.values=F)



