rm(list=ls(all=TRUE))
#install.packages("plotrix")
library(plotrix)

row = 10; col = 10; aux = 10

j <- sapply(aux, function(x) {
  matrix(seq(1,100,1), nrow = row, ncol = col) 
}, simplify = FALSE)

re = sample(seq(from = 0, to = 1, by = 1), size = row*col, replace = TRUE)
j <- matrix(re, nrow= row, ncol =col)
j

j[1:col, 1]   <- 0 
j[1, 1: row]  <- 0
j[1:row, col] <- 0
j[row, 1:col] <- 0

j
h <- matrix(NaN, row - 2 , col - 2)

for (t in 1: (row-2)){
  for (i in 1:(col-2)){
    h[t, i] <- sum(j[t: (2 + t) , i : (2 + i) ]) - j[t + 1, 1 + i]
    if (h[t, i] >= 3){ 
      h[t, i] = 1} else {
      h[t, i] = 0}
  
  }
}

h
au=matrix(1:16, 4, 4)

colnames(h) <- c("X1", "X2", "X3", "X4")
rownames(h) <- c("Y1", "Y2", "Y3", "Y4")
color2D.matplot(h,c(0,1),c(0,0),c(0,0),show.legend=TRUE, show.values=F)


