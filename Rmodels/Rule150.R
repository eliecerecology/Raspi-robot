install.packages("plotrix")
library(plotrix)
rm(list=ls(all=TRUE))
#install.packages("plotrix")
row = 100; col = 100
trant <- matrix(0, row, col)
trant[1, ] <- edit(trant[1,])
trant[1, ]
for (j in 1: (row-1)){
  for (i in 1:(col-2)){
    if (sum(trant[j, i : (2 + i)]) == 0) {
      trant[j + 1, i + 1] <- 0 & print(1)
    } else if (sum(trant[j, i : (2 + i)]) == 1) {
      trant[j + 1, i + 1] <- 1 & print(2)
    } else if (sum(trant[j, i : (2 + i)]) == 2) {
      trant[j + 1, i + 1] <- 0 & print(3)
    } else if (sum(trant[j, i : (2 + i)]) == 3){
      trant[j + 1, i + 1] <- 1 & print(4)}
  }
}
trant
color2D.matplot(trant,c(0,1),c(0,0),c(0,0),show.legend=TRUE, show.values=F)
