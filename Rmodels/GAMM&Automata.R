for (l in 1:2){
  for (k in 1:span){
    for (t in 1: (row-2)){
      for (i in 1:(col-2)){
        #l = 1; k = 1; t = 1; i = 1
        if (j[[1]][t + 1, i + 1] > (sum(j[[1]][t: (2 + t) , i : (2 + i)]) - j[[1]][t + 1, i + 1])/8){
          p <- j[[1]][t + 1, i + 1]/ 8
          h_out[[l]][[k]][t: (2 + t) , i : (2 + i)]  <- p # reparticion
          h_out[[l]][[k]][t + 1, i + 1] <- 0 # reparticion leaving out center
        } else { if (j[[1]][t + 1, i + 1] < (sum(j[[1]][t: (2 + t) , i : (2 + i)]) - j[[1]][t + 1, i + 1])/8 ){
          h_1[[k]][t, i] <- (sum(j[[1]][t: (2 + t) , i : (2 + i)]) - j[[1]][t + 1, i + 1]) / 8 #average outside  
          p <- h_1[[k]][t, i] # average outside
          h_out[[l]][[k]][t + 1, i + 1] <- p # reparticion leaving out center
        }
        }  
        h_out                    
      }
    }
  }
}  

for (i in 1:9) for (j in 1:9) {
  print(paste(i, j, sep = " "))#; print(j)
}


.list <- list(matrix(1, 2,2 ), matrix(2, 2,2))

Reduce('+', .list)
##       [,1] [,2] [,3] [,4] [,5]
## [1,]    2   12   22   32   42
## [2,]    4   14   24   34   44
## [3,]    6   16   26   36   46
## [4,]    8   18   28   38   48
## [5,]   10   20   30   40   50


p_23 <-list()
for(i in 1:5)
  p_23[[i]] = matrix(0,2,2)
p_23














