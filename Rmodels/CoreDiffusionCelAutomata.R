rm(list=ls(all=TRUE))
#globals:
row = 5; col = 5; span = 2 #span or steps, col =columns, row = rows in the matrix

plat <- sapply(1:span, function(x) {matrix(0, nrow = row, ncol = col)}, simplify = FALSE) # rocky shore platform

plat[[1]] <- matrix(0, nrow= row, ncol =col) #initial state
plat[[1]] <- edit(j[[1]]) #if I want to check something special

base_matrix = matrix(0, row, col) #the size of the grid
#asis = asistant matrix to store the neighbors sums
asis <- rep(list(rep(list(base_matrix), col-2)), 3) # a is the matrix, col - 2= numero de matrices, and 2 = numero de lists 

for (k in 1:span){        #steps
  for (t in 1:(row -2)){  # row
    for (i in 1:(col-2)){ # column
      if (plat[[k]][t + 1, i + 1] > (sum(plat[[k]][t: (2 + t) , i : (2 + i)]) - plat[[k]][t + 1, i + 1])/8){
            p <- plat[[k]][t + 1, i + 1]/ 8 #average inside or focal PATCH
            asis[[t]][[i]][t: (2 + t) , i : (2 + i)]  <- p # propagation, inside propagates outside
            asis[[t]][[i]][t + 1, i + 1] <- 0 # leaving focal Patch empty
            
      } else if (plat[[k]][t + 1, i + 1] < (sum(plat[[k]][t: (2 + t) , i : (2 + i)]) - plat[[k]][t + 1, i + 1])/8) { 
            p <- (sum(plat[[k]][t: (2 + t) , i : (2 + i)]) - plat[[k]][t + 1, i + 1]) / 8 #average outside  
            asis[[t]][[i]][t + 1, i + 1] <- p # average outside
          }    
        }
    }
  plat[[k + 1]] <- Reduce('+', asis[[1]]) + Reduce('+', asis[[2]]) + Reduce('+', asis[[3]]) + plat[[k]] #summing matrices
  plat[[k + 1]][1:col, 1]   <- 0; plat[[k + 1]][1, 1: row]  <- 0 #cleaning the edge
  plat[[k + 1]][1:row, col] <- 0; plat[[k + 1]][row, 1:col] <- 0 #cleaning the edge
 }

Sys.sleep(0.3)


