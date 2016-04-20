rm(list=ls(all=TRUE))
ngenes = 4
nrow = 4
span <- 1:10 # individuals

Pop_genes <- sapply(span, function(x) {
  matrix(round(runif(nrow*ngenes, 1, 6)), ncol = ngenes, nrow = nrow)
}, simplify = FALSE) # U = environmental noise for open control

fitness <- sapply(span, function(x) {
  matrix(round(runif(nrow*ngenes, 1, 1)), ncol = ngenes, nrow = nrow)
}, simplify = FALSE) # U = environmental noise for open control

<<<<<<< HEAD
t <- sapply(span, function(x) {
  vector()
}, simplify = FALSE) # U = environmental noise for open control
t[1]
#Pop_genes[[1]][1 ,4]

#gen <- function() {}
for (k in 1:length(span)){  
  for (j in 1:2) { #columns = genes
    for (i in 1:4) { # intrones
      if (Pop_genes[[k]][i ,j] >  mean(Pop_genes[[k]][,1])) {fitness[[k]][i ,j] = 6} 
      if (Pop_genes[[k]][i ,j] ==  mean(Pop_genes[[k]][,1])) {fitness[[k]][i ,j] = -1} 
      if (Pop_genes[[k]][i ,j] <  mean(Pop_genes[[k]][,1])) {fitness[[k]][i ,j] = -2} 
    }
    #print(genome[ ,which.max(colSums(genome))])
    t[[k]] <- sum(fitness[[k]][, j])
    #genome[,length(ncol(genome)) + 1] = genome[,which.max(ncol(genome))]
  }
  
}
t[[4]]


dim(genome)
which.max(t) - t[[4]]
which.max(colSums(genome))
genome  
genome[,length(ncol(genome)) + 1] = genome[,which.max(ncol(genome))]  
genome[,length(ncol(genome)) + 2] = c(3,3,3,3)  
  
gen1 <- function() {
  for (i in 1:length(a)){
    if (a[i] > b[i]) {p[i] = R[1]} else {p[i] = 0}
    if (a[i] == b[i]) {p1[i] = R[2]} else {p1[i] = 0}
    if (a[i] < b[i]) {p2[i] = R[3]} else {p2[i] = 0}
    t[[i]] <<- p[i] + p1[i] + p2[i]
=======
New_fitness <- sapply(span, function(x) {
  matrix(NA, ncol = ngenes, nrow = nrow)
}, simplify = FALSE) # U = environmental noise for open control

t <- sapply(span, function(x) {
  vector()
}, simplify = FALSE) # U = environmental noise for open control

generation <- matrix(NA, ncol = length(span), nrow = nrow)

#gen <- function() {}
for (k in 1:length(span)){  
  for (j in 1:ngenes) { #columns = genes
    for (i in 1:nrow) { # intrones
      if (Pop_genes[[k]][i ,j] >  mean(Pop_genes[[k]][,j])) {fitness[[k]][i ,j] = 6} 
      if (Pop_genes[[k]][i ,j] ==  mean(Pop_genes[[k]][,j])) {fitness[[k]][i ,j] = -1} 
      if (Pop_genes[[k]][i ,j] <  mean(Pop_genes[[k]][,j])) {fitness[[k]][i ,j] = -2} 
      t[[k]][j] <- sum(fitness[[k]][, j])
      
     
    }
    
>>>>>>> c303938666a0602de0fa076fa6c8ad2b093751f3
  }
  print(which.max(t[[k]]))  
  generation[, k] = Pop_genes[[k]] [, which.max(t[[k]] )]

} #End selection of best genes


generation
which.max(t[[5]])
which.max(colSums(genome))
  
#gen1 <- function() {}
for (k in 1:length(span)){  
    for (j in 1:ngenes) { #columns = genes
      for (i in 1:nrow) { # intrones
          if (generation[i, j] > mean(generation[, j)]) {New_fitness[[k]][i ,j] = 6}
          if (generation[i, j] == mean(generation[, j)]) {New_fitness[[k]][i ,j] = -2 + runif(1,1,6)} 
          if (generation[i, j] < mean(generation[, j)]) {New_fitness[[k]][i ,j] = 6 }
          t[[k]][j] <- sum(fitness[[k]][, j])
        }
        
      }

gen()
t
t[[5]]#, t[[2]], t[[3]]


do.call(sum, t)
max(t[[1]], t[[2]], t[[3]])


sample(a)
do.call(sum, t)
max(t[[1]], t[[2]], t[[3]])

jojo = list(sa=21, se = 22, si =34)
(which.max(jojo))



t = list(a=4, b = 9, c = 10)
#which(t,  max(t))
lapply(t, function(x) max(x)(x))
which.max(a)
which(a == max(a))
for (i in 1: length(a)) {
    
    
}

[which.max(abs(t))]
