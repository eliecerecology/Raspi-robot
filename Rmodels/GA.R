rm(list=ls(all=TRUE))
genome = matrix(round(runif(16, 1, 6)), 4, 2)
mean(genome[,1])
performance = matrix(round(runif(16, 1, 4)), 4, 2)
fitness = matrix(1, 5, 2)
#a = c(3, 6, 3 , 1) # environment
#b = c(5, 2, 4, 3) # environment, a and b resemble 3 neighbor cell in cellulat automata
#R <- c(2, 0, 1) # reward
p <<- as.numeric()
p1 <<- as.numeric()
p2 <<- as.numeric()
t = list()
gen <- function() {
  for (j in 1:2){}
    for (i in 1:length(a)){
      if (genome[j, i] >  mean(genome[ ,i ]) {fitness[j ,i] = performance[j, i]} else {fitness[j ,i] = 0}
      if (genome[j, i] == mean(genome[ ,i ]) {fitness[j ,i] = performance[j, i]} else {fitness[j ,i] = 0}
      if (genome[j, i] <  mean(genome[ ,i ]) {fitness[j ,i] = performance[j, i]} else {fitness[j ,i] = 0}
      t[[i]] <<- fitness[1] + fitness[i] + fitness[i] + fitness[]
    }
      if (t[[1]] == max(t[[1]], t[[2]], t[[3]], t[[4]])) {print(which.max(t))} else {R <<- sample(R)} & {do.call(gen1, list())}
}
which.max(a)
gen1 <- function() {
  for (i in 1:length(a)){
    if (a[i] > b[i]) {p[i] = R[1]} else {p[i] = 0}
    if (a[i] == b[i]) {p1[i] = R[2]} else {p1[i] = 0}
    if (a[i] < b[i]) {p2[i] = R[3]} else {p2[i] = 0}
    t[[i]] <<- p[i] + p1[i] + p2[i]
  }
  which.max(t)
  if (t[[1]] == max(t[[1]], t[[2]], t[[3]], t[[4]])) {print(which.max(t))} else {R <<- sample(R)} #& {do.call(gen1, list())}
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
