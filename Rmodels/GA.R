rm(list=ls(all=TRUE))
a = c(3, 6, 3 , 1)
b = c(5, 2, 4, 3)
R <- c(2, 0, 1) # reward
p <<- as.numeric()
p1 <<- as.numeric()
p2 <<- as.numeric()
t = list()
gen <- function() {
    for (i in 1:length(a)){
      if (a[i] > b[i]) {p[i] = R[1]} else {p[i] = 0}
      if (a[i] == b[i]) {p1[i] = R[2]} else {p1[i] = 0}
      if (a[i] < b[i]) {p2[i] = R[3]} else {p2[i] = 0}
      t[[i]] <<- p[i] + p1[i] +p2[i]
    }
      if (t[[1]] == max(t[[1]], t[[2]], t[[3]], t[[4]])) {print("success")} else {R <<- sample(R)} & {do.call(gen1, list())}
}

gen1 <- function() {
  for (i in 1:length(a)){
    if (a[i] > b[i]) {p[i] = R[1]} else {p[i] = 0}
    if (a[i] == b[i]) {p1[i] = R[2]} else {p1[i] = 0}
    if (a[i] < b[i]) {p2[i] = R[3]} else {p2[i] = 0}
    t[[i]] <<- p[i] + p1[i] +p2[i]
  }
  if (t[[1]] == max(t[[1]], t[[2]], t[[3]], t[[4]])) {print("success")} else {R <<- sample(R)} #& {do.call(gen1, list())}
}

gen()
R
t[[5]]#, t[[2]], t[[3]]


do.call(sum, t)
max(t[[1]], t[[2]], t[[3]])


sample(a)
do.call(sum, t)
max(t[[1]], t[[2]], t[[3]])






t = list(a=4, b = 9, c = 10)
#which(t,  max(t))
lapply(t, function(x) max(x)(x))
which.max(a)
which(a == max(a))
for (i in 1: length(a)) {
    
    
}

[which.max(abs(t))]
