rm(list=ls(all=TRUE))
a = c(12, 11, 10, 8)
b = c(4, 10, 8, 7)
rewardLo = as.numeric()
rewardEq = as.numeric()
rewardLa = as.numeric()

for (i in 1: length(a)){
      if (a[i] < b[i])  { (foo[[1]])} & {rewardLo$pointa[i] = 2}
      if (a[i] == b[i]) { (foo[[2]])} & {rewardEq$pointb[i] = 0} # & {do.call(j,list() ) } &
      if (a[i] > b[i])  { (foo[3])}   & {rewardLa$pointc[i] = 1} # & {do.call(j,list() ) } &  
}

foo <- c(2, 0, 1)
names(foo) <- c("adelante", "Atras", "vuelvete")
foo[[1]]
