#EUler method and plotting in R

rm(list=ls(all=TRUE))
T = 5 # initial T
delta  = list(rep(1,4), rep(2,4), rep(3,4)) # at 1 steps, 2 steps and 3 steps
der = list(vector(), vector(), vector())
step = list(vector(),vector(), vector())
step[[1]][1] = T #initial T
step[[2]][1] = T
step[[3]][1] = T

for (j in 1:3){
   for (i in 1:5) {     
      der[[j]][i] = 0.2 * (20 - step[[j]][i])
      step[[j]][i+1] = step[[j]][i] + (der[[j]][i] * delta[[j]][i])

   }
}


plot(seq(1,5,1), step[[1]][1:5], type="l")
lines(seq(1,5,1), step[[2]][1:5], type="l", col="red")
lines(seq(1,5,1), step[[3]][1:5], type="l", col="green")


