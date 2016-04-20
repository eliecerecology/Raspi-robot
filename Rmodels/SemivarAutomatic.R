rm(list=ls(all=TRUE))
setwd("C:/Users/localadmin_eliediaz/Documents/MEGA/Storage/DATA_CLIMATE")

s <- read.table("grazerT.txt", header = T)
edit(s)
lag <- seq(0.25, 14.75 , 0.25)
n = length(lag) # 59
LNvecLag = log(lag)

lags =  30 #length(s$red)/2 #NUMBER OF LAGS
A = s$red
o <- sapply(1:lags, function(x) {
  rep(0, n)
}, simplify = FALSE) # List of lags
b = 1; c = 1
for(j in 1: lags){ #number of matrix
  for(i in 1: (n)-1){ # first lag
    o[[j]][i] <- (A[i+j] - A[i])^2
    b[[j]] <- sum(o[[j]][1:(n)], na.rm=TRUE)
    c[[j]] <- log(b[[j]]/ (2*(n-j+1)) ) # +1 because n is UNEVEN = 59, if 60 use "n-j"
  }
}

relat = lm(c ~ LNvecLag[1:lags])
res = resid(relat)
plot(LNvecLag[1:lags], res, type = "l", col=4, xlab='Lag', ylab='residual')
pnt <- identify(LNvecLag[1:lags], res, plot = F) # To detect points
points(LNvecLag[1:lags][pnt], res[pnt], col = "red") # To mark points

#step 1
plot(seq(1, 10, 1), res[1: pnt[3]], type = "l")
res[1:max[res]]
pnt[3]
