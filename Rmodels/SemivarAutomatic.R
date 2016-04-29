rm(list=ls(all=TRUE))
setwd("C:/Users/localadmin_eliediaz/Documents/MEGA/Storage/DATA_CLIMATE")
setwd("/home/ellis/Documents/Storage/DATA_CLIMATE") #LINUX MEGA

s <- read.csv("grazerT.txt", header = T)
lag <- seq(0.25, 14.75 , 0.25)
n = length(lag) # 59

#length(s$red)/2 #NUMBER OF LAGS Allowed to Use (half of the transect)
var1 =  s$ulva
var2 <- s$biomass

#############################Semivariogram
semivariogram <- function(var1, var2, lags, n){
    o <- sapply(1:lags, function(x) { #lista
      rep(0, n) #n
    }, simplify = FALSE) # vector size
    
    
    b = 1; semiv = 1
    for(j in 1: lags){ #number of matrix
      for(i in 1: (n)-1){ # first lag, n = 59
        o[[j]][i] <- (var1[i+j] - var1[i])^2
        b[[j]] <- sum(o[[j]][1:(n)], na.rm=TRUE)
        semiv[[j]] <- log(b[[j]]/ (2*(n-j+1)) ) # +1 because n is UNEVEN = 59, if 60 use "n-j"
      }
    }
    ploty <- plot(log(seq(1,30,1)), log(semiv), type = "o")
    return(semiv)
    return(ploty)
}
semivariogram(var1, var2, 30, 59) # calling function
    lags <-  30 
    relat <- lm(c ~ LNvecLag[1:lags])
    res <- resid(relat)
    plot(LNvecLag[1:lags], res, type = "l", col=4, xlab='Lag', ylab='residual')
    pnt <- identify(LNvecLag[1:lags], res, plot = F) # To detect points
    points(LNvecLag[1:lags][pnt], res[pnt], col = "red") # To mark points

#step 1
plot(seq(1, 10, 1), res[1: pnt[3]], type = "l")
res[1:max[res]]
pnt[3]


##########################################CROSEMIVARIOGRAM

cross_semivariogram <- function(var1, var2, permutations,
                                title) {
  lag <- seq(0.1, 6 , 0.1)
  n = length(lag) # 59
  lags <-  n/2
  o <- sapply(1:lags, function(x) { #lista
    rep(0, n) #n
  }, simplify = FALSE) # vector size
  
  b = 1; cross_s = 1
  for(j in 1: lags){ #number of matrix
    for(i in 1: (n)-1){ # first lag, n = 59
      o[[j]][i] <- (var1[i+j] - var1[i]) * (var2[i + j] - var2[i])
      b[[j]] <- sum(o[[j]][1:(n)], na.rm=TRUE)
      cross_s[[j]] <- (b[[j]]/ (2*(n-j)))   #for even n use "n-j"
      #cross_s[[j]] <- (b[[j]]/ (2*(n-j+1)) )# +1 because n is UNEVEN
    }
  }
  
  ###Permuatation- 1000 cross_semivariogram
  b = 1; permut = permutations
  c_list = sapply(1:permut, function(x) { #lista
    rep(0, lags) #n
  }, simplify = FALSE) # vector size
  
  for (k in 1:permut){
    for(j in 1: lags){ #number of matrix
      for(i in 1: (n)-1){ # first lag, n = 59
        resa_V1 <- sample(var1, replace = T)
        resa_V2 <- sample(var2, replace = T)
        o[[j]][i] <- (resa_V1[i+j] - resa_V1[i]) * (resa_V2[i + j] - resa_V2[i])
        b[[j]] <- sum(o[[j]][1:(n)], na.rm=TRUE)
        c_list[[k]][j] <- (b[[j]]/ (2*(n-j)) )  # for even n use "n-j"
        #c_list[[k]][j] <- (b[[j]]/ (2*(n-j+1)) ) # +1 because n is UNEVEN
        
      }
    }
  }
  
  pmore <- sapply(1:permut, function(x) { #lista
    rep(0, lags) #n
  }, simplify = FALSE) # vector size
  
  p_less <- vector()
  
  for (k in 1: permut){
    for(j in 1: lags){ #number of matrix
      if (cross_s[j] > c_list[[k]][j]) { pmore[[k]][j] <- 1 }
      else {pmore[[k]][j] <- 0}
    }
  }
  
  matrix <- do.call(rbind, pmore) #convert the list into a matrix
  pvalues <- vector(); which_ones <- vector()
  
  for (i in 1:lags){
    pvalues[i] <- (sum(matrix[,i])/permut)
  }
  p_less <-  (1 - pvalues)
  which_ones <- which(pvalues < 0.05)
  
  list_r <- list(P_values = pvalues, P_values_small_than_0.05 = which_ones,
                 P_val_pos_coss = p_less, cross_s_values = cross_s)
  
  
  Results <<- NULL
  Results <<- (list_r)
  plt <- plot(seq(0.1,3,0.1), Results$cross_s_values, type ="o", main=title,
              xlab = "lag", ylab = "cross-semivariance") # xlim=c(xmin, xmax), ylim=c(ymin, ymax))
  return(plt)
  
}

cross_semivariogram(var1, var2, 1000, c("TotCovROCAHIGHPNT2")) #calling function
