rm(list=ls(all=TRUE)) 

library(fractaldim); library(RandomFields)
library(gstat); library(lattice); library(fields); library(sp)
library(geoR); require(gridExtra)
#setwd("/home/eliecer/MEGA/Storage") # Linux
#setwd("C:/Users/localadmin_eliediaz/Dropbox/DATA_CLIMATE")
setwd("C:/Users/localadmin_eliediaz/Documents/MEGA/Storage/DATA_CLIMATE")
loc1 = setwd("C:/Users/localadmin_eliediaz/Documents/MEGA/")
#1...FUNCTION SIMULATION NATURAL

simulation_Nat = function(replicas){
  #setwd("/home/eliecer/MEGA/Storage/DATA_CLIMATE") # Linux
  setwd("C:/Users/localadmin_eliediaz/Documents/MEGA/Storage/DATA_CLIMATE")
  
  NatWeather <- read.table(file = "Rtemp.txt", 
                           header = TRUE, 
                           dec = ".")
  Result_Nat = list()
  #pb <- txtProgressBar(min = 0, max = replicas, style = 3)
  for (k in 1:replicas){ #initiate meta-loop
    #now meta-loop START HERE!   
    ncol = 60; nrow = 60; span = 364 #days
    aux <- 1:span #how many matrices
    r = 0.7; K = 100 
    rEnc = 0.7 ;rOpen = 0.3
    
    #weather <- 19 + 15 * cos(0.02 * (1 : span)) * runif(span) #5 - 33 degrees celcius #old one
    weather <- runif(364, min(6), max(35)) # random
    #weather <- NatWeather$temp # natural Mystic # data starting dec
    
    xy <- expand.grid(1:ncol, 1:nrow) # 1:ncol, 1:span # creates a  grid of coordinates
    names(xy) <- c('x','y') # coordinates, x = columns, y = rows # gives the names 
    
    g.dummy <- gstat(formula=z~ 1, locations=~x+y,
                     dummy=T, beta=c(1,0.01,0.005), model=vgm(psill=5, 
                                                              range=15, model='Exp'), nmax=60)
    
    yy <- predict(g.dummy, newdata=xy, nsim=1) #random
    #yy$sim1 = yy$sim1 + max(yy$sim1)  # calibrating topography 2 -5 cm
    #yy$sim1 = yy$sim1 + max(yy$sim1) + rpois(3600, 70.3) # calibrating SUPERnatural 50-100
    
    yy$sim1 = (runif(3600, min(50), max(101))) # RANDOM topography
    
    topo <- sapply(aux, function(x) {
      matrix(yy$sim1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # TOPOGRAPHY
    
    
    U <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # U = environmental noise
    
    Uopen <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # U = environmental noise for open control
    
    #Ua <- sapply(aux, function(x) {
    #  matrix(1, ncol = ncol, nrow = nrow)
    #}, simplify = FALSE) # envorinmental noise filling after calculation
    
    #Ug <- sapply(aux, function(x) { #for open exclosures
    #  matrix(1, ncol = ncol, nrow = nrow)
    #}, simplify = FALSE) # envorinmental noise filling after calculation
    
    Y = matrix(0, nrow, ncol) #Tidal matrix
    
    Y[1:20, 1:60] = 10 + rnorm(1, 2, 3)
    Y[20:60, 4:60] = 10 + rnorm(1, 2, 3)
    Y[20:40, 1:40] = 5 + rnorm(1, 1, 3)
    Y[40:60, 20:40] = 5 + rnorm(1, 1, 3)
    Y[40:60, 1:20] = -5 + rnorm(1, 1, 2)
    
    t_extra = 10 + rnorm(length(weather), 1, 0.5) # extra stress on open cages
    
    #LOOP 1: 
    for (i in 1:length(weather)){
      U[[i]] = topo[[i]] * weather[i] * rEnc / 1000  #+ Y
      Uopen[[i]] = (topo[[i]] * weather[i] * rOpen + t_extra[i]) / 1000 
      #U[[i]] = topo[[i]] * weather[i] #+ Y
      #Uopen[[i]] = topo[[i]] * weather[i] + t_extra[i] 
      #Ua[[i]] = -0.26 * U[[i]]^2     + 10.23*U[[i]]
      #Ug[[i]] = -0.26 * Uopen[[i]]^2 + 10.23*Uopen[[i]]
      #this function optimize to 17 degrees the maximum growth rate
      #and reduce it near 0 and 40
    } 
    
    ##LOOP 2: to remove negative values
    #for (j in 1:span){ #THIS will fill noise matrix with extra grazing
    #  for (i in 1:(nrow * ncol)){
    #    if (Ug[[j]][i] < 0){
    #      Ug[[j]][i] = 0
    #    } else {
    #      if (Ug[[j]][i] > 40){ # 40
    #        Ug[[j]][i] = 0
    #      }   
    #    }
    #  }
    #}
    
    #for (j in 1:span){ # noise matrix without grazing
    #  for (i in 1:(nrow * ncol)){
    #    if (Ua[[j]][i] < 0){
    #      Ua[[j]][i] = 0
    #    } else {
    #      if (Ua[[j]][i] > 40){ # 40
    #        Ua[[j]][i] = 0
    #      }   
    #    }
    #  }
    #}
    
    X <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # biological matrix = Ulva
    
    
    XOpen <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # biological matrix = Open Control
    
    Z <- sapply(aux, function(x) {
      matrix(runif(ncol*nrow,0,2), ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # biological
    
    vecTopo1 = matrix(yy$sim1, ncol, nrow) # Transf TOPOGRAPHY into 1 matrix
    X[[1]] =     round(abs(vecTopo1 + rnorm(ncol*nrow, 0, 2))) # adding individuals to the starting point time zero
    XOpen[[1]] = round(abs(vecTopo1 + rnorm(ncol*nrow, 0, 2))) # adding individuals to the starting point
    
    pb <- txtProgressBar(min = 0, max = (span - 1), style = 3)
    for (t in 1: (span - 1)){ # number of matrices
      for (i in 1: (ncol * nrow)){
        X[[t + 1]][i] = X[[t]][i] * exp(U[[t]][[i]] * (1 - (X[[t]][i] / K))) + Z[[t]][i] #solo Ulva
        XOpen[[t + 1]][i] = XOpen[[t]][i] * exp(Uopen[[t]][[i]] * (1 - (XOpen[[t]][i] / K))) + Z[[t]][i] 
        #X[[t + 1]][i] = X[[t]][i] * exp(rEnc * (Ua[[t]][[i]] / 26.7) * (1 - (X[[t]][i] / K))) + Z[[t]][i] #solo Ulva
        #XOpen[[t + 1]][i] = XOpen[[t]][i] * exp(rOpen * (Ug[[t]][[i]] / 26.7) * (1 - (XOpen[[t]][i] / K))) + Z[[t]][i]        
        } # 
      setTxtProgressBar(pb, t)
    }
    
    XER <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # Store ln(C/E), effect ratios
    
    #for (t in 1: (span)){ # number of matrices
    #  for (i in 1: (ncol * nrow)){
    #    XER[[t]][i] = log(((0.1 + XOpen[[t]][i]) / (0.1 + X[[t]][i])))
    #  }
    #}
    
    #Antes de convertirla a vector WARNING "XER" or "X"
    FXOpen = list()
    FXx = list() #encloures
    FX = list()############### calculattion of fractal dimension of XER
    Var_XER = list(); Var_X = list();Var_XOpen = list(); mean_XER = list()
    Mean_X = list(); Mean_X_Open = list(); Var_XOpen = list()
    DX = list(); DXOpen = list()
    #Fractal dimension estimation for Effect ratio
    for (i in 1:span){
      FXOpen[[i]] = fd.estim.isotropic(XOpen[[i]],  direction='hv', #open controls
                                       plot.loglog = F, plot.allpoints = TRUE)
      #FX[[i]] = fd.estim.isotropic(XER[[i]],  direction='hv', #XER or EFFECT RATIO
      #                             plot.loglog = F, plot.allpoints = TRUE)
      FXx[[i]] = fd.estim.isotropic(X[[i]],  direction='hv', #Xenclosures AGAIN
                                    plot.loglog = F, plot.allpoints = TRUE)
      
      DX[[i]] = FXx[[i]]$fd #, digits=2) # notice is rounded, ENCLOSURE
      DXOpen[[i]] = FXOpen[[i]]$fd
      
      #Var_XER[[i]] = sd(XER[[i]])
      #mean_XER[[i]] = mean(XER[[i]])
      Var_X[[i]] = sd(X[[i]])
      Var_XOpen[[i]] = sd(XOpen[[i]])
      Mean_X[[i]] = mean(X[[i]]) #"enclosure"
      Mean_X_Open[[i]] = mean(XOpen[[i]]) #"open"
    }
        
    #### META-LOOP!
    #Result_Nat    = cbind(DXOpen, DX, Var_XER, Var_X, mean_XER, mean_X, MeanX, Mean_X_Open)
    Result_Nat[[k]]    = cbind(DXOpen, DX, Mean_X_Open, Mean_X, Var_XOpen, Var_X) 
    
  }  #######END METALOOP!########
  
  Res_simul_Nat <<- NULL
    
  for (l in 1:replicas) {
    Res_simul_Nat[[l]] <<- data.frame(Result_Nat[[l]])
  }
  
}#######END function SIMULATION########

#2......FUNCTION SIMULATION (SIMUlATION WEATHER)

simulation = function(replicas){
  #setwd("/home/eliecer/MEGA/Storage/DATA_CLIMATE") # Linux
  setwd("C:/Users/localadmin_eliediaz/Documents/MEGA/Storage/DATA_CLIMATE")
  NatWeather <- read.table(file = "Rtemp.txt", 
                           header = TRUE, 
                           dec = ".")
  Result = list()
  for (k in 1:replicas){ #initiate meta-loop
    #not meta-loop START HERE!   
    ncol = 60; nrow = 60; span = 364 #days
    aux <- 1:span #how many matrices
    r = 0.7; K = 100 
    rEnc = 0.7 ;rOpen = 0.3
    
    weather <- 19 + 15 * cos(0.02 * (1 : span)) * runif(span) #5 - 33 degrees celcius #old one
    #weather <- runif(364, min(weather), max(weather)) # random
    
    xy <- expand.grid(1:ncol, 1:nrow) # 1:ncol, 1:span # creates a  grid of coordinates
    names(xy) <- c('x','y') # coordinates, x = columns, y = rows # gives the names 
    
    g.dummy <- gstat(formula=z~ 1, locations=~x+y,
                     dummy=T, beta=c(1,0.01,0.005), model=vgm(psill=5, 
                                                range=15, model='Exp'), nmax=60)
    
    yy <- predict(g.dummy, newdata=xy, nsim=1) #random
    yy$sim1 = yy$sim1 + max(yy$sim1) # calibrating topography 2 -5 cm
    #yy$sim1 = (runif(3600, min(yy$sim1), max(yy$sim1))) # RANDOM topography
    
    topo <- sapply(aux, function(x) {
      matrix(yy$sim1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # TOPOGRAPHY
    
    
    U <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # U = environmental noise
    
    Uopen <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # U = environmental noise for open control
    
    Ua <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # envorinmental noise filling after calculation
    
    Ug <- sapply(aux, function(x) { #for open exclosures
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # envorinmental noise filling after calculation
    
    Y = matrix(0, nrow, ncol) #Tidal matrix
    
    Y[1:20, 1:60] = 10 + rnorm(1, 2, 3)
    Y[20:60, 4:60] = 10 + rnorm(1, 2, 3)
    Y[20:40, 1:40] = 5 + rnorm(1, 1, 3)
    Y[40:60, 20:40] = 5 + rnorm(1, 1, 3)
    Y[40:60, 1:20] = -5 + rnorm(1, 1, 2)
    
    t_extra = 10 + rnorm(length(weather), 1, 0.5) # extra stress on open cages
    
    #LOOP 1: 
    for (i in 1:length(weather)){
      U[[i]] = topo[[i]] * weather[i] + Y
      Uopen[[i]] = topo[[i]] * weather[i] + t_extra[i] 
      Ua[[i]] = -0.26 * U[[i]]^2 + 10.23*U[[i]]
      Ug[[i]] = -0.26 * Uopen[[i]]^2 + 10.23*Uopen[[i]]
      #this function optimize to 17 degrees the maximum growth rate
      #and reduce it near 0 and 40
    } 
    
    #LOOP 2: to remove negative values
    for (j in 1:span){ #THIS will fill noise matrix with extra grazing
      for (i in 1:(nrow * ncol)){
        if (Ug[[j]][i] < 0){
          Ug[[j]][i] = 0
      } else {
          if (Ug[[j]][i] > 40){ # 40
            Ug[[j]][i] = 0
          }   
        }
      }
    }
    
    for (j in 1:span){ # noise matrix without grazing
      for (i in 1:(nrow * ncol)){
        if (Ua[[j]][i] < 0){
          Ua[[j]][i] = 0
        } else {
          if (Ua[[j]][i] > 40){ # 40
            Ua[[j]][i] = 0
          }   
        }
      }
    }
    
    X <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # biological matrix = Ulva
    
       
    XOpen <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # biological matrix = Open Control
    
    Z <- sapply(aux, function(x) {
      matrix(runif(ncol*nrow,0,2), ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # biological
    
    vecTopo1 = matrix(yy$sim1, ncol, nrow) # Transf TOPOGRAPHY into 1 matrix
    X[[1]] = round(abs(vecTopo1 + rnorm(ncol*nrow, 0, 2))) # adding individuals to the starting point time zero
    XOpen[[1]] = round(abs(vecTopo1 + rnorm(ncol*nrow, 0, 2))) # adding individuals to the starting point
    
    pb <- txtProgressBar(title = "modelling...!", min = 0, max = (span - 1), style = 3)
    for (t in 1: (span - 1)){ # number of matrices
      for (i in 1: (ncol * nrow)){
        X[[t + 1]][i] = X[[t]][i] * exp(rEnc * (Ua[[t]][[i]] / 26.7) * (1 - (X[[t]][i] / K))) + Z[[t]][i] #solo Ulva
        XOpen[[t + 1]][i] = XOpen[[t]][i] * exp(rOpen * (Ug[[t]][[i]] / 26.7) * (1 - (XOpen[[t]][i] / K))) + Z[[t]][i] 
      } # 
      setTxtProgressBar(pb, t)
    }
    
    XER <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
      }, simplify = FALSE) # Store ln(C/E), effect ratios
    
    #for (t in 1: (span)){ # number of matrices
    #  for (i in 1: (ncol * nrow)){
    #    XER[[t]][i] = log(((0.1 + XOpen[[t]][i]) / (0.1 + X[[t]][i])))
    #    }
    #}
    
    #Antes de convertirla a vector WARNING "XER" or "X"
    FXOpen = list()
    FXx = list() #encloures
    FX = list()############### calculattion of fractal dimension of XER
    Var_XER = list(); Var_X = list();Var_XOpen = list(); mean_XER = list()
    Mean_X = list(); Mean_X_Open = list(); Var_XOpen = list()
    DX = list(); DXOpen = list()
    #Fractal dimension estimation for Effect ratio
    for (i in 1:span){
      FXOpen[[i]] = fd.estim.isotropic(XOpen[[i]],  direction='hv', #open controls
                                       plot.loglog = F, plot.allpoints = TRUE)
      #FX[[i]] = fd.estim.isotropic(XER[[i]],  direction='hv', #XER or EFFECT RATIO
      #                             plot.loglog = F, plot.allpoints = TRUE)
      FXx[[i]] = fd.estim.isotropic(X[[i]],  direction='hv', #Xenclosures AGAIN
                                    plot.loglog = F, plot.allpoints = TRUE)
      
      DX[[i]] = FXx[[i]]$fd #, digits=2) # notice is rounded, ENCLOSURE
      DXOpen[[i]] = FXOpen[[i]]$fd
      
      #Var_XER[[i]] = sd(XER[[i]])
      #mean_XER[[i]] = mean(XER[[i]])
      Var_X[[i]] = sd(X[[i]])
      Var_XOpen[[i]] = sd(XOpen[[i]])
      Mean_X[[i]] = mean(X[[i]]) #"enclosure"
      Mean_X_Open[[i]] = mean(XOpen[[i]]) #"open"
    }
    
    #### META-LOOP!
    #Result_Nat    = cbind(DXOpen, DX, Var_XER, Var_X, mean_XER, mean_X, MeanX, Mean_X_Open)
    Result[[k]]    = cbind(DXOpen, DX, Mean_X_Open, Mean_X, Var_XOpen, Var_X) 
    #invisible(Result_Nat)  
  }  #######END METALOOP!########
  
  Res_simul_Art <<- NULL
  
  
  for (l in 1:replicas) {
    Res_simul_Art[[l]] <<- data.frame(Result[[l]])
  }
  
  Res_simul_Art <- do.call(rbind.data.frame, Res_simul_Art)
}#######END function SIMULATION########

#3. SIMULATION RANDOM..
simulation_rand = function(replicas){
  #setwd("/home/eliecer/MEGA/Storage/DATA_CLIMATE") # Linux
  setwd("C:/Users/localadmin_eliediaz/Documents/MEGA/Storage/DATA_CLIMATE")
  Result = list()
  for (k in 1:replicas){ #initiate meta-loop
    #not meta-loop START HERE!   
    ncol = 60; nrow = 60; span = 364 #days
    aux <- 1:span #how many matrices
    r = 0.7; K = 100 
    rEnc = 0.7 ;rOpen = 0.3
    
    weather <- 19 + 15 * cos(0.02 * (1 : span)) * runif(span) #5 - 33 degrees celcius #old one
    weather <- runif(364, min(weather), max(weather)) # random
    
    xy <- expand.grid(1:ncol, 1:nrow) # 1:ncol, 1:span # creates a  grid of coordinates
    names(xy) <- c('x','y') # coordinates, x = columns, y = rows # gives the names 
    
    g.dummy <- gstat(formula=z~ 1, locations=~x+y,
                     dummy=T, beta=c(1,0.01,0.005), model=vgm(psill=5, 
                                                              range=15, model='Exp'), nmax=60)
    
    yy <- predict(g.dummy, newdata=xy, nsim=1) #random
    yy$sim1 = yy$sim1 + max(yy$sim1) # calibrating topography 2 -5 cm
    yy$sim1 = (runif(3600, min(yy$sim1), max(yy$sim1))) # RANDOM topography
    
    topo <- sapply(aux, function(x) {
      matrix(yy$sim1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # TOPOGRAPHY
    
    
    U <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # U = environmental noise
    
    Uopen <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # U = environmental noise for open control
    
    Ua <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # envorinmental noise filling after calculation
    
    Ug <- sapply(aux, function(x) { #for open exclosures
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # envorinmental noise filling after calculation
    
    Y = matrix(0, nrow, ncol) #Tidal matrix
    
    Y[1:20, 1:60] = 10 + rnorm(1, 2, 3)
    Y[20:60, 4:60] = 10 + rnorm(1, 2, 3)
    Y[20:40, 1:40] = 5 + rnorm(1, 1, 3)
    Y[40:60, 20:40] = 5 + rnorm(1, 1, 3)
    Y[40:60, 1:20] = -5 + rnorm(1, 1, 2)
    
    t_extra = 10 + rnorm(length(weather), 1, 0.5) # extra stress on open cages
    
    #LOOP 1: 
    for (i in 1:length(weather)){
      U[[i]] = topo[[i]] * weather[i] + Y
      Uopen[[i]] = topo[[i]] * weather[i] + t_extra[i] 
      Ua[[i]] = -0.26 * U[[i]]^2 + 10.23*U[[i]]
      Ug[[i]] = -0.26 * Uopen[[i]]^2 + 10.23*Uopen[[i]]
      #this function optimize to 17 degrees the maximum growth rate
      #and reduce it near 0 and 40
    } 
    
    #LOOP 2: to remove negative values
    for (j in 1:span){ #THIS will fill noise matrix with extra grazing
      for (i in 1:(nrow * ncol)){
        if (Ug[[j]][i] <= 0){
          Ug[[j]][i] = 0
        } else {
          if (Ug[[j]][i] >= 40){ # 40
            Ug[[j]][i] = 0
          }   
        }
      }
    }
    
    for (j in 1:span){ # noise matrix without grazing
      for (i in 1:(nrow * ncol)){
        if (Ua[[j]][i] <= 0){
          Ua[[j]][i] = 0
        } else {
          if (Ua[[j]][i] >= 40){ # 40
            Ua[[j]][i] = 0
          }   
        }
      }
    }
    
    X <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # biological matrix = Ulva
    
    
    XOpen <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # biological matrix = Open Control
    
    Z <- sapply(aux, function(x) {
      matrix(runif(ncol*nrow,0,2), ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # biological
    
    vecTopo1 = matrix(yy$sim1, ncol, nrow) # Transf TOPOGRAPHY into 1 matrix
    X[[1]] = round(abs(vecTopo1 + rnorm(ncol*nrow, 0, 2))) # adding individuals to the starting point time zero
    XOpen[[1]] = round(abs(vecTopo1 + rnorm(ncol*nrow, 0, 2))) # adding individuals to the starting point
    
    pb <- txtProgressBar(title = "modelling...!", min = 0, max = (span - 1), style = 3)
    for (t in 1: (span - 1)){ # number of matrices
      for (i in 1: (ncol * nrow)){
        X[[t + 1]][i] = X[[t]][i] * exp(rEnc * (Ua[[t]][[i]] / 26.7) * (1 - (X[[t]][i] / K))) + Z[[t]][i] #solo Ulva
        XOpen[[t + 1]][i] = XOpen[[t]][i] * exp(rOpen * (Ug[[t]][[i]] / 26.7) * (1 - (XOpen[[t]][i] / K))) + Z[[t]][i] 
      } # 
      setTxtProgressBar(pb, t)
    }
    
    XER <- sapply(aux, function(x) {
      matrix(1, ncol = ncol, nrow = nrow)
    }, simplify = FALSE) # Store ln(C/E), effect ratios
    
    #for (t in 1: (span)){ # number of matrices
    #  for (i in 1: (ncol * nrow)){
    #    XER[[t]][i] = log(((0.1 + XOpen[[t]][i]) / (0.1 + X[[t]][i])))
    #  }
    #}
    
    #Antes de convertirla a vector WARNING "XER" or "X"
    FXOpen = list()
    FXx = list() #encloures
    FX = list()############### calculattion of fractal dimension of XER
    Var_XER = list(); Var_X = list();Var_XOpen = list(); mean_XER = list()
    Mean_X = list(); Mean_X_Open = list(); Var_XOpen = list()
    DX = list(); DXOpen = list()
    #Fractal dimension estimation for Effect ratio
    for (i in 1:span){
      FXOpen[[i]] = fd.estim.isotropic(XOpen[[i]],  direction='hv', #open controls
                                       plot.loglog = F, plot.allpoints = TRUE)
      #FX[[i]] = fd.estim.isotropic(XER[[i]],  direction='hv', #XER or EFFECT RATIO
      #                             plot.loglog = F, plot.allpoints = TRUE)
      FXx[[i]] = fd.estim.isotropic(X[[i]],  direction='hv', #Xenclosures AGAIN
                                    plot.loglog = F, plot.allpoints = TRUE)
      
      DX[[i]] = FXx[[i]]$fd #, digits=2) # notice is rounded, ENCLOSURE
      DXOpen[[i]] = FXOpen[[i]]$fd
      
      #Var_XER[[i]] = sd(XER[[i]])
      #mean_XER[[i]] = mean(XER[[i]])
      Var_X[[i]] = sd(X[[i]])
      Var_XOpen[[i]] = sd(XOpen[[i]])
      Mean_X[[i]] = mean(X[[i]]) #"enclosure"
      Mean_X_Open[[i]] = mean(XOpen[[i]]) #"open"
    }
    #### META-LOOP!
    #Result_Nat    = cbind(DXOpen, DX, Var_XER, Var_X, mean_XER, mean_X, MeanX, Mean_X_Open)
    Result[[k]]    = cbind(DXOpen, DX, Mean_X_Open, Mean_X, Var_XOpen, Var_X) 
    #invisible(Result_Nat)  
  }  #######END METALOOP!########
  
  Res_simul_Rand <<- NULL
    
  for (l in 1:replicas) {
    Res_simul_Rand[[l]] <<- data.frame(Result[[l]])
  }
  
}#######END function SIMULATION########
  
####CALLING RESULTS: FROM METALOOP
k = 5

simulation_Nat(k)   #Natural
simulation(k)       #artificial
simulation_rand(k)  #random
loc1 = setwd("C:/Users/localadmin_eliediaz/Documents/MEGA/")
s = 1
for (s in 1:k){
  #pdf(paste("/home/eliecer/MEGA/Natsimul/extra", s, ".pdf"))
  #pdf(paste("C:/Users/localadmin_eliediaz/Documents/MEGA/Natsimul/MegaNatural", s, ".pdf"))
  #pdf(paste("C:/Users/localadmin_eliediaz/Documents/MEGA/Natsimul/plus", s, ".pdf"))
  par(mfrow=c(3,1))
  plot(seq(1,364, 1), Res_simul_Nat[[s]]$DXOpen, type = "l",col=4, xlab='time',ylab='Fractal D', ylim=c(2.8,3.1), pch=1) #, solo pa cachar!
  lines(seq(1,364, 1), Res_simul_Nat[[s]]$DX, type = "l",col="red", xlab='time',ylab='Fractal D', pch=1) #, solo pa cachar!
  plot(seq(1,364, 1), Res_simul_Nat[[s]]$Mean_X_Open, type = "l",col=4, xlab='time',ylab='Mean_cover (%)', pch=1) #, solo pa cachar!
  lines(seq(1,364, 1), Res_simul_Nat[[s]]$Mean_X, type = "l",col="red", xlab='time', pch=1) #, solo pa cachar!
  plot(seq(1,364, 1), Res_simul_Nat[[s]]$Var_XOpen, type = "l",col=4, xlab='time',ylab='Sd_cover (%)', pch=1) #, solo pa cachar!
  lines(seq(1,364, 1), Res_simul_Nat[[s]]$Var_X, type = "l",col="red", xlab='time', pch=1) #, solo pa cachar!
  #dev.off()
}
save(Res_simul_Rand, file="C:/Users/localadmin_eliediaz/Documents/MEGA/Natsimul/Res_simul_Nat4.rda")
#load("Res_simul_Nat.rda.rda")

for (s in 1:k){
  #pdf(paste("/home/eliecer/MEGA/Rand/monday", s, ".pdf"))
  pdf(paste("C:/Users/localadmin_eliediaz/Documents/MEGA/Rand/extra", s, ".pdf"))
  par(mfrow=c(3,1))
  plot(seq(1,364, 1), Res_simul_Rand[[s]]$DXOpen, type = "l",col=4, xlab='time',ylab='Fractal D', ylim=c(2.8,3.1), pch=1) #, solo pa cachar!
  lines(seq(1,364, 1), Res_simul_Rand[[s]]$DX, type = "l",col="red", xlab='time',ylab='Fractal D', pch=1) #, solo pa cachar!
  plot(seq(1,364, 1), Res_simul_Rand[[s]]$Mean_X_Open, type = "l",col=4, xlab='time',ylab='Mean_cover (%)', pch=1) #, solo pa cachar!
  lines(seq(1,364, 1), Res_simul_Rand[[s]]$Mean_X, type = "l",col="red", xlab='time', pch=1) #, solo pa cachar!
  plot(seq(1,364, 1), Res_simul_Rand[[s]]$Var_XOpen, type = "l",col=4, xlab='time',ylab='Sd_cover (%)', pch=1) #, solo pa cachar!
  lines(seq(1,364, 1), Res_simul_Rand[[s]]$Var_X, type = "l",col="red", xlab='time', pch=1) #, solo pa cachar!
  dev.off()
}

save(Res_simul_Rand, file="C:/Users/localadmin_eliediaz/Documents/MEGA/Res_simul_Rand4.rda")


for (s in 1:k){
  #pdf(paste("/home/eliecer/MEGA/Arti/monday", s, ".pdf"))
  pdf(paste("C:/Users/localadmin_eliediaz/Documents/MEGA/Arti/extra", s, ".pdf"))
  par(mfrow=c(3,1))
  plot(seq(1,364, 1), Res_simul_Art[[s]]$DXOpen, type = "l",col=4, xlab='time',ylab='Fractal D', ylim=c(2.8,3.1), pch=1) #, solo pa cachar!
  lines(seq(1,364, 1), Res_simul_Art[[s]]$DX, type = "l",col="red", xlab='time',ylab='Fractal D', pch=1) #, solo pa cachar!
  plot(seq(1,364, 1), Res_simul_Art[[s]]$Mean_X_Open, type = "l",col=4, xlab='time',ylab='Mean_cover (%)', pch=1) #, solo pa cachar!
  lines(seq(1,364, 1), Res_simul_Art[[s]]$Mean_X, type = "l",col="red", xlab='time', pch=1) #, solo pa cachar!
  plot(seq(1,364, 1), Res_simul_Art[[s]]$Var_XOpen, type = "l",col=4, xlab='time',ylab='Sd_cover (%)', pch=1) #, solo pa cachar!
  lines(seq(1,364, 1), Res_simul_Art[[s]]$Var_X, type = "l",col="red", xlab='time', pch=1) #, solo pa cachar!
  dev.off()
}

save(Res_simul_Rand, file="C:/Users/localadmin_eliediaz/Documents/MEGA//Res_simul_Art4.rda")



###LOOP 5. unpacking the list and transforming data to vectors
span = 364
ncol = 60
nrow = ncol
varName = ncol * nrow 
for (i in 1:span){
  mean_XER[[i]] = as.vector(mean_XER[[i]]) # Originally was X[[i]] but change XER
  varName[i] = paste("Day", i, sep = "")
  names(mean_XER) = c(varName) # X or XER 
  mean_X[[i]] = as.vector(mean_X[[i]])
  names(mean_XER) = c(varName) # X or XER 
  names(mean_X) = c(varName)
}  #Transform the elements of the list to vectors

Xdf= data.frame(do.call(cbind, XER)) # XER or X
Xdfx =data.frame(do.call(cbind, X))
ci = cbind(yy, Xdf) # biniding yy and X
cix = cbind(yy, Xdfx)


###TO ANIMATE OR GO to 6
#plotting http://www.rdocumentation.org/packages/sp/functions/spplot
#https://stat.ethz.ch/pipermail/r-sig-geo/2010-July/008894.html
gridded(yy) = ~ x + y
gridded(ci) = ~ x + y #ER
gridded(cix) = ~ x + y # Ulva only in abscence of grazers

testit <- function(x){p1 <- proc.time()
                      Sys.sleep(x)
                      proc.time() - p1 # The cpu usage should be negligible
} # CLOCK!
require(gridExtra)

min = round(min(unlist(lapply(XER,FUN=min)))) #legend bar max
max = round(max(unlist(lapply(XER,FUN=max)))) #legen bar min


a = 0
for (i in 1:span){ #(i in 1:span){
  a[i] = i
  tmp = spplot(obj=cix[i], main = paste(a[i], c("day"), round(weather[i]), c("Temp"), c("D"), Dx[i],  sep = ":"),  scales = list(draw = T), col.regions = colorRampPalette(c('white', "green", "black")), at = 1:110)
  #tmp = spplot(obj=ci[i], main = paste(a[i], c("day"), weather[i], c("Temp"), c("D"),D[i],  sep = ","),  scales = list(draw = T), col.regions = colorRampPalette(c('white', "green", "black")), at = 1:110)
  #tmp1 = spplot(obj=ci[i], main = paste(a[i], c("day"), round(weather[i]), c("Temp"), c("D"), D[i],  sep = ","),  scales = list(draw = T), col.regions = colorRampPalette(c('white', "green", "black")), at = seq(min,max, l=50)) #, at = seq(-3,5, l=100))#, at = -14:10, at = 1:110
  
  print(tmp) #, split=c(2,1,3,3), more=T)
  #print(tmp1) #, split=c(2,2,3,3), more=T)
  
  testit(0.1)
}

spplot(obj=cix[1], main = paste(a[i], c("day"), round(weather[i]), c("Temp"), c("D"), Dx[i],  sep = ":"),  scales = list(draw = T), col.regions = colorRampPalette(c('black', 'gray80','red')))
plot(seq(1,length(weather),1), weather, type = "l")


####################################SEMIVARIOGRAMS, traditional way!
#### STEP 6
library(geoR)
#setwd("C:/Users/localadmin_eliediaz/Desktop") #WINDOWS
#s <- read.table("semivario.txt", header = TRUE)
names(ci)
dists <- dist(ci[,1:2]) # HE3E ARE COORDiNATES!

v1TestY = 1; v1TestY = list(v1TestY)
breaks = seq(0.25,15, l = 60)

for (j in 1:span-3){
  v1TestY[[j + 3]] <- variog(coords = ci[1800: 1860, 1: 2], data = ci[j+3][1800:1860,], breaks = breaks)
} # run the semivaiogram in the middel of the matrix (1800-1860)


v1.summaryTest = 1; v1.summaryTest = list(v1.summaryTest); Dt = 1; rela = list() #to initialize loop

for (j in 1: span){
  v1.summaryTest[[j]] <- cbind(1:length(breaks), v1TestY[[j]]$v)
  colnames(v1.summaryTest[[j]]) <- c("lag", "semi-variance")
  v1.summaryTest[[j]] = log(v1.summaryTest[[j]])
  rela[[j]] = lm(v1.summaryTest[[j]][ ,2] ~ v1.summaryTest[[j]][ ,1])
  Dt[j] =  (4 - abs((coef(rela[[j]])[2])))/2
}

par(mfrow=c(1,2))
plot(seq(1, 364,1), Dt, type="l"); plot(seq(1, 364,1), D, type="l")
relationship = lm(D ~ Dt)
summary(relationship)

plot(seq(1, 364,1), D, xaxt="n", type="l", col=4, xlab ='space', ylab ='indi', ylim = c(1.5,3.5))
lines(seq(1, 364,1), Dt, type="l", col=2, xlab ='space', ylab ='indi')     
axis(1, at = seq(0, 364, by = 2), las=1)
axis(2, at = seq(1.5, 4, by = 0.2), las=1)
