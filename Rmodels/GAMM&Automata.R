rm(list=ls(all=TRUE))
#setwd("/home/eliecer/Documents/Dropbox/DATA_CLIMATE") #LINUX
setwd("/home/ellis/Documents/Storage/DATA_CLIMATE") #LINUX MEGA
setwd("C:/Users/localadmin_eliediaz/Documents/MEGA/Storage/DATA_CLIMATE")
#Mac <- read.table("MonthNestedHabitat1.0.txt", 
#                   header = TRUE,
#                   dec = ".")
Mac <- read.table("All2.txt", 
                  header = TRUE,
                  dec = ".")
library(fields); library(sp)
library(geoR); require(gridExtra)
library(lattice);library(lme4); library(maptools)
library(sp); library(MASS); library(gamlss)  #For GAMs
library(gstat); library(mgcv); library(gamlss.dist); library(gamlss.add)
library(plotrix)
source("HighstatLibV9.R") 
source("/home/ellis/Documents/Storage/CDMCMCGLMGAMCourse/Data/MCMCSupportHighstat.R")
library(scatterplot3d); library(rgl)

Mac$year <- as.factor(Mac$year)
Mac$Month <- as.factor(Mac$Month)
Mac$Patch <- as.factor(Mac$Patch)
Mac$Temp <- as.factor(Mac$Temp)
Mac$Dist <- as.numeric(Mac$Dist)
Mac$Elev <- as.numeric(Mac$Elev)
Mac$Inun <- as.numeric(Mac$Inun)
Mac$Meso <- as.numeric(Mac$Meso)
Mac$Micro <- as.numeric(Mac$Micro)
Mac$Hab <- as.factor(Mac$Hab)
Mac$Response <- as.numeric(round(Mac$Response, digits = 0))

#OUTLIER value 138 removed
Mac = Mac[!Mac$Micro==138, ]
#What is the percentage of zeros?
100 * sum(Mac$Response == 0, na.rm = TRUE) / nrow(Mac)


################### to standardize the continuous covariates.

Mac$Elev <- (Mac$Elev - mean(Mac$Elev)) /   sd(Mac$Elev)
Mac$wm <- (Mac$wm - mean(Mac$wm)) / sd(Mac$wm)
Mac$Dist <- (Mac$Dist - mean(Mac$Dist)) /   sd(Mac$Dist)
Mac$Inun <- (Mac$Inun - mean(Mac$Inun)) /   sd(Mac$Inun)
Mac$Micro <- (Mac$Micro - mean(Mac$Micro)) /   sd(Mac$Micro)
Mac$Meso <- (Mac$Meso - mean(Mac$Meso)) /   sd(Mac$Meso)

G1_4 <- gamm(Response ~ Elev + s(Micro) + Hab  + Treat,
             random = list(Temp =~ 1),
             family = "negbin(0.1501)",
             niterPQL = 300,
             data = Mac, method = "REML")

summary(G1_4$lme)
summary(G1_4$gam)
#Linear mixed-effects model fit by maximum likelihood
#Random effects:
#  Formula: ~Xr - 1 | g
#Structure: pdIdnot
#Xr1      Xr2      Xr3      Xr4      Xr5      Xr6      Xr7      Xr8
#StdDev: 37.5562 37.5562 37.5562 37.5562 37.5562 37.5562 37.5562 37.5562
#
#Formula: ~1 | Temp %in% g
#(Intercept)  Residual
#StdDev:   0.3822987 0.9045686
#
#Variance function:
#  Structure: fixed weights
#Formula: ~invwt 
#Fixed effects: list(fixed) 
#Value Std.Error  DF   t-value p-value
#X(Intercept) -0.8933982 0.2107595 734 -4.238947  0.0000
#XElev        -0.9828271 0.1265155 734 -7.768434  0.0000
#XHab2         1.3884564 0.2065314 734  6.722739  0.0000
#XTreatT       2.5490041 0.1877689 734 13.575223  0.0000
#Xs(Micro)Fx1 -0.4750284 1.1883457 734 -0.399739  0.6895
#Correlation: 
#  X(Int) XElev  XHab2  XTretT
#XElev        -0.038                     
#XHab2        -0.546  0.146              
#XTreatT      -0.529 -0.031  0.040       
#Xs(Micro)Fx1  0.013 -0.117  0.011 -0.009

#Standardized Within-Group Residuals:
#  Min         Q1        Med         Q3        Max 
#-0.9734132 -0.4229894 -0.2913243 -0.1261893  7.2267975 

#Number of Observations: 752
#Number of Groups: 
#  g Temp %in% g 
#1          14 

ran1    <- ranef(G1_4$lme, level = 1)   #<-----------------------
ran2    <- ranef(G1_4$lme, level = 2)
bet1    <- fixef(G1_4$lme)


###############################checking

X2 <- expand.grid(
  Elev = seq(from = -1.629312, to = 1.917677, length = 12),
  Micro = seq(from = -1.194984, to = 2.228501, length = 12),
  Treat = levels(Mac$Treat),
  Hab = levels(Mac$Hab)
)

X2 <- expand.grid(
  Elev = -1.629312,
  Micro = -1.194984,
  Treat = levels(Mac$Treat),
  Hab = levels(Mac$Hab)
)


O1        <- X2[X2$Hab == "2",] #& X1[X1$Treat == "C", ]
#P1        <- X2[X2$Hab == "2",]
OT1       <- O1[O1$Treat == "C",] 

Xc5 <- model.matrix(~ Elev + Micro + Hab + Treat, #c => count part
                    data = OT1)

temp <- seq(11, 22, 5)
climChange <- -0.033*(temp^2) + 1.006*temp - 8.24 # transform temp to intercept scale


a <- as.numeric()
for (i in 1:length(climChange)){
  Sys.sleep(0.2)
  a[i] = i + 12
  Pr   <- predict(G1_4$gam, OT1, se = TRUE, type = "link") 
  Pra  <- exp(Pr$fit + climChange[i]) # Mean + random effects
  Pre  <- exp(Pr$fit - (Pr$se.fit) + climChange[i]) #lower CI
  Post  <- exp(Pr$fit + (Pr$se.fit) + climChange[i]) #upper CI
  
  asin <- plot3d(OT1$Elev, OT1$Micro, Pra, pch = 16,  highlight.3d=T, zlim = c(0, 100), ylim = c(-2,2),
                 type="s", # h original
                 box=FALSE,
                 size = 1,
                 #main="OpenRock-Noexclusions",
                 angle = 70, 
                 main = paste(a[i], c("C degree")),
                 zlab = " algal cover %", xlab = "elevation.sd", ylab="Micrograzers.sd")
  elev25 = seq(-1.61, 1.92 , length = 12)
  micro25 = seq(-1.194984, 2.21, length = 12)
  
  #And we convert the vector with expected values, ExpY, into
  #a 25 by 25 matrix
  ExpY.2d <- matrix(Pra, nrow = length(elev25), ncol = length(elev25))
  ExpY.pre <- matrix(Pre, nrow = length(elev25), ncol = length(elev25))
  ExpY.post <- matrix(Post, nrow = length(elev25), ncol = length(elev25))
  
  surface3d(elev25, micro25, ExpY.2d, 
            alpha = 0.7, 
            back = "lines", 
            col = "green",
            col.mesh = "black",
            alpha.mesh = 1,
            lit = TRUE)
  terrain3d(elev25, micro25,  ExpY.pre, col = "gray31", front = "lines", col.mesh = "red" )
  terrain3d(elev25, micro25,  ExpY.post, alpha = 0.8, front = "lines", col = "gray 39", lit = TRUE, col.mesh = "red" )
  Sys.sleep(1.7)
}


#SIMULATION

ncol = 6; nrow = 6; span = 3 #days
aux <- 1:span #how many matrices
range(Mac$Elev)

xy <- expand.grid(1:ncol, 1:nrow) # 1:ncol, 1:span # creates a  grid of coordinates

names(xy) <- c('x','y') # coordinates, x = columns, y = rows # gives the names 
g.dummy <- gstat(formula=z~ 1, locations=~x+y,
                 dummy=T, beta=c(1,0.01,0.005), model=vgm(psill=5, 
                                                          range=15, model='Exp'), nmax=60)

yy <- predict(g.dummy, newdata=xy, nsim=1) #random
yy$sim2 <- yy$sim1*18 - 51


#gridded(yy) = ~ x + y
#spplot(obj=yy[2], col.regions=colorRampPalette(c('black', 'gray80','red')))

topo.std <- (yy$sim2 - mean(yy$sim2)) / sd(yy$sim2)

topo <- sapply(aux, function(x) {
  matrix(topo.std, ncol = ncol, nrow = nrow)
}, simplify = FALSE) # TOPOGRAPHY

#Micrograzers autocorrelation generation

g.dummy_micro <- gstat(formula=z~ 1, locations=~x+y,
                       dummy=T, beta=c(1,0.01,0.005), model=vgm(psill=5, 
                                                                range=15, model='Exp'), nmax=60)

zz <- predict(g.dummy, newdata=xy, nsim=1) #random
zz$sim1
zz$sim2 <- zz$sim1*-41 - 4

#gridded(zz) = ~ x + y
#spplot(obj=zz[2], col.regions=colorRampPalette(c('black', 'gray80','red')))

zz_std <- (zz$sim2 - mean(zz$sim2)) / sd(zz$sim2) #Standarized micrograzers

micro_mat <- sapply(aux, function(x) {
  matrix(zz_std, ncol = ncol, nrow = nrow)
}, simplify = FALSE) # U = environmental noise


#Populating
alga <- sapply(aux, function(x) {
  matrix(1, ncol = ncol, nrow = nrow)
}, simplify = FALSE) # U = environmental noise

Pra <- sapply(aux, function(x) {
  matrix(1, ncol = ncol, nrow = nrow)
}, simplify = FALSE) # 


for (i in 1:span){
  for (j in 1:(nrow*ncol)){
    X2 <- expand.grid(
      Elev = topo[[i]][j], #each value of elev in each cell
      Micro = micro_mat[[i]][j], #each value of grazer in each cell
      Treat = levels(Mac$Treat),
      Hab = levels(Mac$Hab)
    )
    O1  <- X2[X2$Hab == "2",] #1 open rock, 2 = tidal pools
    OT1 <- O1[O1$Treat == "C",] #T = exclosure, #C = open access to grazers
    
    Pra[[i]][j] <- as.numeric(predict(G1_4$gam, OT1, se = F, type = "link")) #fixed effects
    print(j) 
    print(Pra[[i]][j])
    alga[[i]][j] <- exp(Pra[[i]][j] + climChange[i]) # fixed + random effects
    
    
  }
  cellcol<-matrix(seq(0,36),1) #gives color scale
  
  color2D.matplot(alga[[i]], #extremes = c(0,100),
                  c(0,1), c(0,0),c(0,0),
                  xrange = c(80, 90),
                  show.legend= T,
                  nslices = 20,
                  show.values=F)
  
  Sys.sleep(1.7)
}

alga_v = vector()
alga_v1 = unlist(alga)
alga_v2 = data.frame(t1 = alga_v1[1:36], t2 = alga_v1[37:72], t3 = alga_v1[73:108])
cix = cbind(yy, alga_v2)
gridded(cix) = ~ y + x

min = round(min(unlist(lapply(alga_v1,FUN=min)))) #legend bar max
max = round(max(unlist(lapply(alga_v1,FUN=max)))) #legen bar min


a = 0
for (i in 5:span){ #(i in 1:span){
  a[i] = i + 4
  tmp = spplot(obj=cix[i], main = paste(a[i], c("day"), round(a[i]), c("Temp"), c("D"), sep = ":"), scales = list(draw = T), col.regions = colorRampPalette(c('white', "green", "black")), at = 1:110)
  print(tmp) #, split=c(2,1,3,3), more=T)
  Sys.sleep(0.1)
}

#################PART 2 Cellular automata
#globals:
row = 60; col = 60; span = 10 #span or steps, col =columns, row = rows in the matrix

plat <- sapply(1:span, function(x) {matrix(0, nrow = row, ncol = col)}, simplify = FALSE) # rocky shore platform

plat[[1]] <- matrix(0, nrow= row, ncol =col) #initial state HERE DATA WILL GO, and evolution of states
plat[[1]] <- edit(plat[[1]]) #if I want to check something special

base_matrix = matrix(0, row, col) #the size of the grid
#asis = asistant matrix to store the neighbors sums
asis <- rep(list(rep(list(base_matrix), col-2)), col-2) # ASISTANT is the matrix, col - 2= numero de matrices, and 2 = numero de lists 

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
  asis_1 <- list()
  for (g in 1:(col-2)){
    asis_1[[g]] <- Reduce('+', asis[[g]]) #+ Reduce('+', asis[[2]]) + Reduce('+', asis[[3]]) + plat[[k]] #summing matrices
  }
  plat[[k + 1]] <- Reduce('+', asis_1) + plat[[k]]
  plat[[k + 1]][1:col, 1]   <- 0; plat[[k + 1]][1, 1: row]  <- 0 #cleaning the edge
  plat[[k + 1]][1:row, col] <- 0; plat[[k + 1]][row, 1:col] <- 0 #cleaning the edge
}




