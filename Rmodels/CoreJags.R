rm(list=ls(all=TRUE))
setwd("E:/Dropbox/CDMCMCGLMGAMCourse/eliScripts") #WINDOWS
setwd("C:/Documents and Settings/Diaz/Desktop/Dropbox/CDMCMCGLMGAMCourse/eliScripts") #windowsn office

setwd("/home/eliecer/Documents/Dropbox/CDMCMCGLMGAMCourse/eliScripts") #LINUX
Multi <- read.table(file = "Book2.txt", header = TRUE)
source("HighstatLibV6.R")
source("MCMCSupportHighstat.R")
library(lattice)
library(R2jags)
names(Multi)

#plotting the relationships
MyVar <- c("wm","distS", "Inund", "Elev",  "Meso",  "micro")
Mydotplot(Multi[,MyVar])

#collinearity
Mypairs(Multi[,MyVar])

#standarization
                     
Multi$wms <- (Multi$wm-mean(Multi$wm))/sd(Multi$wm)
Multi$distSs <- (Multi$distS-mean(Multi$distS))/sd(Multi$distS)
Multi$Inunds <- (Multi$Inund-mean(Multi$Inund))/sd(Multi$Inund)
Multi$Elevs <- (Multi$Elev-mean(Multi$Elev))/sd(Multi$Elev)
Multi$Mesos <- (Multi$Meso-mean(Multi$Meso))/sd(Multi$Meso)
Multi$micros <- (Multi$micro-mean(Multi$micro))/sd(Multi$micro)

#Apply linear regression in frequentist setiing

#this is the model  COver-i = beta_1 + beta_2 + beta_n 
#Everything signficant?
NOLE

M1 <- lm(Cover~wms + distSs + Inunds + Elevs + Mesos + micros, data = Multi)
summary(M1)


#Model validation
#A. Look at homogeneity: plot fitted values vs residuals
#B. Look at influential values: Cook
#C. Look at independence: 
#      plot residuals vs each covariate in the model
#      plot residuals vs each covariate not in the model
#      Common sense (smelly dependence?)
#D. Look at normality: histogram

#A. Homogeneity
E1 <- resid(M1)  
F1 <- fitted(M1)
par(mfrow = c(1, 1))
plot(x = F1, y = E1, 
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)

#B. Influential observations: Cook distance
plot(M1)


#C. Independence
plot(x=Multi$wms, y = E1, main = "Independence")
abline(0, 0, lty = 2)
plot(x=Multi$distSs, y = E1, main = "Independence")
abline(0, 0, lty = 2)
plot(x=Multi$Elev, y = E1, main = "Independence")
abline(0, 0, lty = 2)




#D. Normality
par(mfrow = c(1,1))
hist(E1, breaks = 5)

M2 <- lm(Cover~ wms + distSs + Elevs + Mesos + micros, data = Multi)
summary(M2)

plot(x=Multi$distSs, y = E1, main = "Independence")
abline(0, 0, lty = 2)

#(Intercept) -0.41023    0.18301  -2.242   0.0662 .
#wms         -0.16363    0.31459  -0.520   0.6216  
#distSs      -0.01234    0.28952  -0.043   0.9674  
#Elevs       -0.04438    0.31291  -0.142   0.8919  
#Mesos       -0.19820    0.32914  -0.602   0.5691  
#micros       0.35808    0.24709   1.449   0.1975  
#---










#Fit the same model in JAGS using specific coding      #preparaion of the data!

#1. Bundle data #construir the model para jags
X <- model.matrix(~wms + distSs + Elevs + Mesos + micros, 
                   data =Multi)
K <- ncol(X)  #Number of columns, gives the matrix according to x, porque 
#formo una matriz
edit(Multi)
colnames(X)

win.data <- list(Y = Multi$Cover, 
                 X = X,
                 N = nrow(Multi),
                 K = K)

###############################MARKHELP
#variance inflation
##################################MARKEHELP END


#2. JAGS modelling code
sink("/home/eliecer/Desktop/modellmeloco.txt")
cat("
model{

    #1. Priors beta and sigma
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)}  #0.0001 precision! = 1/sigma^2
    #priors for sigma
    tau <- 1 / (sigma * sigma)
    sigma ~ dunif(0.0001, 10)

    #2. Likelihood
    for (i in 1:N) {
      Y[i]   ~ dnorm(mu[i], tau)  #tau was sd or some precision 
      mu[i]  <- eta[i]
      eta[i] <- beta[1] * X[i,1]  + beta[2]* X[i,2] + beta [3]* X[i,3]+ beta[4]*X[i,4] + beta[5]*X[i,5] + beta[6]*X[i,6] # X[i,1] = 1 so no worries
  
      #Discrepancy measures
      Res[i]    <- Y[i] - mu[i]   #residuals
    }
}
",fill = TRUE)
sink()

inits  <- function () {
  list(
    beta     = rnorm(K, 0, 0.01), #ncolumns = number of residuals
    sigma    = runif(1, 0.0001, 10))  }

params <- c("beta", "sigma", "Res") #for M2
#4. Start JAGS
Eliecer <- jags(data       = win.data,
           inits      = inits,
           parameters = params,
           model      = "modellmeloco.txt",
           n.thin     = 10, #every 10th itiration will be saved
           n.chains   = 3,
           n.burnin   = 4000, #try to increase the burn in
           n.iter     = 30000) # try to increase the n
print(Eliecer, digits = 3)
names(Jone)
mean
summary(M2)
#dont forget beta1 is the intercept
#5. Assess mixing
out <- Eliecer$BUGSoutput
out
summary(M2)

#uNames is a function that we wrote.
#It creates a vector of character names.
uNames("beta", K)

MyBUGSChains(out, c(uNames("beta", K), "sigma")) #makes the mixing graph
MyBUGSACF(out, c(uNames("beta", K), "sigma"))    #autocorrelation graphs

#6. Present output
OUT <- MyBUGSOutput(out, c(uNames("beta", K), "sigma"))
print(OUT, digits =5) #DE RESULTADO
MyBUGSHist(out, c(uNames("beta", K), "sigma")) # just graph to see if the significant resultados

44444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444

#6. Present output
OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma"))
print(OUT1, digits =5)
MyBUGSHist(out, c(uNames("beta", K), "sigma"))


#Compare frequentist and JAGS results
OUT1
summary(M1)


0.90-0.09#####################################
#Model validation
#Get residuals. 
#How do we get residuals?
# 1. Get them from JAGS
# 2. Calculate them using posterior mean betas
# 3. If you did not calculate them in JAGS, calculate them outside JAGS

# 1. get them from JAGS
# These are posterior means of the residuals
E1a <- out$mean$Res

# 2. Get posterior means of the betas
#    Calculate fitted values
#    Calculate residuals
Beta.pm <- out$mean$beta   #Or use OUT1[1:2,1]  
eta     <- win.data$X %*% Beta.pm    #eta is beta1 +beta2 +x
mu      <- eta
E1b     <- win.data$Y - mu  

#3. If you did not calculate the residuals in JAGS,
#   then calculate them afterwards. It is a little
#   bit fussy to do this.
#   First extract all the MCMC beta values:

Beta.mcmc <- out$sims.list$beta  #All mcmc betas 
dim(Beta.mcmc)

#   And now calculate fitted values and residuals for 
#   each MCMC iteration

eta.mcmc  <- win.data$X %*% t(Beta.mcmc)
mu.mcmc   <- eta.mcmc

#Suppose you have 6,000 MCMC iterations. Then mu.mcmc
#contains the 6.000 fitted values. 

dim(mu.mcmc)

#   So, each column in mu.mcmc contains fitted values for a 
#   particular MCMC iteration. We could use these 
#   to calculate 95% credible intervals for the fitted
#   values!

#   Next we want to calculate Y - mu.mcmc, as this gives
#   us the residuals. But the problem is that Y is of dimension
#   146 by 1, and mu.mcmc is of dimension 146 by 6,000. Either we
#   use a loop (but R is slow with loops), or we create a matrix
#   Ybig that contains 142 rows and 6,000 columns. We opt for
#   the second approach. Each column in Ybig contains the Y data.


NumberMCMCIterations <- nrow(Beta.mcmc)
NumberMCMCIterations

#   Code below in words: Repeat the Y data in 6,000 columns 
#   In words again: Copy and paste the Y data in 6,000 columns 

Ybig <- matrix(rep(win.data$Y, NumberMCMCIterations), 
               ncol = NumberMCMCIterations, 
               byrow = FALSE)
dim(Ybig)
#   So...each column in Ybig contains the density data.
#   Now we can calculate the residuals for each of the 
#   6,000 MCMC iterations

E.mcmc <-  Ybig - mu.mcmc 
dim(E.mcmc)
#   These are the residuals that JAGS calculated!
#   These should be exactly the same as: out$sims.list$Res

#   Calculate the mean values (which are the posterior means)
#   per site:

E1c <- rowSums(E.mcmc) / NumberMCMCIterations
E1c

#   Are the same as out$mean$Res
######################################################




######################################################
# Continue model validation using E1a (most Bayesian approach), 
# E1b or E1c. The code below is copy-paste from the frequentist
# approach except that I now use E1a instead of E1.

#A. Homogeneity
F1 <- fitted(M2)
par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1a, 
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)
#Ouch...negative fitted values!!!!

#C. Independence
plot(x = Multi$wm, 
     y = E1a, 
     main = "Independence")
abline(h = 0,  
       lty = 2)
#Ouch.....we have heterogeneity and non-linear patterns

#Also plot residuals vs each covariate not in the model
boxplot(E1a ~ Multi$Meso)
abline(h = 0,  
       lty = 2)
#We need to add Period to the model!

#Spatial independence?
Multi$MyCex <- abs(E1a) / max(abs(E1a))
Fish3$MyCol <- E1a
Fish3$MyCol[E1a >= 0 ] <- 1
Fish3$MyCol[E1a < 0 ] <- 2

library(lattice)
xyplot(Ykm ~ Xkm,
       cex = 5 * sqrt(Fish3$MyCex),
       pch = 16,
       col = Fish3$MyCol,
       data = Fish3,
       main = "Spatial plot of residuals")

#D. Normality
par(mfrow = c(1, 1))
hist(E1a, breaks = 10)
######################################################


######################################################
#Model interpretation: Sketch fitted values
#Option 1: Only sketch the fitted values
#Option 2: Sketch fitted values with a 95% credible interval

#Problem for both options: depth may not be sorted.
#So we better create some artificial depth values on a grid
#and do predictions for these artifical values:

MyData <- data.frame(Depth.std = seq(from = -1.27 , to = 1.97 , 
                                     length = 10))
Xp     <- model.matrix(~ Depth.std, data = MyData)
#You better replace that 10 by a variable so that it is not hard-coded

#Under option 1:
Beta.pm <- out$mean$beta
P       <- Xp %*% Beta.pm
plot(x = Fish3$Depth.std, 
     y = Fish3$Dens, 
     col = Fish3$Period,
     xlab = "Depth",
     ylab = "Density", 
     main = "But it is all rubbish!",
     pch = 16)
lines(MyData$Depth.std, P, col = 1, lwd = 5)


#Under option 2:
# We have the 6,000 MCMC values of the betas.
# We can calculate the fitted values 6,000 times  
# and use these to calculate credible intervals:

#Get the 6,000 betas
Beta.mcmc <- out$sims.list$beta 

#Predict 6,000 times
eta.mcmc <- Xp %*% t(Beta.mcmc)
mu.mcmc  <- Xp %*% t(Beta.mcmc)
#Note that this mu.mcmc contains
#predictions for the 10 artifical depth values
#Earlier in this exercise mu.mcmc contained the 
#fitted values for the original data!

#Calculate the 2.5% and 97.5% values in
#mu.mcmc for each of the 10 depth values:
MyLinesStuff <- function(x){
   OUT <- matrix(nrow = nrow(x), ncol=4) 
	for(i in 1:nrow(x)){
	  xi <- x[i,]	
     OUT[i,3:4] <- quantile(xi, probs = c(0.025, 0.975))
     OUT[i,1] <- mean(xi)
     OUT[i,2] <- sd(xi)
	}
	colnames(OUT) <- c("mean", "se", "2.5%", "97.5%")
	OUT
}
L <- MyLinesStuff(P.mcmc)
L
#Each row in L is an artificial depth 
#Now plot the data and draw lines for the mean,
#and 2.5% and 97.5% values:

plot(x = Fish3$Depth.std, 
     y = Fish3$Dens, 
     col = Fish3$Period,
     xlab = "Depth",
     ylab = "Density", 
     main = "But it is all rubbish!",
     pch = 16)

lines(MyData$Depth.std, L[,"mean"], col = 1, lwd = 1)
lines(MyData$Depth.std, L[,"2.5%"], col = 1, lwd = 1, lty = 2)
lines(MyData$Depth.std, L[,"97.5%"], col = 1, lwd = 1, lty = 2)

#And these are the results from lm; nearly identical.
lines(MyData1$Depth.std, P1$fit, col = 2)
lines(MyData1$Depth.std, P1$fit + 1.96 * P1$se.fit, col = 2)
lines(MyData1$Depth.std, P1$fit - 1.96 * P1$se.fit, col = 2)
######################################################







######################################################
#Making the JAGS code more general
#Note the inprod function used for the predictor function eta
#It does the same thing as above

#1. Bundle data
X <- model.matrix(~Depth.std,
                   data = Fish3)
K <- ncol(X)  #Number of columns
head(X)
colnames(X)

win.data <- list(Y = Fish3$Dens, 
                 X = X,
                 N = nrow(Fish3),
                 K = K)
win.data

###################################################
#2. JAGS modelling code
sink("modellm.txt")
cat("
model{
    #1. Priors beta and sigma
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)}
    tau <- 1 / (sigma * sigma)
    sigma ~ dunif(0.0001, 10)

    #2. Likelihood
    for (i in 1:N) {
      Y[i]   ~ dnorm(mu[i], tau)   
      mu[i]  <- eta[i]
      eta[i] <- inprod(beta[], X[i,]) 
  
      #Discrepancy measures
      Res[i]    <- Y[i] - mu[i]
    }
}
",fill = TRUE)
sink()
#####################################
#

#3. Initial values & parameters to save
inits  <- function () {
  list(
    beta     = rnorm(K, 0, 0.01),
    sigma    = runif(1, 0.0001, 10))  }

params <- c("beta", "sigma", "Res")

#4. Start JAGS
J1 <- jags(data       = win.data,
           inits      = inits,
           parameters = params,
           model      = "modellm.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)

J2 <- update(J1, n.iter = 10000)  
print(J2, digits = 3)

#5. Assess mixing
out <- J2$BUGSoutput
MyBUGSChains(out, c(uNames("beta", K), "sigma"))
MyBUGSACF(out, c(uNames("beta", K), "sigma"))

#6. Present output
OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma"))
print(OUT1, digits =5)
MyBUGSHist(out, c(uNames("beta", K), "sigma"))


#Compare frequentist and JAGS results
OUT1
summary(M1)
######################################################








######################################################
#Add Period and interaction

#1. Bundle data
X <- model.matrix(~Depth.std * factor(Period),
                   data = Fish3)
K <- ncol(X)  #Number of columns
head(X)

win.data <- list(Y = Fish3$Dens, 
                 X = X,
                 N = nrow(Fish3),
                 K = K)
win.data

###################################################
#2. JAGS modelling code
sink("modellm.txt")
cat("
model{
    #1. Priors beta and sigma
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)}
    tau <- 1 / (sigma * sigma)
    sigma ~ dunif(0.0001, 10)

    #2. Likelihood
    for (i in 1:N) {
      Y[i]   ~ dnorm(mu[i], tau)   
      mu[i]  <- eta[i]
      eta[i] <- inprod(beta[], X[i,]) 
  
      #Discrepancy measures
      Res[i]    <- Y[i] - mu[i]
    }
}
",fill = TRUE)
sink()
#####################################
#

#3. Initial values & parameters to save
inits  <- function () {
  list(
    beta     = rnorm(K, 0, 0.01),
    sigma    = runif(1, 0.0001, 10))  }

params <- c("beta", "sigma", "Res")

#4. Start JAGS
J1 <- jags(data       = win.data,
           inits      = inits,
           parameters = params,
           model      = "modellm.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)

J2 <- update(J1, n.iter = 10000)  
print(J2, digits = 3)

#5. Assess mixing
out <- J2$BUGSoutput
MyBUGSChains(out, c(uNames("beta", K), "sigma"))
MyBUGSACF(out, c(uNames("beta", K), "sigma"))

#6. Present output
OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma"))
print(OUT1, digits =5)
MyBUGSHist(out, c(uNames("beta", K), "sigma"))


#Compare frequentist and JAGS results
M2 <- lm(Dens ~ Depth.std * factor(Period),
         data = Fish3)
OUT1
summary(M2)
######################################################







######################################################
#1. Improve priors in JAGS
#   Use multivariate normal distribution for beta priors
#   Use half-Cauchy(25) for priors of variances
#2. Calculate AIC and BIC in JAGS


#Model with depth, period and interaction:
X <- model.matrix(~Depth.std * factor(Period),
                   data = Fish3)
K <- ncol(X)  #Number of columns


#1. Bundle data
win.data <- list(Y    = Fish3$Dens, 
                 X    = X,
                 N    = nrow(Fish3),
                 logN = log(nrow(Fish3)),
                 Pi   = pi,
                 K    = K,
                 b0   = rep(0,K),
                 B0   = diag(0.0001, K))
win.data
#The b0 and B0 are used for a multivariate normal distribution
#b0 is a vector of zeros
#B0 is a diagonal matrix with values 0.0001 on the diagonal and zeros
#   elsewhere


###################################################
#2. JAGS modelling code
sink("modellm.txt")
cat("
model{
    #1A. Priors beta
    beta ~ dmnorm(b0[], B0[,]) 

    #1B. Priors for sigma terms (half-Cauchy(25))
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma <- abs(num / denom) 
    tau <- 1 / (sigma * sigma)


    #2. Likelihood
    for (i in 1:N) {
      Y[i]   ~ dnorm(mu[i], tau)   
      mu[i]  <- eta[i]
      eta[i] <- inprod(beta[], X[i,])         
  
      #Discrepancy measures
      Res[i]    <- Y[i] - mu[i]
      
      #Likelihood component for AIC and BIC terms
      Li[i] <- -( (Y[i] - mu[i])^2 ) / (2 * sigma * sigma) 
    }
    LogL <- sum(Li[1:N]) - (N / 2) * log(2 * Pi * sigma * sigma)
    AIC <- -2 * LogL + 2 * K
    BIC <- -2 * LogL + logN * K
}
",fill = TRUE)
sink()
#####################################
#

#3. Initial values & parameters to save
inits  <- function () {
  list(
    beta     = rnorm(K, 0, 0.01),
    num      = rnorm(1, 0, 25), 
    denom    = rnorm(1, 0, 1))  }

params <- c("beta", "sigma", "Res", "AIC", "BIC", "LogL")


#4. Start JAGS
J1 <- jags(data       = win.data,
           inits      = inits,
           parameters = params,
           model      = "modellm.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)

J2 <- update(J1, n.iter = 10000)  
print(J2, digits = 3)

#5. Assess mixing
out <- J2$BUGSoutput

MyBUGSChains(out, c(uNames("beta", K), "sigma"))
MyBUGSACF(out, c(uNames("beta", K), "sigma"))

#6. Present output
OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma", "AIC", "BIC","LogL"))
print(OUT1, digits =5)
MyBUGSHist(out, c(uNames("beta", K), "sigma"))
##########################################################







##########################################################
#Final task....sketch the fitted values for each period with CI

MyData <- expand.grid(Depth.std = seq(from = -1.27 , to = 1.97 , 
                                     length = 10),
                      Period = c(1,2))
Xp     <- model.matrix(~ Depth.std * factor(Period), data = MyData)
#You better replace that 10 by a variable so that it is not hard-coded


#Under option 2:
# We have the 6,000 MCMC values of the betas.
# We can calculate the fitted values 6,000 times  
# and use these to calculate credible intervals:

#Get the 6,000 betas
Beta.mcmc <- out$sims.list$beta 

#Predict 6,000 times
eta.mcmc <- Xp %*% t(Beta.mcmc)
mu.mcmc  <- Xp %*% t(Beta.mcmc)
dim(mu.mcmc)

#Note that this mu.mcmc contains
#predictions for the 10 artifical depth values
#Earlier in this exercise mu.mcmc contained the 
#fitted values for the original data!

#Calculate the 2.5% and 97.5% values in
#mu.mcmc for each of the 10 depth values:
MyLinesStuff <- function(x){
   OUT <- matrix(nrow = nrow(x), ncol=4) 
	for(i in 1:nrow(x)){
	  xi <- x[i,]	
     OUT[i,3:4] <- quantile(xi, probs = c(0.025, 0.975))
     OUT[i,1] <- mean(xi)
     OUT[i,2] <- sd(xi)
	}
	colnames(OUT) <- c("mean", "se", "2.5%", "97.5%")
	OUT
}
MyData$L <- MyLinesStuff(mu.mcmc)
MyData
#Each row in L is an artificial depth 
#Now plot the data and draw lines for the mean,
#and 2.5% and 97.5% values:

plot(x = Fish3$Depth.std, 
     y = Fish3$Dens, 
     col = Fish3$Period,
     xlab = "Depth",
     ylab = "Density", 
     main = "But it is all rubbish!",
     pch = 16)

with(subset(MyData, Period==1), {
 lines(Depth.std, L[,"mean"], col = 1, lwd = 1)
 lines(Depth.std, L[,"2.5%"], col = 1, lwd = 1, lty = 2)
 lines(Depth.std, L[,"97.5%"], col = 1, lwd = 1, lty = 2)
})

with(subset(MyData, Period == 2), {
 lines(Depth.std, L[,"mean"], col = 2, lwd = 1)
 lines(Depth.std, L[,"2.5%"], col = 2, lwd = 1, lty = 2)
 lines(Depth.std, L[,"97.5%"], col = 2, lwd = 1, lty = 2)
})
###############################################################




###############################################################
#Advanced exercise. Can be skipped in the course
#Using the 6,000 MCMC iterations we can also calculate the difference
#between the 2 lines ...and also calculate a 95% credible interval for the difference:
mu.mcmcP1 <- mu.mcmc[1:10,]   #6000 MCMC iterations first line
mu.mcmcP2 <- mu.mcmc[11:20,]  #6000 MCMC iterations second line

#Calculate the difference between the two lines for each of the 6,000 iterations
DifferenceP1P2 <- mu.mcmcP1 - mu.mcmcP2
dim(DifferenceP1P2)  #This is the difference between the 2 lines for each MCMC iteration

#quartz()
#Plot the posterior mean difference
NaturePaper <- MyLinesStuff(DifferenceP1P2)
plot(x= MyData$Depth.std[1:10], 
     y = NaturePaper[,1], 
     xlab = "Depth",
     ylab = "Posterior mean of difference between the two fitted lines",
     type =  "l", 
     ylim = c(-0.01,0.01))

#Add a 95% credible interval for the difference
lines(MyData$Depth.std[1:10], NaturePaper[,3])
lines(MyData$Depth.std[1:10], NaturePaper[,4])
abline(h = 0, col =2)
###############################################################



