rm(list=ls(all=TRUE))
#setwd("/home/eliecer/Documents/Dropbox/DATA_CLIMATE") #LINUX
setwd("/home/eliecer/MEGA/Storage/DATA_CLIMATE") #LINUX MEGA
setwd("C:/Users/localadmin_eliediaz/Documents/MEGA/Storage/DATA_CLIMATE")
#Mac <- read.table("MonthNestedHabitat1.0.txt", 
#                   header = TRUE,
#                   dec = ".")
Mac <- read.table("All2.txt", 
                  header = TRUE,
                  dec = ".")
edit(Mac)
library(lattice)  #Needed for multi-panel graphs
library(lme4); library(ggplot2);  library(maptools)
library(sp); #install.packages("ggmap")
#library(ggmap); #install.packages("rgdal")
#library(rgdal)
library(mgcv);
source("HighstatLibV9.R") 
#edit(Mac)
#names(Mac)
#str(Mac)
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
#Mac$Elevtemp <- as.numeric(Mac$Elevtemp)
#Mac$Patch <- as.factor(Mac$Patch)
Mac$Response <- as.numeric(round(Mac$Response, digits = 0))
#Mac$controls <- as.numeric(round(Mac$controls, digits = 0))
##################Data exploration
#Outliers
#Collinearity
MyVar1 <- c("Elev", "Dist", "Inun", "Micro", "Meso","wm")
MyVar2 <- c("Elev", "Micro", "wm")
MyVar3 <- c("Elev", "Inun", "Meso")
MyVar4 <- c("Elev", "Micro") # <-----NO COLLINEARITY


#OUTLIER value 138 removed
Mac = Mac[!Mac$Micro==138,]


pairs(Mac[, MyVar1], lower.panel = panel.cor) #Elev
pairs(Mac[, MyVar2], lower.panel = panel.cor) # good additive
pairs(Mac[, MyVar3], lower.panel = panel.cor)
pairs(Mac[, MyVar4], lower.panel = panel.cor)


corvif(Mac[,MyVar1])
corvif(Mac[,MyVar2])
corvif(Mac[,MyVar3])
corvif(Mac[,MyVar4])
#Indicates collinearity SHOULD NOT EXCEED 3
#But the outliers may be doing funny things!

xyplot(Mac$Response ~ Mac$Elev | factor(Mac$Temp), 
       strip = strip.custom(bg ="red"))
oma = fitted(G1, link="logit")
xyplot(oma ~ Mac$Elev | factor(Mac$Temp), 
       strip = strip.custom(bg ="blue"))


#### I will use mesograzers as OFFSET

#Is there a  effect?
boxplot(Response ~ Hab, 
        data = Mac, 
        varwidth = TRUE) #stays

boxplot(Response ~ Patch, 
        data = Mac, 
        varwidth = TRUE) #Stays

boxplot(Response ~ Treat, 
        data = Mac, 
        varwidth = TRUE) #stays

boxplot(Response ~ year, 
        data = Mac, 
        varwidth = TRUE) #NOT

boxplot(Response ~ Temp, 
        data = Mac, 
        varwidth = TRUE) # Stays

xyplot(Response ~ Micro  , 
       type="h",
       data = Mac, 
       col = 1)



#What is the percentage of zeros?

100 * sum(Mac$Response == 0, na.rm = TRUE) / nrow(Mac)
#68.8%, 68.08 without 1 outlier
plot(table(Mac$Response), 
     type = "h")

#I have zero inflation!
#And this captures the sequential nature of the data  
xyplot(Response ~ Elev | Patch,
       type="h",
       subset = (Hab == "1"),
       data = Mac,
       col = "blue")

win.graph()

xyplot(Response ~ Elev | Patch,
       type="h",
       subset = (Hab == "2"),
       data = Mac,
       col = "red")

# RELATIONSHIPS?

plot(x = Mac$Elev,
     y = Mac$Response,
     xlab = "Elevation",
     ylab = "% Alga")
win.graph()

plot(x = Mac$Micro,
     y = Mac$Response,
     xlab = "micrograzers",
     ylab = "% Alga")



par(mfrow = c(2,3))
boxplot(Response ~ Hab, data = Mac)
boxplot(Response ~ Treat, data = Mac)
boxplot(Response ~ Temp, data = Mac) 
boxplot(Response ~ year, data = Mac)
boxplot(Response ~ Hab, data = Mac)


########################### Frequentist analysis
#######################################################################
############## To avoid warning messages from lme4 it may be wise
################### to standardize the continuous covariates.

Mac$Elev <- (Mac$Elev - mean(Mac$Elev)) /   sd(Mac$Elev)
Mac$wm <- (Mac$wm - mean(Mac$wm)) / sd(Mac$wm)
Mac$Dist <- (Mac$Dist - mean(Mac$Dist)) /   sd(Mac$Dist)
Mac$Inun <- (Mac$Inun - mean(Mac$Inun)) /   sd(Mac$Inun)
Mac$Micro <- (Mac$Micro - mean(Mac$Micro)) /   sd(Mac$Micro)
Mac$Meso <- (Mac$Meso - mean(Mac$Meso)) /   sd(Mac$Meso)
#micro = log(Mac$Micro04 + 10) ###Offset
#glmer instead of glm for random effects
names(Mac)

M0 <- glmer(Response ~ Elev + Micro + Hab + Treat +  (1|Month), 
           data = Mac,
           family = poisson)

M1 <- glmer(Response ~ Elev + Micro*Hab + Treat +  (1|Month), 
            data = Mac,
            family = poisson)

AIC(M0, M1) # still M3
#df      AIC
#M0  6 16185.44
#M1  7 16086.88

#Check overdispersion
o = N1_1
E <- resid(o, type = "pearson")
N  <- nrow(Mac)
p  <- length(coef(o))  + 1 #if you add random effects
Dispersion <- sum(E^2) / (N - p)
Dispersion

# Get the fitted values
F1 <- fitted(o)
# Plot residuals versus fitted values
plot(x = F1,
     y = E,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)     
#>>>>>>>>>>>>>>>>>got 1 residual, but not so much

#Plot residuals vs each covariate in the model
#M8 : Response ~ Micro + Meso + Hab + Treat +  Micro:Meso +  (1|Elev:Temp:Dist), 
      
plot(x = Mac$Micro,
     y = E,
     xlab = "Months",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)    #Not good 

plot(x = Mac$Elev,
     y = E,
     xlab = "Months",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)    #good 

# Is there a non-linear pattern in here?
# Apply a smoother on the residuals. If
# it is a flat line then there are no
# residual patterns.
#M3 :glmer(Response ~ Elev*Hab*Treat + Micro + Meso + Dist  +  (1|Month),

T1 <- gam(E ~ s(Micro), 
          data = Mac) #VERY
T2 <- gam(E ~ s(Elev), 
          data = Mac) #very

plot(T2)
#Mac$Eps <- 1:nrow(Mac) #observational level random intercept #thing
#micro = log(Mac$Micro + 10) ###Offset

library(glmmADMB)
library(MASS)
#installed.packages("gamlss")
library(gamlss)  #For GAMs
#library(mgcv)    #For GAMs
library(gstat)
#m1:Response ~            Elev + Micro*Hab + Treat +  (1|Month)

N1 <- glmmadmb(Response ~ Elev + Micro*Hab + Treat +  (1|Month), 
               family = "nbinom",
               mcmc = F,
               zeroInfl = F, 
               data = Mac, debug = F) # ZINB Disp 0.76

o = N1_1
E <- resid(o, type = "pearson")
N  <- nrow(Mac)
p  <- length(coef(o)) + 1 + 1   # 1 = binomial and +1 if you add random effects
Dispersion <- sum(E^2) / (N - p)
Dispersion #N1= 1.001 without outlier, N1 =1.01 with outlier

drop1(N1)

N1_1 <- glmmadmb(Response ~ Elev + Micro + Hab + Treat +  (1|Month), 
               family = "nbinom",
               mcmc = F,
               zeroInfl = F, 
               data = Mac, debug = F) # ZINB Disp 0.76

AIC(N1, N1_1)


F <- fitted(o)
plot(x = F,
     y = E,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)    

summary(N1_1)





#GAMS
G0 <- gam(Response ~ s(Elev) + s(Micro, by=Hab)  + Treat,
          random = list(Month =~ 1), 
          family = poisson,
          data = Mac)

plot(G0, select = c(4), cex.lab = 1.5)

o = G0
E3 <- resid(o, type = "pearson")
N  <- nrow(Mac)
p  <- length(coef(o)) + 1 + 1   # 1 = binomial and +1 if you add random effects
Dispersion <- sum(E3^2) / (N - p)
Dispersion # 19.19 sin el outliar, 4643 con el residual


plot(x = Mac$Elev,
     y = E3,
     xlab = "Response",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2) # Great


plot(x = Mac$Hab,
     y = E3,
     xlab = "Habitat",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)    #Great

plot(x = Mac$Treat,
     y = E3,
     xlab = "Habitat",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)    #Great

#Section 6.6 Zero inflated GAM
library(gamlss.dist)
library(gamlss.add)

#Fit the equivalent of M8
G1 <- gam(Response ~ s(Elev) + s(Micro, by=Hab)  + Treat,
          random = list(Month =~ 1), 
          family = nb,
          data = Mac)

G1_1 <- gam(Response ~ s(Elev) + s(Micro) +Hab  + Treat,
          random = list(Month =~ 1), 
          family = nb,
          data = Mac)


o = G1_1
E3 <- resid(o, type = "pearson")
N  <- nrow(Mac)
p  <- length(coef(o)) +  1 +1   # 1 = binomial and +1 if you add random effects
Dispersion <- sum(E3^2) / (N - p)
Dispersion # 1.38 sin el residual, 1.40 con el residual
summary(G1_1)
#comand to detect outliers
#pnt <- identify(Fitted, PearsonRes, plot = F) # To detect points
#points(Mac$Micro[pnt], PearsonRes, col = "red") # To mark points

con <- gamlss.control(n.cyc = 200)
G2 <- gamlss(Response ~ cs(Elev, df = 8) +
               cs(Micro, by=Hab) +
               factor(Treat),
             random = list(Month =~ 1),
             family = ZIP(),
             control = con,
             data = Mac)
summary(G2)

Pi <- fitted(G2, "sigma")[1]
Pi
o = G2
pi   <- o$sigma.fv[1]
mu   <- o$mu.fv
ExpY <- (1 - pi) * mu
varY <-  mu * (1 - pi) * (1 + mu * pi)
PearsonRes <- (Mac$Response - ExpY) / sqrt(varY)
N    <- nrow(Mac)
p    <- o$df.fit
Overdispersion <- (sum(PearsonRes^2) / (N - p)) + 1 
Overdispersion #2.7 sin el residual y 2.68 con el residual


summary(G2)

###################################DONE#########################
plot(x = ExpY, 
     y = PearsonRes,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, v = 0)

plot(x = Mac$Elev,
     y = PearsonRes,
     xlab = "Elevation",
     ylab = "residual")

plot(x = Mac$Micro,
     y = PearsonRes,
     xlab = "micrograzers",
     ylab = "% Alga")
par(mar = c (2,2,2,2))
vis.gam(G1, theta = 137, color ="gray")

xyplot(Mac$Response ~ Mac$Elev | factor(Mac$Temp), 
       strip = strip.custom(bg ="red"))

AIC(N1,G0,G1,G2) 
#df       AIC #CON EL RESIDUAL
#N1  8.00000  3170.100
#G0 27.08735 12171.486
#G1 20.41123  3095.776 <--------------BEST
#G2 16.00030  8179.181
####SIN RESIDUAL
#df       AIC
#N1  8.00000  3168.880
#G0 28.82868 11582.129
#G1 20.35868  3093.547 <--------------BEST
#G2 16.00038  8175.350
#####################################################END##########################################3


