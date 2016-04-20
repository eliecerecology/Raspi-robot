#install.packages("gsarima")
#library(gsarima)
library(fractaldim); library(RandomFields)
library(gstat); library(lattice); library(fields); library(sp)
#print(acf(arima.sim(model=list(ar=-0.5),n=400)))
#http://rstudio-pubs-static.s3.amazonaws.com/9688_a49c681fab974bbca889e3eae9fbb837.html
#http://santiago.begueria.es/2010/10/generating-spatially-correlated-random-fields-with-r/
library(gstat)

xy <- expand.grid(1:100, 1:100)
names(xy) <- c('x','y')
g.dummy <- gstat(formula=z~1+x-y,
                 locations=~x+y,
                 dummy=T,
                 beta= c(0.005,2,10),
                 model=vgm(psill=0.025, range=15, model='Exp'),
                 nmax=20) 

xx <- runif(10000, 5.0, 40.5)

yy <- predict(g.dummy, newdata=xy, nsim=4)
gridded(yy) <- ~x+y
spplot(obj=yy[3])
spplot(yy)
aa <- matrix(Y, ncol = 100, nrow = 100)

fd.estim.isotropic(bb,  direction='hv', #XER or X
                   plot.loglog = F, plot.allpoints = TRUE)

####################################################################################
####################################################################################
install.packages("raster")
library(raster)
simgrid <- expand.grid(1:50, 1:50)
n <- nrow(simgrid)

elev <- raster(matrix(rnorm(n), 50, 50),
               xmn = 0, xmx = 50,
               ymn = 0, ymx = 50)

plot(elev, main = "Elevation")
bb <- matrix(elev[1], ncol= 50, nrow= 50)
edit(bb)
rmvn <- function(n, mu = 0, V = matrix(1)) {
  p <- length(mu)
  if (any(is.na(match(dim(V), p)))) 
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n * p), ncol = p) %*% D + rep(mu, rep(n, p)))
}

# Set up a square lattice region
simgrid <- expand.grid(1:50, 1:50)
n <- nrow(simgrid)

# Set up distance matrix
distance <- as.matrix(dist(simgrid))
# Generate random variable
phi <- 0.001
X <- rmvn(1, rep(0, n), exp(-phi * distance))

# Visualize results
Xraster <- rasterFromXYZ(cbind(simgrid[, 1:2] - 0.5, X))
par(mfrow = c(1, 1))
plot(1:100, exp(-phi * 1:100), type = "l", xlab = "Distance", ylab = "Correlation")
plot(Xraster)










