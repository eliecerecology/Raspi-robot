# Environmental stochasticity
rm(list=ls(all=TRUE)) 
par(mfrow=c(1,1))
# Write a function for generating environmental noise:
noise = function(size,k,w){ # 200,0.5, 10
	y = numeric(size)
	y[1] = rnorm(1,0,sd)
	e = rnorm(size)
	for(t in 2:size){
		y[t] = k*y[t-1] + w^2*sqrt(1-k^2)*e[t-1]
	}
	y = w*(y-mean(y))/sd(y)
	return(y)
}

# Environmental noise# --------------------
F = 60     # simulation time span 
kappa = 0 # environmental autocorrelation
sd = 1   # environmental amplitude (standard deviation)

Y = noise(F,kappa,sd) # call function with same structure of elements,  
#noise to generate an environmental time series
length(Y)

#setwd("/home/eliecer/grive/EcolModelling14")
#write.table(Y, "NOISY.txt")




# Plot noise time series
par(mfrow=c(1,1))
plot(Y,type='l',col=4,main='Noise time series',xlab='Time',ylab='Noise')

# Plot noise autocorrelation function
acf(Y,main='Autocorrelation function')

# Population dynamics:
# --------------------
r = 0.7 # Intrinsic population growth rate
K = 100 # carrying capacity

x = as.numeric(); xx = as.numeric(); xxx = as.numeric(); xxxx = as.numeric()
x[1] = 10
xx[1] = 10
xxx[1] = 10
xxxx[1] = 10


for(t in 1:(60-1)){
	# Noise affects population K at each time: K[t] = K + Y[t]
	x[t+1] = x[t]*exp(r*Y[t]*(1 - x[t]/(K*Y[t])))
	xx[t+1] = xx[t]*exp(r*(1 - xx[t]/(K[t]+Y[t])))
	xxx[t+1] = xxx[t]*exp(r*(1 - xxx[t]/(K)))
  xxxx[t+1] = xxxx[t]*exp(r*(1 - xxxx[t]/(K)))
}
par(mfrow=c(2,2))
plot(seq(1,60,1),(1 - (xxx/(K+Y))), type ="l")
plot(seq(1,60,1),(1 - (xxx/(K*Y))), type ="l")
plot(seq(1,60,1),(1 - (xxx/(K))), type ="l")
plot(seq(1,60,1),(1 - (xxxx/(K))), type ="l")

xxx
edit(xxxx)
print(xx)
koko = (r*Y)
range(r)
range(Y)
range(koko)
par(mfrow=c(3,1))
# Plot population time series
plot(x,type='l',col=4,main='Population time series',xlab='Time',ylab='Population density')
plot(xx,type='l',col=4,main='Population time series',xlab='Time',ylab='Population density')
plot(xxx,type='l',col=4,main='Population time series',xlab='Time',ylab='Population density')
plot(xxxx,type='l',col=4,main='Population time series',xlab='Time',ylab='Population density')

plot(Y,type='l',col="red",main='Noise time series',xlab='Time',ylab='Noise')
# Plot population autocorrelation function
acf(x,main='Autocorrelation function')