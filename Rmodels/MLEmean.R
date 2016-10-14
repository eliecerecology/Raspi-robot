rm(list=ls(all=TRUE))

data <- c(10, 7 ,9, 8, 6, 3 , 2, 6, 7, 4, 6, 7) #Eliecer DATA

#usage: Have to define the range to assess the parameter, in this case MEAN, for example
#lowerlimit = -5, and upperlimit = 30 (range in which the mean can be found); precision
#means the variation in the differences between averages, eg. precision = 0.01. Expected mean,
#implies the mean to start to search for the best expected mean, and dispersion referres to
#the dispersion parameter in the negativebinomial distribution.
#
# eg.
# average_MLE(-5, 30, 0.01, 7,5) # FOR E. Diaz data


average_MLE <- function(lowerlimit, upperlimit, precision, expected_mean, dispersion) {
  
    x <- seq(lowerlimit, upperlimit, precision)
    Expe_means <- expected_mean + x
    dispersion <- dispersion
    ncol = length(x) + 1
    Prob_success <- (dispersion/(dispersion + Expe_means))
    matriz <- matrix(0, nrow = length(data) + 3, ncol = ncol)
    
    matriz[3:14, 1] <- data
    matriz[1, 2:ncol] <- Expe_means
    matriz[2, 2:ncol] <- dispersion/(dispersion + Expe_means) #Prob_success
    
    
    for (j in 3:14){
        for (i in 1: (ncol - 1)){
            matriz[3:14, i + 1] <- log(dnbinom(data, size = dispersion, matriz[2, i + 1]))
        }
    }
    for (i in 1:length(x)){matriz[15, 1 + i] <- sum(matriz[3: 14, 1 + i])*-1}

    print(c("MLE = "));
    print((min(matriz[15,2:ncol]))) # y-axis
    min_index <- match(min(matriz[15,2:ncol]), matriz[15,2:ncol]) #index
    print(c("Expected average = "));
    print(min(matriz[1,min_index])) # x-axis
    
    plot(matriz[1, 2:ncol], matriz[15, 2:ncol], ylim = c(0, 100), xlab ="Expected average",
         ylab ="Sum-Log_likelihood", main = "Likelihood profile")
    points(x = min(matriz[1,min_index]), y = min(matriz[15,2:ncol]), col = "red", pch=20, cex= 2)
    abline(h = min(matriz[15,2:ncol]) + 1.92, b = F)
    abline(v = min(matriz[1,min_index]) + 1.95, b = F)
    abline(v = min(matriz[1,min_index]) - 1.95, b = F)
    #coords <- locator(type="l") # add lines / localizer
}

#Answers and plot
average_MLE(-5, 30, 0.01, 7, 5)

#EXPECTED MEAN OF BIRDS = 6.24 calculated using MLE = 29.10372
#The confidence about the uncertainty of population density, there is some overdispersion or overestimation on the transects with large amount of birds
# and some underdispersion or underestimation of birds in the transects with low number of birds


average_MLE(lowerlimit, upperlimit, precision, expected_mean, dispersion)
