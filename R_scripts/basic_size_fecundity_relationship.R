# Basic size-fecundity relationship
# contact: sabrina.beyer@noaa.gov

# An example of developing a size-fecundity relationship

# Data required: 
# 1) fish size (length or weight)
# 2) fecundity (obtained by either the AD, gravimetric, or other fecundity method)



rm(list=ls())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Basic size-fecundity relationship----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example:length-fecundity relationship

# simulate some data for the example
# log(fecundity) = log(alpha) + beta * log(length) + some noise
xx            <- seq(300, 500, 1)                        # independent variable (using fish length here for the example, but could be weight instead)
log.alpha     <- -11.938                                 # intercept parameter of log-log linear model
beta          <- 4.043                                   # slope parameter of log-log linear model (also the exponent parameter of a power function)  
error         <- rnorm(length(xx), 0, 0.1)               # simulate some error (not quite accurate, but works for the purpose of this example)
log.fecundity <- log.alpha + beta*log(xx) + error  
fecundity     <- exp(log.fecundity)                      # simulated fecundity data

# create a data frame 
mydf <- data.frame(xx,fecundity)

# fit a simple linear model to the natural log transformed simulated data
lm.mod         <- lm(log(fecundity)~log(xx), data=mydf)
log.alpha.mod  <- coef(lm.mod)[1]
alpha.mod      <- exp(log.alpha.mod)  # note, this is the median
beta.mod       <- coef(lm.mod)[2]                         

# power function form 
# fecundity = alpha * length ^ beta
yy <- alpha.mod * xx ^ beta.mod  

# plot the simulated data and the model fit
plot(fecundity~xx, data=mydf, main="Size-Fecundity Relationship")   
lines(yy~xx)                                                         
text(350, 5.5e+05, labels=paste("y =",round(alpha.mod,6),"x^",round(beta.mod,3), sep=" "), cex=0.8)

