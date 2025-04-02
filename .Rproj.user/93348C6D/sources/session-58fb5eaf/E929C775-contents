# Example of a basic size-fecundity relationship
# Description: Fit a power function to body size and fecundity data
# Contact: sabrina.beyer@noaa.gov, NOAA Northwest Fisheries Science Center

# Data required: 
# 1) fish size (length or weight)
# 2) fecundity (obtained by either the autodiametric (AD), gravimetric, 
#    or other fecundity method)


# clear work space
rm(list=ls())

# ~~~~~~~~~~~~~
# Functions----
# ~~~~~~~~~~~~~
  # Bias correction function to return the mean instead of median from log-space
  # parameter estimation of a power function (only needed for alpha)
  # Background:
      # log-normal distribution: lognormal(u,sd^2)
      # median: exp(u)
      # mean:   exp(u + sd^2/2), same as: exp(u)*exp(0.5*sd^2)
  bias_correct <- function(model){
    alpha_med  <- exp(model$coefficients[1])
    beta       <- model$coefficients[2]
    sdres      <- sd(model$residuals) 
    alpha_mean <- alpha_med*exp(0.5*sdres^2)  
    return(as.numeric(c(alpha_mean, beta)))
  }

  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic size-fecundity relationship----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 # Example:length-fecundity relationship

# ~~~~~~~~~~~~~~~~~~~
# Simulate data----
# ~~~~~~~~~~~~~~~~~~~
  # simulate some data for the example
  # log(fecundity) = log(alpha) + beta * log(length) + some noise
  xx            <- seq(300, 500, 1)                         # independent variable (using fish length here (mm) for the example, but this could be weight instead)
  log.alpha     <- -11.938                                  # intercept parameter of log-log linear model
  beta          <- 4.043                                    # slope parameter of log-log linear model (this is also the exponent parameter of the power function)  
  error         <- rlnorm(length(xx), meanlog=0, sdlog=0.1) # simulate some error
  log.fecundity <- log.alpha + beta*log(xx) + error  
  fecundity     <- exp(log.fecundity)                       # simulated fecundity data
  
  # create a data frame 
  mydf <- data.frame(xx,fecundity)

  
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Parameter estimation----
# ~~~~~~~~~~~~~~~~~~~~~~~~~
  # fit a simple linear model to the natural log transformed simulated data
  lm.mod         <- lm(log(fecundity)~log(xx), data=mydf)
  log.alpha.mod  <- coef(lm.mod)[1]
  alpha.mod      <- exp(log.alpha.mod)  # note, this is the median
  beta.mod       <- coef(lm.mod)[2]    
  
  # bias correct (bc) alpha to return the mean instead of median
  # beta remains the same
  bc.results.mod  <- bias_correct(lm.mod)
  bc.alpha.mod    <- bc.results.mod[1]
  
  # parameter estimates for the power function
  # power function form 
  # fecundity = alpha * length ^ beta
  alpha.mod          # scaling coefficient median
  bc.alpha.mod       # scaling coefficient mean (bias corrected)
  beta.mod           # exponent

  
#~~~~~~~~~~~~~~~~~~~~~~~
# Visualize results----
#~~~~~~~~~~~~~~~~~~~~~~~
  # fecundity = alpha * length ^ beta
  yy.median  <- alpha.mod    * xx ^ beta.mod
  yy.mean    <- bc.alpha.mod * xx ^ beta.mod
  
  # plot the simulated data and the model fit
  plot(fecundity~xx, data=mydf, main="Size-Fecundity Relationship", 
       pch=19, col="gray", 
       xlab="Length (cm)",
       ylab="Fecundity")   
  lines(yy.mean  ~ xx, lty=1,col="black")
  lines(yy.median~ xx, lty=2,col="red")
  legend("topleft", bty='n', legend = c("mean","median","simulated data"), lty=c(1,2,NA), pch=c(NA,NA,19), col=c("black", "red","gray"))
  text(450, 2.0e+05, labels=paste("y (mean) =",round(bc.alpha.mod,6),"x^",round(beta.mod,3), sep=" "), cex=0.8)

