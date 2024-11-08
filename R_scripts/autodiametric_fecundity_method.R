# Auto-diametric fecundity method
# contact: sabrina.beyer@noaa.gov


# Reference: Mapes, H., Beyer, S.G., Choi, J., Saas. E., Alonzo, S.H., Field, J.C.
# (2023) Image analysis approach to estimate fecundity of live-bearer rockfishes
# (Sebastes spp.) along the California Coast. Environmental Biology of Fishes 160:
# 1715-1732; doi.org/10.1007/s10641-023-01448-4


# Workflow:

# Auto-diametric (AD) fecundity method
    # 1) Develop the AD calibration curve for a new species from samples where 
    # both the mean oocyte diameter (imaging measurements) and the oocyte density  
    # (number per gram) are known

    # 2) After the AD curve is developed: use the parameters of the AD curve 
    # to convert the mean oocyte diameter of a new sample to an expected oocyte 
    # density (eggs per gram) (imaging only to measure oocyte diameters)

    # 3) Calculate fecundity of that sample by multiplying the oocyte density 
    # (eggs per gram) by the weight of the ovary (g)




rm(list=ls())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Auto-diametric (AD) fecundity method----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1) Develop the AD calibration curve----

# simulate some data
# log(oocyte_density) = log(alpha) + beta * log(oocyte_diameter) + some noise
o_diams       <- seq(400,1000, 5)                        # independent variable (mean oocyte diameter)
log.a         <- 26.93                                   # intercept parameter of log-log linear model
b             <- -2.78                                   # slope parameter of log-log linear model (also the exponent parameter of a power function)  
error         <- rnorm(length(o_diams), 0, 0.1)          # simulate some error (not quite accurate, but works for the purpose of this example)
log.o_density <- log.a + b*log(o_diams) + error          
o_density     <- exp(log.o_density)                      # oocyte density (eggs per gram) simulated data

# create a data frame 
mydf <- data.frame(o_diams,o_density )

# fit a simple linear model to the natural log transformed simulated data
lm.mod         <- lm(log(o_density )~log(o_diams), data=mydf)
log.a.mod      <- coef(lm.mod)[1]
a.mod          <- exp(log.a.mod)
b.mod          <- coef(lm.mod)[2]                         

# power function form 
# o_density  = alpha * length ^ beta
yy.o_dens <- a.mod * o_diams ^ b.mod  # median

# plot the simulated data and the model fit
plot(o_density ~o_diams, data=mydf, main="AD calibration curve")     
lines(yy.o_dens~o_diams)                                                    
text(850, 2.7e+04, labels=paste("y =",round(a.mod),"x^",round(b.mod,3), sep=" "), cex=0.8)



# 2) After the AD curve is developed: use the parameters of the AD curve 
# to convert the mean oocyte diameter of a new sample to an expected oocyte 
# density (eggs per gram) (imaging only, counts are no longer needed)
o_diameter_sample <- 600                                 # the mean oocyte diameter of a new sample (um)

o_density_sample <- a.mod * o_diameter_sample ^ b.mod    # the expected oocyte density (eggs per gram) for that new sample



# 3) Calculate fecundity of the new sample by multiplying the expected oocyte 
# density (eggs per g) by the weight of the ovary (g)
ovary_wgt_sample <- 100                # total weight of the ovary

fecundity_sample <- o_density_sample * ovary_wgt_sample
fecundity_sample                       # the fecundity estimate for the new sample



