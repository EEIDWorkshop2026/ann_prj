###############################################
# Zuur et al. 8, 9, 10, 11 
# Input: Hawaii.txt
# Author: Ann 
###############################################

library(dplyr)
library(tidyverse)
library(nlme)
library(lme4)
library(ggplot2)
library(lmerTest)
library(gridExtra)

#evaluate normality assumption with histograms
Sparrows <- read.table(file = "ZuurDataMixedModelling/Sparrows.txt", header = TRUE, dec = ".")

# Set up 2x2 grid
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Panel A: Simple Frequency Histogram
hist(Sparrows$wt, nclass = 15, col = "grey", 
     xlab = "Weight", main = "A: Observed Data (Counts)")

# Panel B: Density Histogram (Area = 1)
# freq = FALSE or prob = TRUE scales the y-axis to density
hist(Sparrows$wt, nclass = 15, col = "grey", freq = FALSE, #freq = FALSE option scales HISTOGRAM so that the area inside the histogram equals 1.
     xlab = "Weight", main = "B: Observed Data (Density)")

# Panel C: Simulated Data from Normal Distribution
# Using the sparrow parameters: Mean and SD
set.seed(123) # For reproducibility
Y_sim <- rnorm(nrow(Sparrows), mean = mean(Sparrows$wt), sd = sd(Sparrows$wt)) #rnorm takes random samples from a Normal distribution with a specified mean and standard deviation
hist(Y_sim, nclass = 15, col = "lightblue",
     xlab = "Weight", main = "C: Simulated Normal Data")

# Panel D: Theoretical Normal Probability Curve
X_range <- seq(from = 0, to = 30, length = 200)
Y_dens <- dnorm(X_range, mean = mean(Sparrows$wt), sd = sd(Sparrows$wt)) #dnorm calculates the Normal density curve for a given range of values X and for given mean and variance.

plot(X_range, Y_dens, type = "l", lwd = 2, 
     xlab = "Weight", ylab = "Probability Density", 
     ylim = c(0, 0.25), xlim = c(0, 30), 
     main = "D: Normal Density Curve")
# Add a shaded area to illustrate "Surface adds up to 1"
polygon(c(X_range, rev(X_range)), c(Y_dens, rep(0, length(Y_dens))), 
        col = rgb(0, 0, 1, 0.1), border = NA)

# Reset parameters
par(op)

#see fig 8.2 for code on plotting poisson distributions with various values of the mean
#The function dpois calculates Poisson probabilities for a given μ, and the probability for certain Y-values




###############################################
# Zuur et al. Ch. 9,
# Input: Hawaii.txt
###############################################
Hawaii <- read.table(file = "ZuurDataMixedModelling/Hawaii.txt", header = TRUE, dec = ".")


###############################################
# Zuur et al. Ch. 10,
# Input: Hawaii.txt
###############################################



###############################################
# Zuur et al. Ch. 11,
# Input: Hawaii.txt
###############################################
