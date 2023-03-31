# Load libraries
library(RColorBrewer)
library(rgdal)
library(sp) 
library(spdep)
library(tmap) 

# Load data
insurance.data <- read.csv("C:/Users/levin/Documents/uni/urban_analytics/git/urban_analytics/13_Regression/data/phoenix_insurance_data.csv")

# Preview 
head(insurance.data)

# Perform regression on insurance using unemployment as the independent variable
insurance.model <-  lm(pct_insur ~ pct_unemp, data = insurance.data, na.action=na.exclude)
summary(insurance.model)

plot(insurance.data$pct_insur, insurance.data$pct_unemp, 
     xlab = "% Insured",  
     ylab = "% Unemployed",
     main = "Linear Regression of Population Insured in Phoenix, AZ",
     col = rgb(0, 0, 0, 0.2), pch = 16)
abline(insurance.model, col = "blue")



# MULTIPLE REGRESSION
# Perform regression on insurance using multiple independent variables
insurance.model <-  lm(pct_insur ~ pct_unemp + pct_mar + pct_65over + pct_white + pct_hisp, 
                       data = insurance.data,
                       na.action=na.exclude)
summary(insurance.model)

# remove pct 65 over and pct white since p value very high -> insignificant
# instead add a few other independant variables
# Perform regression on insurance using multiple predictor variables
insurance.model <-  lm(pct_insur ~ pct_unemp + pct_mar + pct_hisp + med_income, 
                       data = insurance.data,
                       na.action=na.exclude)
summary(insurance.model)



# RESIDUAL PLOTS = ERROR TERMS = ACTUAL - PREDICTED -> SHOULD BE RANDOM
# BIG RESIDUALS = MODEL DOESNT BEHAVE SO WELL

# Make a data frame with actual, predicted and residual values
insurance.valid.data <- data.frame(actual = insurance.data$pct_insur, predicted = predict(insurance.model), residual = resid(insurance.model))

head(insurance.valid.data)

# Plot the residuals
plot(insurance.valid.data$predicted, insurance.valid.data$residual, 
     xlab = "Predicted Value of insurance",  
     ylab = "Residual",
     main = "Residual plot of Insurance Model",
     col = rgb(0, 0, 0, 0.2), pch = 16)

abline(0,0, col = "tomato") # Zero baseline
# WHEN INSURANCE LVL LOWER = REGRESSION MODEL HAS ISSUES PREDICTING



# SPACIAL REGRESSION MODELS
# EG DIFFERENCES IN AREAS -> THEY BEHAVE DIFFERENT

# Read Shapefile phoenix_insurance_data
insurance.shp <- readOGR("C:/Users/levin/Documents/uni/urban_analytics/git/urban_analytics/13_Regression/data/", "phoenix_insurance_data")

insurance.shp <- spTransform(insurance.shp, CRS("+proj=longlat +datum=WGS84"))

# Append residuals to data
insurance.shp@data$ols_resid <-  resid(insurance.model)

# Make map of residuals
resid_map <- tm_shape(insurance.shp) + 
             tm_polygons(style="cont", col = "ols_resid", palette="RdBu", n=7, title="OLS Residuals", border.alpha = 0.1) +
             tm_legend(text.size = .8)

resid_map

# CALCULATE MORAN'S I SEE LECTURE
# Contiguity Spatial Matrix
sp.matrix <- poly2nb(insurance.shp, queen=TRUE)

# Calculating the weights matrix. "W" stands for row standardised values
sp.weights <- nb2listw(sp.matrix, style='W', zero.policy=TRUE)

# We can visualise spatial connections with the following map
plot(insurance.shp)
plot(sp.matrix, coordinates(insurance.shp), 
     col='red', lwd=0.5,
     add=TRUE) # this argument plots the second map on top of the first one

# Fix NAs to zero
resid_moran <- insurance.shp@data$ols_resid
resid_moran[is.na(resid_moran)] <- 0

# NULLHYPTHOSE = KEINE SPATIAL EFFEKTE
# HIER P = 0.01 -> 0 HYPO ABGELEHNT -> HABEN SPATIAL EFFECTS
# calculate I
moran.mc(resid_moran, listw = sp.weights, nsim = 999)

# Run the two spatial regression models

# NEED spatialreg PACKAGE SINCE spdeg package is defunct
library(spatialreg)
# Spatial simultaneous autoregressive lag model
insurance.model.lag <- lagsarlm(pct_insur ~ pct_unemp + pct_mar + pct_65over + pct_hisp,
                                data=insurance.shp@data, sp.weights)
summary(insurance.model.lag, Nagelkerke = T)

# Spatial simultaneous autoregressive error model 
insurance.model.error <- errorsarlm(pct_insur ~ pct_unemp + pct_mar + pct_65over + pct_hisp,
                                    data=insurance.shp@data, sp.weights)
summary(insurance.model.error, Nagelkerke = T)

anova(insurance.model.lag, insurance.model.error)
