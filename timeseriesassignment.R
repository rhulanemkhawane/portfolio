rm(list = ls())
#  Removes all objects from the current workspace (R memory)

# Install readxl and stargazer
install.packages("stargazer")
install.packages("readxl")
install.packages("dplyr")

library(readxl)
library(stargazer)
library(dplyr)

setwd("~/Documents/University of Cape Town/Fourth Year/Quantitative Methods in Economics/Time Series Assignment/Dataset")

data <- read_excel("assignment_data.xlsx")

#Creating a  GDP lag variable of 4 in order to calculate the year-on-year growth.

data$gdp_lag4 <- dplyr::lag(data$gdp, 4)

#Calculating the year-on-year growth of GDP using the "gdp" and "gdp_lag4" variables.

data$gdp_growth <- (data$gdp - data$gdp_lag4) / (data$gdp_lag4)

#Creating a new variable called "yield_spread", which takes the difference 
#between the yield on the ten year government bond and the three month 
#government bond.

data$yield_spread <- (data$ten_year - data$three_month)

#Line graph of GDP

plot(y = data$gdp,
     x = data$date,
     type = "l", 
     main = "Graph of South Africa's Quarterly Gross Domestic Product (GDP) from 2001 to 2019.",
     ylab = "Gross Domestic Product in Rand millions",
     xlab = "Year")

#Line graph of growth rate of GDP

plot(y = data$gdp_growth,
     x = data$date,
     type = "l", 
     main = "Graph of South Africa's Gross Domestic Product Growth Rate from 2001 to 2019.",
     ylab = "Gross Domestic Product Growth Rate",
     xlab = "Year")

#Line graph of yield spread

plot(y = data$yield_spread,
     x = data$date,
     type = "l", 
     main = "South Africa's Government Bond Yield Spread from 2001 to 2019.",
     ylab = "Yield Spread (in %)",
     xlab = "Year")

#Creating summary statistics of all given and created variables.

summary(data[,c(2,3,4,5,7,8)])
stargazer(as.data.frame(data[,c(2,3,4,5,7,8)]), type = "text", out = "summary_statistics.html")

#Finding median of all variables in data
 median(data$three_month)
 median(data$five_year)
 median(data$ten_year)
 median(data$gdp)
 median(data$gdp_growth)
 median(data$yield_spread)

#Scatter Plot of relationship between GDP Growth and Yield Spread

data$yield_spread_lag4 <- dplyr::lag(data$yield_spread, 4)
plot(y = data$gdp_growth,
     x = data$yield_spread_lag4,
     main = "Relationship Between GDP Growth(t) and Yield Spread of the Previous Year",
     ylab = "GDP Growth(t)",
     xlab = "Yield Spread(t-4)(in %)")

#Generating an AR(1) model on gdp_growth, but the lag of gdp_growth is 
#needed.

data$gdp_growth_lag <- dplyr::lag(data$gdp_growth)
gdp_growth_ar1 <- lm(gdp_growth ~ gdp_growth_lag, data = data)
stargazer::stargazer(gdp_growth_ar1, type = "text", out = "gdp_growth_ar1.html")


#Generating a lagged model with yield_spread_lag4 as an independent variable

gdp_growth_yield_spread <- lm(gdp_growth ~ yield_spread_lag4, data = data)
stargazer::stargazer(gdp_growth_yield_spread, type = "text", out = "gdp_growth_yield_spread.html")

#Generating a model for gdp_growth with both lag of gdp_growth and
#yield_spread_lag4

gdp_growth_ardl <- lm(gdp_growth ~ gdp_growth_lag + yield_spread_lag4, data = data)
stargazer::stargazer(gdp_growth_ardl, type = "text", out = "gdp_growth_ardl.html")

#Combining all our models into one table for cohesion. 

stargazer::stargazer(gdp_growth_ar1, 
                     gdp_growth_yield_spread, 
                     gdp_growth_ardl, 
                     type = "text", 
                     out = "ar_models.html")
#-------------------------------------------------------------------------------
#Creating a forecast for GDP growth using each model.
#-------------------------------------------------------------------------------
# AR(1) Model

ar1_beta0 <- gdp_growth_ar1$coefficients[1]
ar1_beta0 <- as.numeric(ar1_beta0)

ar1_beta1 <- gdp_growth_ar1$coefficients[2]
ar1_beta1 <- as.numeric(ar1_beta1)

forecast_ar1 <- ar1_beta0 + ar1_beta1*data$gdp_growth[64]
forecast_ar1 <- forecast_ar1*100

#-------------------------------------------------------------------------------
# Model with lagged yield spread

yield_spread_beta0 <- gdp_growth_yield_spread$coefficients[1]
yield_spread_beta0 <- as.numeric(yield_spread_beta0)

yield_spread_beta1 <- gdp_growth_yield_spread$coefficients[2]
yield_spread_beta1 <- as.numeric(yield_spread_beta1)

forecast_yield_spread <- yield_spread_beta0 + yield_spread_beta1*data$yield_spread[64]
forecast_yield_spread <- as.numeric(forecast_yield_spread)
forecast_yield_spread <- forecast_yield_spread*100

#-------------------------------------------------------------------------------
# ARDL Model

ardl_beta0 <- gdp_growth_ardl$coefficients[1]
ardl_beta0 <- as.numeric(ardl_beta0)

ardl_beta1 <- gdp_growth_ardl$coefficients[2]
ardl_beta1 <- as.numeric(ardl_beta1)

ardl_beta2 <- gdp_growth_ardl$coefficients[3]
ardl_beta2 <- as.numeric(ardl_beta2)

forecast_ardl <- (ardl_beta0 + ardl_beta1*data$gdp_growth[64] + 
                          ardl_beta2*data$yield_spread[64])
forecast_ardl <- forecast_ardl*100
#-------------------------------------------------------------------------------

