data <- read.csv("Data/congress.csv")
attach(data)

library(pander)

plot(Congress, DemHouseMaj, type="l")


##########ARIMA###################

xcovariates <- cbind(PartisanMidterm, PartisanUnem, Coattails, Pre1994)
ar0 <- arima(DemHouseMaj, order = c(0,0,0),
             xreg = xcovariates, include.mean = TRUE)
print(ar0)

####################################

## Estimate an AR(1) using arima
xcovariates <- law
arima.res1a <- arima(death, order = c(1,0,0),
                     xreg = xcovariates, include.mean = TRUE
)
print(arima.res1a)

# Extract estimation results from arima.res1a
pe.1a <- arima.res1a$coef                    # parameter estimates (betas)
se.1a <- sqrt(diag(arima.res1a$var.coef))    # standard errors
ll.1a <- arima.res1a$loglik                  # log likelihood at its maximum
sigma2hat.1a <- arima.res1a$sigma2           # standard error of the regression
aic.1a <- arima.res1a$aic                    # Akaike Information Criterion
resid.1a <- arima.res1a$resid                # residuals

# Attempt at rolling window cross-validation (see caveats)
cv.1a <- arimaCV(death, order=c(1,0,1), forward=forward,
                 xreg=xcovariates, include.mean=TRUE, minper=minper)


table.names <- c("Model Components", "AIC", "\\hat{sigma}^2")

pander(table.names)
