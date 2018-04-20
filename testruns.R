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

# Make a plot
plot.new()
par(usr = c(0, length(DemHouseMaj) + n.ahead, -50, 80) )
# make the x-axis
axis(1,
     at = seq(from = 1, to = 31, by = 1),
     labels = 88:118
)
axis(2)

title(xlab = "Session of Congress",
      ylab = "Size of Democratic House Majority",
      main="Predicted effect of Scenario 1")


# Polygon of predictive interval for no law (optional)
x0 <- (length(DemHouseMaj)+1):(length(DemHouseMaj) + n.ahead)
y0 <- c(ypred1$pred - 2*ypred1$se, rev(ypred1$pred + 2*ypred1$se), (ypred1$pred - 2*ypred1$se)[1] )
polygon(x = c(x0, rev(x0), x0[1]),
        y = y0,
        border=NA,
        col="#FFBFBFFF"
)

# Plot the actual data
lines(x = 1:length(DemHouseMaj),
      y = DemHouseMaj
)

# Add the predictions for no law
lines(x = length(DemHouseMaj):(length(DemHouseMaj)+n.ahead),
      y = c(DemHouseMaj[length(DemHouseMaj)],ypred1$pred),  # link up the actual data to the prediction
      col = "red"
)

