## ----setup, include=FALSE-----------------------------------------------------
library(heteroTests)

## -----------------------------------------------------------------------------
head(quakes)

## -----------------------------------------------------------------------------
base_model <- lm(stations ~ mag + depth, data = quakes)
summary(base_model)

## -----------------------------------------------------------------------------
hd <- HeteroDiagnostic(base_model, quakes)
test(hd)
plot(hd)

## -----------------------------------------------------------------------------
wls_model <- fitWLS(base_model)
summary(wls_model)

## -----------------------------------------------------------------------------
compareModelDiagnostics(list(base_model, wls_model))

