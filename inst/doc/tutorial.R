## ----setup, include=FALSE-----------------------------------------------------
library(heteroTests)
# The tests log each run at the INFO level; this guide shows their results.
heteroTests::ht_set_log_level("WARN")

## -----------------------------------------------------------------------------
head(quakes)

## -----------------------------------------------------------------------------
base_model <- lm(stations ~ mag + depth, data = quakes)
summary(base_model)

## -----------------------------------------------------------------------------
hd <- HeteroDiagnostic(base_model, quakes)
runDiagnostics(hd$model, hd$data)
plot(hd)

## -----------------------------------------------------------------------------
wls_model <- fitWLS(base_model)
summary(wls_model)

## -----------------------------------------------------------------------------
compareModelDiagnostics(list(base_model, wls_model))

## -----------------------------------------------------------------------------
performVarianceFormTest(wls_model)

