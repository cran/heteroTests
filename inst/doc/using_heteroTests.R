## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#")

## ----example------------------------------------------------------------------
library(heteroTests)

# Fit a simple linear model on real data
model <- lm(stations ~ mag + depth, data = quakes)
# A second dataset demonstrates collinearity and mild nonlinearity
data(diagnostic_data)
diag_model <- lm(y ~ x1 + x2, data = diagnostic_data)

# Run White's test and get an htest object
performWhiteTest(model, quakes)
performWhiteTest(diag_model, diagnostic_data)

## ----more-tests---------------------------------------------------------------
# Breusch-Pagan and its robust Koenker version. The data argument must be
# the data the model was fitted on.
performBPTest(model, quakes)
performKoenkerTest(model, quakes)

# Group-based tests need a grouping factor in that same data
quakes_grouped <- quakes
quakes_grouped$depth_band <- cut(quakes_grouped$depth, 3,
                                 labels = c("shallow", "mid", "deep"))
performLeveneTest(model, quakes_grouped, "depth_band")

# ARCH effects in time series
performArchLMTest(model, lags = 2)

## ----run-many-----------------------------------------------------------------
# Alternatively run multiple tests at once
runHeteroTests(model, quakes)
# Choose a subset of diagnostics
runHeteroTests(model, quakes, tests = c("white", "koenker", "ncv"))
runDiagnostics(model, quakes)

## ----remediation--------------------------------------------------------------
fitWLS(model)
fitRobust(model)
autoTransform(model)

## ----quakes-example-----------------------------------------------------------
quakes_model <- lm(stations ~ mag + depth, data = quakes)
runDiagnostics(quakes_model, quakes)
fitWLS(quakes_model)

## ----theoph-example-----------------------------------------------------------
data(Theoph, package = "datasets")
dose_model <- lm(conc ~ Time, data = Theoph)
performWhiteTest(dose_model, Theoph)
plot(HeteroDiagnostic(dose_model, Theoph))

## ----step-by-step-------------------------------------------------------------
hd <- HeteroDiagnostic(stations ~ mag + depth, quakes)
test(hd)

wls_model <- fitWLS(hd$model)

# Re-test after remedy
test(HeteroDiagnostic(wls_model, quakes))

## ----validation-example-------------------------------------------------------
# Compare our Breusch-Pagan test with lmtest
library(lmtest)
data(mtcars)
model <- lm(mpg ~ wt + hp, data = mtcars)

# Our implementation
our_result <- performBreuschPaganTest(model, mtcars)

# Reference implementation
ref_result <- bptest(model)

# Results should match
print(paste("Our statistic:", round(our_result$statistic, 6)))
print(paste("Reference statistic:", round(ref_result$statistic, 6)))

## ----performance-tips, eval=FALSE---------------------------------------------
# # Illustrative only: `your_data` stands for your own data frame, so this
# # chunk is not evaluated when the vignette is built.
# 
# # 1. Subset for initial exploration
# large_subset <- your_data[sample(nrow(your_data), 1000), ]
# quick_result <- performWhiteTest(model, large_subset)
# 
# # 2. Use simpler tests first
# fast_result <- performGQTest(model, your_data, order_by = "x1")
# 
# # 3. Monitor memory usage
# gc()  # Garbage collection before analysis
# result <- performWhiteTest(model, your_data)
# gc()  # Clean up after

