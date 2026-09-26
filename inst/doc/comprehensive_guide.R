## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(heteroTests)
library(ggplot2)

## ----example-finance, echo=TRUE-----------------------------------------------
set.seed(1)
x <- rnorm(250)
y <- 0.5 + 0.3 * x + rnorm(250, sd = abs(x))
dat <- data.frame(x, y)
mod <- lm(y ~ x, dat)
performWhiteTest(mod, dat)

## ----example-cross-sectional, echo=TRUE---------------------------------------
data(mtcars)
fit <- lm(mpg ~ wt + hp, data = mtcars)
performBPTest(fit, mtcars)

## ----example-panel, echo=TRUE-------------------------------------------------
dat <- simulate_hetero(n = 250, beta0 = 1, beta1 = 2, sigma_func = sigma_linear)
fit <- lm(y ~ x, data = dat)
performSzroeterTest(fit, dat, order_by = "x")

