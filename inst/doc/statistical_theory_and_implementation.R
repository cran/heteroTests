## ----setup, include=FALSE-----------------------------------------------------
library(heteroTests)
library(ggplot2)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
model <- lm(stations ~ mag + depth, data = quakes)
summary(model)

## ----fig.width=6, fig.height=4------------------------------------------------
augmented <- data.frame(
  fitted = fitted(model),
  residuals = resid(model)
)
augmented$squared_residuals <- augmented$residuals^2

ggplot(augmented, aes(x = fitted, y = squared_residuals)) +
  geom_point(alpha = 0.6, colour = "#0072B2") +
  geom_smooth(se = FALSE, colour = "#D55E00") +
  labs(
    x = "Fitted values",
    y = expression(hat(e)^2),
    title = "Residual dispersion across fitted values"
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------
white_result <- performWhiteTest(model, quakes)
white_result

## -----------------------------------------------------------------------------
bp_result <- performBPTest(model, quakes)
bp_result

## -----------------------------------------------------------------------------
koenker_result <- performKoenkerTest(model, quakes)
koenker_result

## -----------------------------------------------------------------------------
park_result <- performParkTest(model, quakes, "mag")
harvey_result <- performHarveyTest(model)
list(Park = park_result, Harvey = harvey_result)

## ----fig.width=6, fig.height=4------------------------------------------------
augmented$scaled_residuals <- scale(augmented$residuals)[, 1]
augmented$mag <- quakes$mag

ggplot(augmented, aes(x = mag, y = scaled_residuals)) +
  geom_point(alpha = 0.6, colour = "#56B4E9") +
  geom_smooth(method = "loess", se = FALSE, colour = "#009E73") +
  labs(
    x = "Event magnitude (mag)",
    y = "Scaled residual",
    title = "Relationship between residual spread and event magnitude"
  ) +
  theme_minimal()

