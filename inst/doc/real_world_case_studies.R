## ----setup, include=FALSE-----------------------------------------------------
library(heteroTests)
library(ggplot2)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# The tests log each run at the INFO level; this guide shows their results.
heteroTests::ht_set_log_level("WARN")

## -----------------------------------------------------------------------------
quakes_model <- lm(stations ~ mag + depth, data = quakes)
summary(quakes_model)

## -----------------------------------------------------------------------------
white_q <- performWhiteTest(quakes_model, quakes)
bp_q <- performBPTest(quakes_model, quakes)
koenker_q <- performKoenkerTest(quakes_model, quakes)
list(White = white_q, Breusch_Pagan = bp_q, Koenker = koenker_q)

## ----fig.width=6, fig.height=4------------------------------------------------
plot_data <- data.frame(
  fitted = fitted(quakes_model),
  residual = resid(quakes_model),
  mag = quakes$mag
)

ggplot(plot_data, aes(x = fitted, y = residual, colour = mag)) +
  geom_point(alpha = 0.7) +
  scale_colour_viridis_c(option = "C") +
  geom_smooth(se = FALSE, colour = "black") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    colour = "mag",
    title = "Heteroscedasticity in reported station counts"
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------
wls_q <- fitWLS(quakes_model)

# Compare like with like: the residual variance across thirds of the fitted
# range, unweighted for OLS and weighted for WLS.
third <- cut(fitted(quakes_model), 3, labels = c("low", "mid", "high"))
rbind(
  OLS_resid_var = round(tapply(residuals(quakes_model)^2, third, mean), 1),
  WLS_weighted_resid_var = round(
    tapply((residuals(wls_q) * sqrt(weights(wls_q)))^2, third, mean), 3))

## -----------------------------------------------------------------------------
data(hetero_data, package = "heteroTests")
survey_model <- lm(y ~ x, data = hetero_data)
summary(survey_model)

## -----------------------------------------------------------------------------
survey_diag <- HeteroDiagnostic(survey_model, hetero_data)
runDiagnostics(survey_diag$model, survey_diag$data, tests = c("white", "breusch_pagan"))
performGlejserTest(survey_model, hetero_data, "x")

## ----fig.width=6, fig.height=4------------------------------------------------
plot(survey_diag, plots = c("spread_level"))

## -----------------------------------------------------------------------------
data(diagnostic_data, package = "heteroTests")
diagnostic_ext <- transform(diagnostic_data, x1_sq = x1^2)
engineering_model <- lm(y ~ x1 + x1_sq + x2, data = diagnostic_ext)
engineer_results <- runDiagnostics(engineering_model, diagnostic_ext,
  tests = c("breusch_pagan", "koenker")
)
safe_htest <- function(fn, method) {
  tryCatch(
    fn(),
    error = function(e) {
      message(method, " unavailable: ", e$message)
      structure(
        list(
          statistic = c(statistic = NA_real_),
          parameter = NA_real_,
          p.value = NA_real_,
          method = paste0(method, " (failed)"),
          data.name = deparse(stats::formula(engineering_model))
        ),
        class = "htest"
      )
    }
  )
}
white_engineering <- safe_htest(
  function() performWhiteTest(engineering_model, diagnostic_ext, cross_products = FALSE),
  "White test"
)
bp_engineering <- safe_htest(
  function() performBPTest(engineering_model, diagnostic_ext),
  "Breusch-Pagan test"
)
koenker_engineering <- safe_htest(
  function() performKoenkerTest(engineering_model, diagnostic_ext),
  "Koenker test"
)
harvey_engineering <- safe_htest(
  function() performHarveyTest(engineering_model),
  "Harvey test"
)
engineer_tests <- list(
  white = white_engineering,
  breusch_pagan = bp_engineering,
  koenker = koenker_engineering,
  harvey = harvey_engineering
)
engineer_tests

## -----------------------------------------------------------------------------
engineer_results$vif
engineer_results$reset

## ----fig.width=6, fig.height=4------------------------------------------------
pvals <- sapply(engineer_tests, function(x) x$p.value)
pval_df <- data.frame(
  test = names(pvals),
  p_value = as.numeric(pvals)
)

ggplot(pval_df, aes(x = reorder(test, p_value), y = p_value)) +
  geom_col(fill = "#0072B2", alpha = 0.8) +
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "#D55E00") +
  coord_cartesian(ylim = c(0, 1)) +
  coord_flip() +
  labs(
    x = "Test",
    y = "p-value",
    title = "Contrasting heteroscedasticity diagnostics"
  ) +
  theme_minimal()

