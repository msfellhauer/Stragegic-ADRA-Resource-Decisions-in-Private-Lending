# =============================================================================
# Regression, Correlation, and Mediation Analysis
# =============================================================================
# Author: Mark Fellhauer
# Created: 2026-01-02
#
# Purpose:
#   This script performs regression, correlation, and mediation analyses
#   to test hypothesized relationships among EO, RP, ENT, BIZDEG, and DEPNDT.
#   Analyses include:
#     - Linear regression (H1–H5)
#     - Correlation matrices
#     - Mediation models using lavaan
#     - Model diagnostics and summary outputs
#
# Assumptions:
#   - Data objects (data, TRUNC_ENT, TRUNC_BIZDEG, UNION_Recode_2272025)
#     are already loaded and cleaned.
# =============================================================================

# -------------------------
# Libraries
# -------------------------
library(lavaan)
library(flextable)
library(officer)
library(psych)
library(ggpubr)
library(factoextra)
library(tidyverse)
library(ggplot2)

# =============================================================================
# H1: Direct Effects (Regression)
# =============================================================================
ModelH1A <- lm(DEPNDT ~ ENT, data = data)
ModelH1B <- lm(DEPNDT ~ BIZDEG, data = data)

summary(ModelH1A)
summary(ModelH1B)

# Export AIC / BIC for Model H1A
stats <- data.frame(
  Statistic = c("AIC", "BIC"),
  Value = round(c(AIC(ModelH1A), BIC(ModelH1A)), 3)
)

ft <- flextable(stats) |> theme_vanilla() |> autofit()

doc <- read_docx() |>
  body_add_par("Regression Model Summary (H1A)", style = "heading 1") |>
  body_add_flextable(ft)

print(doc, target = "Model_H1A_Summary.docx")

# =============================================================================
# H2 & H3: Correlation Analyses
# =============================================================================
CorrelationH2A <- cor(
  TRUNC_ENT[, c("DEPNDT", "RP1", "RP2", "RP5", "RP6", "RP7", "RP8")],
  method = "pearson",
  use = "pairwise.complete.obs"
)

CorrelationH2B <- cor(
  TRUNC_BIZDEG[, c("DEPNDT", "RP1", "RP2", "RP5", "RP6", "RP7", "RP8")],
  method = "pearson",
  use = "pairwise.complete.obs"
)

CorrelationH3A <- cor(
  TRUNC_ENT[, c("DEPNDT", "EO1", "EO2", "E3", "EO4", "EO5", "EO6", "EO7", "EO8", "EO9", "EO10")],
  method = "pearson",
  use = "pairwise.complete.obs"
)

CorrelationH3B <- cor(
  TRUNC_BIZDEG[, c("DEPNDT", "EO1", "EO2", "E3", "EO4", "EO5", "EO6", "EO7", "EO8", "EO9", "EO10")],
  method = "pearson",
  use = "pairwise.complete.obs"
)

# =============================================================================
# H4: EO → ENT / BIZDEG
# =============================================================================
data$EO <- rowMeans(
  data[, c("EO1", "EO2", "E3", "EO4", "EO5", "EO6", "EO7", "EO8", "EO9", "EO10")],
  na.rm = TRUE
)

ModelH4A <- lm(ENT ~ EO, data = data)
ModelH4B <- lm(BIZDEG ~ EO, data = data)

summary(ModelH4A)
summary(ModelH4B)

# =============================================================================
# H5: RP → ENT / BIZDEG
# =============================================================================
data$RP <- rowMeans(
  data[, c("RP1", "RP2", "RP5", "RP6", "RP7", "RP8")],
  na.rm = TRUE
)

ModelH5A <- lm(ENT ~ RP, data = data)
ModelH5B <- lm(BIZDEG ~ RP, data = data)

summary(ModelH5A)
summary(ModelH5B)

# =============================================================================
# Mediation Analysis (lavaan)
# =============================================================================
mediation_model <- '
  # Direct effect
  DEPNDT ~ c*ENT

  # Mediators
  EO ~ a1*ENT
  RP ~ a2*ENT
  DEPNDT ~ b1*EO + b2*RP

  # Indirect effects
  ind1 := a1 * b1
  ind2 := a2 * b2
  total := c + ind1 + ind2
'

fit <- sem(
  mediation_model,
  data = data,
  se = "bootstrap",
  bootstrap = 5000
)

summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# =============================================================================
# Data Diagnostics
# =============================================================================
# KMO and Bartlett Tests
selected_vars <- c("EO", "RP", "ENT", "DEPNDT")

kmo_result <- KMO(cor(data[, selected_vars], use = "pairwise.complete.obs"))
bartlett_result <- cortest.bartlett(
  cor(data[, selected_vars], use = "pairwise.complete.obs"),
  n = nrow(data)
)

print(kmo_result)
print(bartlett_result)

# =============================================================================
# Visualizations
# =============================================================================
ggboxplot(data, x = "RP", y = "DEPNDT", add = "jitter")
ggboxplot(data, x = "EO", y = "DEPNDT", add = "jitter")
ggboxplot(data, x = "ENT", y = "DEPNDT", add = "jitter")

ggplot(UNION_Recode_2272025, aes(x = TENURE)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Employee Tenure Distribution", x = "Years with Firm", y = "Frequency") +
  theme_minimal()

# =============================================================================
# Normality Checks (Z-tests)
# =============================================================================
vars <- c("DEPNDT", "EO", "RP", "TENURE", "EDU", "FRMSIZ")
pop_sd <- 1

for (var in vars) {
  x <- na.omit(UNION_Recode_2272025[[var]])
  z <- mean(x) / (pop_sd / sqrt(length(x)))
  p <- 2 * (1 - pnorm(abs(z)))
  cat(paste0("Z-test for ", var, ": Z = ", round(z, 3), ", p = ", round(p, 4), "\n"))
}
