# =============================================================================
# SEM Direct, Indirect, and Total Effects Extraction
# =============================================================================
# Author: Mark Fellhauer
# Created: 2026-01-02
#
# Purpose:
#   This script extracts and formats direct, indirect, and total effects from
#   a fitted SEM mediation model using lavaan. Results include bootstrapped
#   confidence intervals and standardized coefficients for reporting.
#
# Requirements:
#   - A fitted lavaan SEM object named `fit`
# =============================================================================

# -------------------------
# Load required library
# -------------------------
library(lavaan)

# -------------------------
# Extract parameter estimates
# -------------------------
# Includes:
# - Standardized coefficients
# - Bootstrapped 95% confidence intervals
est <- parameterEstimates(
  fit,
  standardized = TRUE,
  ci = TRUE,
  level = 0.95
)

# -------------------------
# 1. Direct Effect: ENT → DEPNDT
# -------------------------
direct <- subset(est, label == "c")

direct_effect <- data.frame(
  effect = direct$est,
  se     = direct$se,
  t      = direct$z,
  p      = direct$pvalue,
  llci   = direct$ci.lower,
  ulci   = direct$ci.upper,
  c_cs   = direct$std.all
)

rownames(direct_effect) <- "Direct effect of ENT on DEPNDT"

# -------------------------
# 2. Indirect Effects via EO and RP
# -------------------------
indirect <- subset(est, label %in% c("indirect_EO", "indirect_RP"))

indirect_effects <- data.frame(
  effect = indirect$est,
  se     = indirect$se,
  t      = indirect$z,
  p      = indirect$pvalue,
  llci   = indirect$ci.lower,
  ulci   = indirect$ci.upper,
  c_cs   = indirect$std.all
)

# Defensive row naming
if (nrow(indirect_effects) == 2) {
  rownames(indirect_effects) <- c(
    "ENT → EO → DEPNDT",
    "ENT → RP → DEPNDT"
  )
} else {
  rownames(indirect_effects) <- indirect$label
}

# -------------------------
# 3. Total Indirect Effect
# -------------------------
total_indirect <- subset(est, label == "total_indirect")

total_indirect_effect <- data.frame(
  effect = total_indirect$est,
  se     = total_indirect$se,
  t      = total_indirect$z,
  p      = total_indirect$pvalue,
  llci   = total_indirect$ci.lower,
  ulci   = total_indirect$ci.upper,
  c_cs   = total_indirect$std.all
)

if (nrow(total_indirect_effect) == 2) {
  rownames(total_indirect_effect) <- c(
    "ENT → EO → DEPNDT",
    "ENT → RP → DEPNDT"
  )
} else {
  rownames(total_indirect_effect) <- "Total Indirect Effect"
}

# -------------------------
# 4. Total Effect: ENT → DEPNDT
# -------------------------
total <- subset(est, label == "total")

total_effect <- data.frame(
  effect = total$est,
  se     = total$se,
  t      = total$z,
  p      = total$pvalue,
  llci   = total$ci.lower,
  ulci   = total$ci.upper,
  c_cs   = total$std.all
)

rownames(total_effect) <- "Total Effect of ENT on DEPNDT"

# -------------------------
# Output results
# -------------------------
direct_effect
indirect_effects
total_indirect_effect
total_effect
