################################################################################
# Statistical Analysis Pipeline
# Descriptive Statistics, Hypothesis Testing, Factor Tests, and Mediation SEM
################################################################################

########################################
# Package Installation & Loading
########################################

required_packages <- c(
  "psych", "ggpubr", "factoextra", "parameters",
  "lavaan", "officer", "flextable", "tidyverse"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg)
}

library(psych)
library(ggpubr)
library(factoextra)
library(parameters)
library(lavaan)
library(officer)
library(flextable)
library(tidyverse)

########################################
# Data Loading
########################################

# Replace with your actual dataset
data <- UNION_Recode_2272025

########################################
# 1. T-Tests and ANOVA
########################################

# Attempted T-tests
t_test_rp  <- t.test(DEPNDT ~ RP, data = data)
t_test_eo  <- t.test(DEPNDT ~ EO, data = data)
t_test_ent <- t.test(DEPNDT ~ ENT, data = data)

print(t_test_rp)
print(t_test_eo)
print(t_test_ent)

# ANOVA where grouping > 2
anova_rp <- aov(DEPNDT ~ RP, data = data)
anova_eo <- aov(DEPNDT ~ EO, data = data)

summary(anova_rp)
summary(anova_eo)

# Visualizations
ggboxplot(data, x = "RP",  y = "DEPNDT", add = "jitter")
ggboxplot(data, x = "EO",  y = "DEPNDT", add = "jitter")
ggboxplot(data, x = "ENT", y = "DEPNDT", add = "jitter")

########################################
# 2. Export ANOVA Results to Word
########################################

export_anova <- function(model, file_name) {

  anova_table <- as.data.frame(summary(model)[[1]])
  colnames(anova_table) <- c("Df", "Sum Sq", "Mean Sq", "F Value", "P Value")

  anova_flex <- flextable(anova_table) %>%
    theme_vanilla() %>%
    autofit() %>%
    bold(part = "header") %>%
    align(align = "center", part = "all")

  doc <- read_docx() %>%
    body_add_par("ANOVA Results", style = "heading 1") %>%
    body_add_flextable(anova_flex)

  print(doc, target = file_name)
}

export_anova(anova_rp, "ANOVA_Results_RP.docx")
export_anova(anova_eo, "ANOVA_Results_EO.docx")

########################################
# 3. KMO and Bartlett’s Tests
########################################

selected_vars <- c("EO", "RP", "ENT", "DEPNDT")

kmo_result <- KMO(cor(data[, selected_vars], use = "pairwise.complete.obs"))
print(kmo_result)

bartlett_result <- cortest.bartlett(
  cor(data[, selected_vars], use = "pairwise.complete.obs"),
  n = nrow(data)
)
print(bartlett_result)

#################
