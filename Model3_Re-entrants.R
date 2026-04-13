
# ================================
# 1. SETUP
# ================================
source("scripts/00_setup.R")

library(dplyr)
library(lmtest)
library(sandwich)
library(car)

#  Load modelsummary nếu có, không thì bỏ qua
if (!require(modelsummary)) {
  cat("modelsummary chưa cài → dùng summary() thay thế\n")
}

# ================================
# 2. LOAD DATA
# ================================
df_train <- readRDS("data/processed/df_train.rds")

# ================================
# 3. CLEAN DATA
# ================================
names(df_train) <- make.names(names(df_train), unique = TRUE)

# ================================
# 4. CREATE VARIABLES
# ================================
df_train <- df_train %>%
  mutate(
    age = exp + schooling + 6,
    mismatch_exp = mismatch * exp
  )

# ================================
# 5. FILTER RE-ENTRANTS
# ================================
df_reentrant_sub <- df_train %>%
  filter(
    re_entrant == 1,
    female == 1,
    age >= 30,
    marital ==1,
    exp <= 10
  )

# ================================
# 6. SANITY CHECK
# ================================
cat("Original N:", nrow(df_train), "\n")
cat("Subset N  :", nrow(df_reentrant_sub), "\n")

if (nrow(df_reentrant_sub) == 0) {
  stop(" Subsample is EMPTY → check filter conditions")
}

# ================================
# 7. REMOVE NA
# ================================
df_reentrant_sub <- df_reentrant_sub %>%
  filter(
    !is.na(ln_wage),
    !is.na(mismatch),
    !is.na(exp),
    !is.na(exp2),
    !is.na(edu_level),
    !is.na(urban),
    !is.na(marital),
    !is.na(sector),
    !is.na(industry_sub)
  )

# ================================
# 8. FACTOR VARIABLES
# ================================
df_reentrant_sub <- df_reentrant_sub %>%
  mutate(
    sector = as.factor(sector),
    industry_sub = as.factor(industry_sub)
  )

df_reentrant_sub$sector <- droplevels(df_reentrant_sub$sector)
df_reentrant_sub$industry_sub <- droplevels(df_reentrant_sub$industry_sub)

# ================================
# 9. MODEL
# ================================
f_model <- ln_wage ~ mismatch + exp + exp2 +
  mismatch_exp +
  edu_level + urban + marital +
  sector + industry_sub

# ================================
# 10. RUN REGRESSION
# ================================
model <- lm(f_model, data = df_reentrant_sub)

# ================================
# 11. ROBUST SE
# ================================
robust <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
print(robust)

# ================================
# 12. OUTPUT
# ================================
if ("modelsummary" %in% installed.packages()[,1]) {
  modelsummary(
    model,
    vcov = "HC1",
    stars = TRUE,
    title = "Re-entrants Model (Clean & Stable)"
  )
} else {
  summary(model)
}

# ================================
# 13. NOTE
# ================================
cat("\nNOTE:\n")
cat("- Female = 1 → removed from model.\n")
cat("- Interaction with female removed.\n")
cat("- Model adjusted for subsample analysis.\n")
