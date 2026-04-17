source("scripts/00_setup.R")
library(modelsummary)
library(lmtest)
library(sandwich)
library(car)
library(dplyr)

df_train <- readRDS("data/processed/df_train.rds")

cat("df_train total obs:", nrow(df_train), "\n")

f_baseline <- ln_wage ~ mismatch +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

m1 <- lm(f_baseline, data = df_train)

m1_robust <- coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
print(m1_robust)

if(!dir.exists("models")) dir.create("models")
saveRDS(m1, "models/m1_object.rds")