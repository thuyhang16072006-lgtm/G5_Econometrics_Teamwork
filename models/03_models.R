source("scripts/00_setup.R")
library(modelsummary)
library(lmtest)
library(sandwich)
library(car)
library(dplyr)

df_train     <- readRDS("data/processed/df_train.rds")
df_test      <- readRDS("data/processed/df_test.rds")
df_male      <- readRDS("data/processed/df_male.rds")
df_reentrant <- readRDS("data/processed/df_reentrant.rds")

train_male      <- df_train |> filter(female == 0)
train_reentrant <- df_train |> filter(re_entrant == 1)

cat("df_train        :", nrow(df_train), "\n")
cat("train_male      :", nrow(train_male), "\n")
cat("train_reentrant :", nrow(train_reentrant), "\n")

f_baseline <- ln_wage ~ mismatch + 
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

m1 <- lm(f_baseline, data = df_train)

m1_robust <- coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
print(m1_robust)

if(!dir.exists("models")) dir.create("models")
saveRDS(m1, "models/m1_object.rds")