library(dplyr)
library(sandwich)
library(lmtest)

df <- readRDS("data/processed/data_processed.rds")

set.seed(123)
train_idx <- sample(1:nrow(df), 0.8 * nrow(df))
train_set <- df[train_idx, ]

m1 <- lm(ln_wage ~ mismatch + exp + exp2 + female + edu_level + 
           urban + marital + factor(sector) + factor(industry_sub), 
         data = train_set)

m1_robust <- coeftest(m1, vcov = vcovHC(m1, type = "HC1"))

print(m1_robust)

if(!dir.exists("models")) dir.create("models")
saveRDS(m1, "models/m1_object.rds")