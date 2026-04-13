# ============================================================
# 03_models.R — OLS Regression Models
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# Input:  data/processed/df_train.rds  (main estimation)
#         data/processed/df_test.rds   (out-of-sample eval)
#         data/processed/df_male.rds
#         data/processed/df_female.rds
# Output: output/tables/Table_2_regression.html
#         data/processed/models_list.rds
# ============================================================
# --- 03_models.R ---
# scripts/03_models.R

# 1. Load dữ liệu và thư viện
source("scripts/00_setup.R") 
library(tidyverse)
library(lmtest)
library(sandwich)

# Sử dụng file data_processed.rds bạn đang có để tạo df_train
full_df <- readRDS("data/processed/data_processed.rds")
set.seed(123)
train_idx <- sample(1:nrow(full_df), 0.8 * nrow(full_df))
df_train  <- full_df[train_idx, ]

# 2. Tạo biến bổ trợ
df_train <- df_train %>%
  mutate(
    exp2 = exp^2,
    mismatch_exp = mismatch * exp
  )

# 3. Chạy model và lưu vào object m2 (Đây là yêu cầu chính của bạn)
f_exp <- ln_wage ~ mismatch + exp + exp2 + female + mismatch_exp +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

m2 <- lm(f_exp, data = df_train)

# 4. Kiểm tra kết quả trong Console
summary(m2)

# (Tùy chọn) Lưu object này thành file vật lý để nộp hoặc dùng cho script sau
saveRDS(m2, "data/processed/m2_object.rds")