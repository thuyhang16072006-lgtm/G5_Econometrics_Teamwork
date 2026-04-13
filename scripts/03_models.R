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

# ------------------------------------------------------------
# 1. SETUP & DATA LOADING
# ------------------------------------------------------------
source("scripts/00_setup.R")
library(modelsummary)
library(lmtest)
library(sandwich)
library(car)

# Kiểm tra và tự tạo dữ liệu nếu thiếu
if (!file.exists("data/processed/df_train.rds")) {
  full_data <- readRDS("data/processed/data_processed.rds")
  set.seed(123)
  train_idx <- sample(1:nrow(full_data), 0.8 * nrow(full_data))
  
  df_train <- full_data[train_idx, ]
  df_test  <- full_data[-train_idx, ]
  
  saveRDS(df_train, "data/processed/df_train.rds")
  saveRDS(df_test, "data/processed/df_test.rds")
  saveRDS(df_train |> filter(female == 0), "data/processed/df_male.rds")
}

# Load data chính xác theo yêu cầu của bạn
df_train <- readRDS("data/processed/df_train.rds")

# TẠO CÁC BIẾN TƯƠNG TÁC (Bắt buộc để chạy Formula)
df_train <- df_train |> 
  mutate(
    exp2            = exp^2,
    mismatch_exp    = mismatch * exp,
    mismatch_female = mismatch * female,
    exp_female      = exp * female,
    triple          = mismatch * exp * female
  )
# ------------------------------------------------------------
# 2. MODEL FORMULAS
# ------------------------------------------------------------

# Baseline: Chỉ có mismatch và kiểm soát cơ bản
f_baseline <- ln_wage ~ mismatch + edu_level + urban + marital + 
  factor(sector) + factor(industry_sub)

# Model 2: Thêm Exp, Exp2 và Tương tác Mismatch*Exp
f_exp <- ln_wage ~ mismatch + exp + exp2 + female + mismatch_exp +
  edu_level + urban + marital + 
  factor(sector) + factor(industry_sub)

# Full Model: Thêm các tương tác với giới tính (Gender effects)
f_full <- ln_wage ~ mismatch + exp + exp2 + female + 
  mismatch_exp + mismatch_female + exp_female + triple +
  edu_level + urban + marital + 
  factor(sector) + factor(industry_sub)

# Sub Model (Dành cho phân tích nhóm nhỏ, ví dụ: chỉ Nam)
f_sub <- ln_wage ~ mismatch + exp + exp2 + mismatch_exp +
  edu_level + urban + marital + 
  factor(sector) + factor(industry_sub)

# ------------------------------------------------------------
# 3. ESTIMATION (Ước lượng)
# ------------------------------------------------------------

m1 <- lm(f_baseline, data = df_train)
m2 <- lm(f_exp, data = df_train)      # Đây là object m2 bạn cần
m3 <- lm(f_full, data = df_train)

# Lưu object m2 lại theo yêu cầu
saveRDS(m2, "data/processed/m2_object.rds")
# Kiểm định với Robust SE
m2_robust <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
print(m2_robust[c("exp", "exp2", "mismatch_exp"), ])

# Giải thích về Beta 5 (mismatch_exp):
# - Nếu p < 0.05: Có ý nghĩa thống kê (mức 5%).
# - Nếu 0.05 < p < 0.10: Có ý nghĩa ở mức 10% (Tạm đạt/Xu hướng).
# - Nếu p > 0.10: Không có ý nghĩa thống kê.