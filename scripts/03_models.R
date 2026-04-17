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

# ============================================================
# 02_model2.R — Model 2 (Main Model): OLS với exp, exp², female, mismatch×exp
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# ============================================================

library(tidyverse)
library(modelsummary)
library(lmtest)
library(sandwich)
library(car)

# ── 1. LOAD DATA ─────────────────────────────────────────────
# source("scripts/00_setup.R")   # bỏ comment nếu cần

df_train     <- readRDS("data/processed/df_train.rds")
df_test      <- readRDS("data/processed/df_test.rds")
df_male      <- readRDS("data/processed/df_male.rds")
df_reentrant <- readRDS("data/processed/df_reentrant.rds")

train_male      <- df_train |> filter(female == 0)
train_reentrant <- df_train |> filter(re_entrant == 1)

cat("df_train        :", nrow(df_train), "\n")
cat("train_male      :", nrow(train_male), "\n")
cat("train_reentrant :", nrow(train_reentrant), "\n")

# ── 2. FORMULA DEFINITIONS ───────────────────────────────────
f_baseline <- ln_wage ~ mismatch +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

f_exp <- ln_wage ~ mismatch + exp + exp2 + female +
  mismatch_exp +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

f_full <- ln_wage ~ mismatch + exp + exp2 + female +
  mismatch_exp + mismatch_female + exp_female + triple +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

f_sub <- ln_wage ~ mismatch + exp + exp2 +
  mismatch_exp +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

# ── 3. ƯỚC LƯỢNG MODEL 2 (f_exp) ────────────────────────────
# Model 2 = f_exp trên df_train
m2 <- lm(f_exp, data = df_train)

cat("\n========================================\n")
cat("  MODEL 2: OLS — f_exp (Main Model)\n")
cat("========================================\n")
cat("Formula: ln_wage ~ mismatch + exp + exp2 + female\n")
cat("                   + mismatch_exp\n")
cat("                   + edu_level + urban + marital\n")
cat("                   + factor(sector) + factor(industry_sub)\n\n")

# ── 4. ROBUST STANDARD ERRORS (HC3) ─────────────────────────
# Dùng robust SE theo heteroskedasticity-consistent (HC3)
vcov_robust <- vcovHC(m2, type = "HC3")
m2_coeftest <- coeftest(m2, vcov = vcov_robust)

cat("--- Coefficients với Robust SE (HC3) ---\n")
print(m2_coeftest)

# ── 5. KIỂM TRA DẤU VÀ Ý NGHĨA THỐNG KÊ ────────────────────
cat("\n========================================\n")
cat("  KIỂM TRA β THEO YÊU CẦU\n")
cat("========================================\n")

coefs <- coef(m2)
coefs_robust <- m2_coeftest[, "Estimate"]
pvals_robust  <- m2_coeftest[, "Pr(>|t|)"]

# Xác định vị trí từng β trong f_exp:
# β₁ = mismatch  (β₁: penalty mismatch)
# β₂ = exp       (β₂: kinh nghiệm, cần > 0)
# β₃ = exp2      (β₃: exp bình phương, cần < 0 → vòng cung)
# β₄ = female    (β₄: chênh lệch giới)
# β₅ = mismatch_exp (β₅: interaction mismatch×exp, kiểm tra ý nghĩa)

beta2_est  <- coefs["exp"]
beta2_p    <- pvals_robust["exp"]

beta3_est  <- coefs["exp2"]
beta3_p    <- pvals_robust["exp2"]

beta5_est  <- coefs["mismatch_exp"]
beta5_p    <- pvals_robust["mismatch_exp"]
beta5_se   <- m2_coeftest["mismatch_exp", "Std. Error"]
beta5_t    <- m2_coeftest["mismatch_exp", "t value"]

# --- Kiểm tra β₂ > 0 (exp dương) ---
cat("\n[β₂] exp — Hệ số kinh nghiệm:\n")
cat(sprintf("  Ước lượng  : %.5f\n", beta2_est))
cat(sprintf("  p-value    : %.4f\n", beta2_p))
if (beta2_est > 0) {
  cat("  ✓ DẤU ĐÚNG: β₂ > 0 (kinh nghiệm làm tăng lương)\n")
} else {
  cat("  ✗ CẢNH BÁO: β₂ ≤ 0 — ngược chiều kỳ vọng!\n")
}
if (beta2_p < 0.05) {
  cat(sprintf("  ✓ Ý NGHĨA thống kê ở mức 5%% (p = %.4f)\n", beta2_p))
} else {
  cat(sprintf("  ⚠ Không có ý nghĩa thống kê ở 5%% (p = %.4f)\n", beta2_p))
}

# --- Kiểm tra β₃ < 0 (exp² âm → vòng cung) ---
cat("\n[β₃] exp² — Hệ số bình phương kinh nghiệm (vòng cung):\n")
cat(sprintf("  Ước lượng  : %.6f\n", beta3_est))
cat(sprintf("  p-value    : %.4f\n", beta3_p))
if (beta3_est < 0) {
  cat("  ✓ DẤU ĐÚNG: β₃ < 0 (hình vòng cung — Mincer chuẩn)\n")
} else {
  cat("  ✗ CẢNH BÁO: β₃ ≥ 0 — không có hình vòng cung!\n")
}
if (beta3_p < 0.05) {
  cat(sprintf("  ✓ Ý NGHĨA thống kê ở mức 5%% (p = %.4f)\n", beta3_p))
} else {
  cat(sprintf("  ⚠ Không có ý nghĩa thống kê ở 5%% (p = %.4f)\n", beta3_p))
}

# --- Đỉnh vòng cung (peak experience) ---
peak_exp <- -beta2_est / (2 * beta3_est)
cat(sprintf("\n  → Đỉnh vòng cung tại exp ≈ %.1f năm\n", peak_exp))

# --- Kiểm tra β₅ (mismatch_exp) ---
cat("\n[β₅] mismatch_exp — Interaction Mismatch × Kinh nghiệm:\n")
cat(sprintf("  Ước lượng  : %.5f\n", beta5_est))
cat(sprintf("  Robust SE  : %.5f\n", beta5_se))
cat(sprintf("  t-statistic: %.3f\n", beta5_t))
cat(sprintf("  p-value    : %.4f\n", beta5_p))

if (beta5_p < 0.01) {
  cat(sprintf("  ✓ Có ý nghĩa thống kê ở mức 1%% (p = %.4f)\n", beta5_p))
} else if (beta5_p < 0.05) {
  cat(sprintf("  ✓ Có ý nghĩa thống kê ở mức 5%% (p = %.4f)\n", beta5_p))
} else if (beta5_p < 0.10) {
  cat(sprintf("  ~ Có ý nghĩa thống kê ở mức 10%% (p = %.4f)\n", beta5_p))
} else {
  cat(sprintf("  ✗ Không có ý nghĩa thống kê (p = %.4f)\n", beta5_p))
}

if (beta5_est > 0) {
  cat("  → Dấu DƯƠNG: kinh nghiệm giảm dần penalty mismatch\n")
  cat("     (người mismatch càng nhiều năm kinh nghiệm, penalty càng nhỏ)\n")
} else {
  cat("  → Dấu ÂM: kinh nghiệm làm tăng thêm penalty mismatch\n")
}

# ── 6. TÓM TẮT KIỂM TRA ─────────────────────────────────────
cat("\n========================================\n")
cat("  TÓM TẮT KIỂM TRA\n")
cat("========================================\n")

check_b2 <- beta2_est > 0
check_b3 <- beta3_est < 0
check_b5_sig <- beta5_p < 0.05

cat(sprintf("  β₂ > 0 (exp dương)    : %s  (%.5f)\n",
            ifelse(check_b2, "✓ PASS", "✗ FAIL"), beta2_est))
cat(sprintf("  β₃ < 0 (vòng cung)    : %s  (%.6f)\n",
            ifelse(check_b3, "✓ PASS", "✗ FAIL"), beta3_est))
cat(sprintf("  β₅ có ý nghĩa (p<.05) : %s  (p=%.4f)\n",
            ifelse(check_b5_sig, "✓ PASS", "✗ FAIL"), beta5_p))

if (check_b2 && check_b3 && check_b5_sig) {
  cat("\n  ✓ Model 2 vượt qua tất cả kiểm tra — sẵn sàng cho bước tiếp theo.\n")
} else {
  cat("\n  ⚠ Model 2 chưa vượt qua một số kiểm tra — xem lại dữ liệu/filter.\n")
}

# ── 7. MODEL FIT ─────────────────────────────────────────────
cat("\n--- Model Fit ---\n")
cat(sprintf("  R²         : %.4f\n", summary(m2)$r.squared))
cat(sprintf("  Adj. R²    : %.4f\n", summary(m2)$adj.r.squared))
cat(sprintf("  N (train)  : %d\n",   nobs(m2)))

# ── 8. XUẤT KẾT QUẢ ─────────────────────────────────────────
# m2 object sẵn sàng cho modelsummary / bước tiếp theo
cat("\n✓ Object m2 đã được tạo.\n")
cat("  Dùng m2 cho modelsummary, predict, hoặc so sánh với m1/m3.\n")

# In bảng tóm tắt chuẩn với robust SE — dùng "markdown" để tránh lỗi HTML temp file
cat("\n--- modelsummary(m2) với Robust SE ---\n")
modelsummary(
  list("Model 2 (f_exp)" = m2),
  vcov      = "HC3",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_omit = "factor",
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  output    = "markdown"   # hoặc "data.frame" nếu muốn dùng tiếp trong R
)
