
# ============================================================
# 03_models.R — OLS Regression Models + Diagnostics
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# Pipeline: 01_clean.R → 02_eda.R → [03_models.R] → 04_margins.R → 05_robust.R
#
# Nội dung:
#   M1  — Baseline (không có exp, female)
#   M2  — +Exp (kiểm tra remedy qua β₅)
#   M3  — Full model (triple interaction)
#   S1  — Male subsample
#   S2  — Re-entrant subsample
#   D   — Diagnostic tests (VIF, BP, residual plots)
#   OUT — Bảng tổng hợp modelsummary + so sánh β₁/β₅
# ============================================================

source("scripts/00_setup.R")
library(modelsummary)
library(lmtest)
library(sandwich)
library(car)
library(ggplot2)
library(dplyr)

# ── 1. LOAD DATA ──────────────────────────────────────────────
# Tất cả data đã clean từ 01_clean.R — không cần NA check thêm
df_train     <- readRDS("data/processed/df_train.rds")
df_test      <- readRDS("data/processed/df_test.rds")

# Subsamples từ df_train, df_test (đã split, tránh data leakage)
train_male      <- df_train |> filter(female == 0)
train_reentrant <- df_train |> filter(re_entrant == 1)
test_male <- df_test |> filter(female==0)
test_reentrant <- df_test |> filter(re_entrant==1)

cat("=== Sample sizes ===\n")
cat(sprintf("df_train        : %d\n", nrow(df_train)))
cat(sprintf("df_test         : %d\n", nrow(df_test)))
cat(sprintf("train_male      : %d\n", nrow(train_male)))
cat(sprintf("train_reentrant : %d\n", nrow(train_reentrant)))

if (nrow(train_reentrant) == 0)
  stop("train_reentrant EMPTY — kiểm tra lại biến re_entrant trong 01_clean.R")

# ── 2. FORMULAS ───────────────────────────────────────────────
# M1: baseline — xác nhận wage penalty tồn tại
f_baseline <- ln_wage ~ mismatch +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

# M2: +exp — kiểm tra β₅ (remedy?)
f_exp <- ln_wage ~ mismatch + exp + exp2 + female +
  mismatch_exp +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

# M3: full — model chính, triple interaction
f_full <- ln_wage ~ mismatch + exp + exp2 + female +
  mismatch_exp + mismatch_female + exp_female + triple +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

# S1/S2: subsample — không có female (đã đồng nhất trong nhóm)
f_sub <- ln_wage ~ mismatch + exp + exp2 +
  mismatch_exp +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

# ════════════════════════════════════════════════════════════
# M1 — BASELINE
# Mục đích: xác nhận wage penalty tồn tại
# Không có exp, female
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   M1 — BASELINE                    \n")
cat("====================================\n")

m1 <- lm(f_baseline, data = df_train)

cat(sprintf("  R²      : %.4f\n", summary(m1)$r.squared))
cat(sprintf("  Adj. R² : %.4f\n", summary(m1)$adj.r.squared))
cat(sprintf("  N       : %d\n",   nobs(m1)))

m1_robust <- coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
b1   <- coef(m1)["mismatch"]
b1_p <- m1_robust["mismatch", "Pr(>|t|)"]

cat(sprintf("  β₁ (mismatch) : %.4f (p=%.4f)\n", b1, b1_p))
cat(sprintf("  Wage penalty  : %.1f%%\n", (exp(b1) - 1) * 100))

if (b1 < 0 & b1_p < 0.05) {
  cat("  ✓ Wage penalty tồn tại và có ý nghĩa thống kê\n")
} else if (b1 < 0 & b1_p >= 0.05) {
  cat("  ⚠ Dấu đúng nhưng không có ý nghĩa thống kê\n")
} else {
  cat("  ✗ Kết quả không như kỳ vọng\n")
}

saveRDS(m1, "data/processed/m1.rds")
cat("  ✓ m1.rds xuất xong\n")

# ════════════════════════════════════════════════════════════
# M2 — +EXP
# Mục đích: kiểm tra β₅ — exp có remedy không?
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   M2 — +EXP                        \n")
cat("====================================\n")

m2 <- lm(f_exp, data = df_train)

cat(sprintf("  R²      : %.4f\n", summary(m2)$r.squared))
cat(sprintf("  Adj. R² : %.4f\n", summary(m2)$adj.r.squared))
cat(sprintf("  N       : %d\n",   nobs(m2)))

m2_robust <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))

beta2_est <- coef(m2)["exp"]
beta3_est <- coef(m2)["exp2"]
beta5_est <- coef(m2)["mismatch_exp"]
beta2_p   <- m2_robust["exp",          "Pr(>|t|)"]
beta3_p   <- m2_robust["exp2",         "Pr(>|t|)"]
beta5_p   <- m2_robust["mismatch_exp", "Pr(>|t|)"]
beta5_se  <- m2_robust["mismatch_exp", "Std. Error"]
beta5_t   <- m2_robust["mismatch_exp", "t value"]

peak_exp <- -beta2_est / (2 * beta3_est)

cat(sprintf("  β₂ (exp)       : %.5f (p=%.4f) %s\n",
            beta2_est, beta2_p, ifelse(beta2_est > 0, "✓", "✗")))
cat(sprintf("  β₃ (exp²)      : %.6f (p=%.4f) %s\n",
            beta3_est, beta3_p, ifelse(beta3_est < 0, "✓ vòng cung", "✗")))
cat(sprintf("  Đỉnh vòng cung : %.1f năm\n", peak_exp))
cat(sprintf("  β₅ (mis×exp)   : %.5f (SE=%.5f, t=%.3f, p=%.4f)\n",
            beta5_est, beta5_se, beta5_t, beta5_p))

if (beta5_p < 0.1) {
  cat(sprintf("  → Có ý nghĩa (p=%.4f): %s\n", beta5_p,
              ifelse(beta5_est > 0, "REMEDY", "WIDEN")))
} else {
  cat(sprintf("  → Không có ý nghĩa (p=%.4f): penalty KHÔNG đổi theo exp\n",
              beta5_p))
}

if (beta2_est > 0 && beta3_est < 0) {
  cat("  ✓ Mincer equation đúng chuẩn\n")
} else {
  cat("  ✗ Kiểm tra lại dữ liệu\n")
}

saveRDS(m2, "data/processed/m2.rds")
cat("  ✓ m2.rds xuất xong\n")

# ════════════════════════════════════════════════════════════
# M3 — FULL MODEL
# Mục đích: model chính, triple interaction
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   M3 — FULL MODEL                  \n")
cat("====================================\n")

m3 <- lm(f_full, data = df_train)

cat(sprintf("  R²      : %.4f\n", summary(m3)$r.squared))
cat(sprintf("  Adj. R² : %.4f\n", summary(m3)$adj.r.squared))
cat(sprintf("  N       : %d\n",   nobs(m3)))

m3_robust <- coeftest(m3, vcov = vcovHC(m3, type = "HC1"))

key_coefs <- c("mismatch", "exp", "exp2", "female",
               "mismatch_exp", "mismatch_female", "exp_female", "triple")
cat("\n  Hệ số chính (Robust SE HC1):\n")
for (v in key_coefs) {
  if (v %in% rownames(m3_robust)) {
    est  <- m3_robust[v, "Estimate"]
    se   <- m3_robust[v, "Std. Error"]
    pval <- m3_robust[v, "Pr(>|t|)"]
    sig  <- ifelse(pval < 0.01, "***",
                   ifelse(pval < 0.05, "**",
                          ifelse(pval < 0.1,  "*", "")))
    cat(sprintf("  %-20s: %+7.4f (SE=%6.4f) %s\n", v, est, se, sig))
  }
}

# ── Train/Test evaluation ─────────────────────────────────────
pred_test <- predict(m3, newdata = df_test)
valid_idx <- !is.na(pred_test) & !is.na(df_test$ln_wage)
r2_train  <- summary(m3)$r.squared
r2_test   <- cor(df_test$ln_wage[valid_idx], pred_test[valid_idx])^2
rmse_test <- sqrt(mean((df_test$ln_wage[valid_idx] - pred_test[valid_idx])^2))

cat(sprintf("\n  R² train : %.4f\n", r2_train))
cat(sprintf("  R² test  : %.4f\n",  r2_test))
cat(sprintf("  RMSE test: %.4f\n",  rmse_test))
cat(sprintf("  Gap R²   : %.4f %s\n", r2_train - r2_test,
            ifelse(r2_train - r2_test > 0.05,
                   "⚠ hơi cao — ghi chú trong báo cáo", "✓ ổn định")))

saveRDS(m3, "data/processed/m3.rds")
)
cat("  ✓ m3.rds xuất xong\n")

# ════════════════════════════════════════════════════════════
# S1 — MALE SUBSAMPLE
# Mục đích: baseline không có career gap
# f_sub: không có female (đồng nhất trong nhóm)
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   S1 — MALE SUBSAMPLE              \n")
cat("====================================\n")

m3_male <- lm(f_sub, data = train_male)

cat(sprintf("  R²      : %.4f\n", summary(m3_male)$r.squared))
cat(sprintf("  N       : %d\n",   nobs(m3_male)))

m3_male_robust <- coeftest(m3_male, vcov = vcovHC(m3_male, type = "HC1"))
cat(sprintf("  β₁ (mismatch)    : %+.4f (p=%.4f)\n",
            m3_male_robust["mismatch",     "Estimate"],
            m3_male_robust["mismatch",     "Pr(>|t|)"]))
cat(sprintf("  β₅ (mismatch_exp): %+.4f (p=%.4f)\n",
            m3_male_robust["mismatch_exp", "Estimate"],
            m3_male_robust["mismatch_exp", "Pr(>|t|)"]))

saveRDS(m3_male, "data/processed/m3_male.rds")
cat("  ✓ m3_male.rds xuất xong\n")

# ════════════════════════════════════════════════════════════
# S2 — RE-ENTRANT SUBSAMPLE
# Mục đích: nhóm phụ nữ quay lại sau career break
# Lưu ý:
#   - Dùng train_reentrant (re_entrant==1 trong df_train)
#   - KHÔNG filter thêm marital==1: re_entrant đã bao gồm
#     C9 %in% c(2,3,4) từ 01_clean.R, không chỉ đang kết hôn
#   - KHÔNG NA check: df_train đã clean từ 01_clean.R
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   S2 — RE-ENTRANT SUBSAMPLE        \n")
cat("====================================\n")

m3_reentrant <- lm(f_sub, data = train_reentrant)

cat(sprintf("  R²      : %.4f\n", summary(m3_reentrant)$r.squared))
cat(sprintf("  N       : %d (cẩn thận — nhỏ)\n", nobs(m3_reentrant)))

m3_re_robust <- coeftest(m3_reentrant, vcov = vcovHC(m3_reentrant, type = "HC1"))
cat(sprintf("  β₁ (mismatch)    : %+.4f (p=%.4f)\n",
            m3_re_robust["mismatch",     "Estimate"],
            m3_re_robust["mismatch",     "Pr(>|t|)"]))
cat(sprintf("  β₅ (mismatch_exp): %+.4f (p=%.4f)\n",
            m3_re_robust["mismatch_exp", "Estimate"],
            m3_re_robust["mismatch_exp", "Pr(>|t|)"]))

saveRDS(m3_reentrant, "data/processed/m3_reentrant.rds")
cat("  ✓ m3_reentrant.rds xuất xong\n")

# ════════════════════════════════════════════════════════════
# D — DIAGNOSTIC TESTS (chạy trên M3 Full)
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   DIAGNOSTIC TESTS — M3 FULL       \n")
cat("====================================\n")

# ── D1. VIF (Safe — tránh lỗi với factor nhiều levels) ────────
# Dùng model rút gọn (sector thay vì industry_sub) vì vif() của
# car không handle tốt factor quá nhiều levels. Ghi chú rõ.
cat("\n--- D1. VIF TEST ---\n")
cat("  (Dùng model rút gọn: sector thay industry_sub để tránh\n")
cat("   rank-deficiency với factor nhiều levels)\n\n")

safe_vif <- function(model) {
  X       <- model.matrix(model)
  X       <- X[, apply(X, 2, var) != 0, drop = FALSE]
  X       <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  qrX     <- qr(X)
  X_clean <- X[, qrX$pivot[seq_len(qrX$rank)], drop = FALSE]
  df_temp <- as.data.frame(X_clean)
  df_temp$y <- rnorm(nrow(df_temp))
  vif(lm(y ~ ., data = df_temp))
}

m3_vif <- lm(
  ln_wage ~ mismatch + exp + exp2 + female +
    mismatch_exp + mismatch_female + exp_female + triple +
    edu_level + urban + marital + sector,
  data = df_train
)

vif_vals <- tryCatch(
  {
    v <- vif(m3_vif)
    # vif() trả về matrix khi có factor — lấy GVIF^(1/(2*Df))
    if (is.matrix(v)) v[, "GVIF^(1/(2*Df))"] else v
  },
  error = function(e) {
    cat("  ⚠ vif() thất bại — dùng safe_vif()\n")
    safe_vif(m3_vif)
  }
)

print(round(vif_vals, 2))

if (any(vif_vals > 10)) {
  cat("  ✗ VIF > 10:", paste(names(vif_vals[vif_vals > 10]), collapse = ", "), "\n")
} else if (any(vif_vals > 5)) {
  cat("  ⚠ VIF > 5 :", paste(names(vif_vals[vif_vals > 5]),  collapse = ", "), "\n")
  cat("    Lưu ý: interaction terms thường có VIF cao — chấp nhận được\n")
} else {
  cat("  ✓ Tất cả VIF <= 5 — không có đa cộng tuyến đáng kể\n")
}

# ── D2. BREUSCH-PAGAN TEST ────────────────────────────────────
cat("\n--- D2. BREUSCH-PAGAN TEST (Heteroskedasticity) ---\n")

bp <- bptest(m3)
cat(sprintf("  BP statistic : %.4f\n", bp$statistic))
cat(sprintf("  p-value      : %.4f\n", bp$p.value))

hetero <- bp$p.value < 0.05
if (hetero) {
  cat("  → Có heteroskedasticity → dùng Robust SE (HC1) ✓\n")
  cat("  → Tất cả models đã dùng vcovHC(type='HC1') — OK\n")
} else {
  cat("  → Không có heteroskedasticity\n")
  cat("  → OLS SE đủ dùng, nhưng Robust SE vẫn valid\n")
}

# ── D3. RESIDUAL PLOTS ────────────────────────────────────────
cat("\n--- D3. RESIDUAL PLOTS ---\n")

# Residuals vs Fitted
png(paste0(OUT_FIG, "Diag1_resid_vs_fitted.png"),
    width = 800, height = 500, res = 120)
plot(m3$fitted.values, resid(m3),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "M3 Full — Residuals vs Fitted",
     pch  = 16, cex = 0.4, col = rgb(0, 0, 0, 0.3))
abline(h = 0, col = "red", lwd = 1.5, lty = 2)
dev.off()
cat("  ✓ Diag1_resid_vs_fitted.png xuất xong\n")

# Q-Q Plot
png(paste0(OUT_FIG, "Diag2_qqplot.png"),
    width = 800, height = 500, res = 120)
qqnorm(resid(m3),
       main = "M3 Full — Normal Q-Q Plot",
       pch  = 16, cex = 0.4, col = rgb(0, 0, 0, 0.3))
qqline(resid(m3), col = "red", lwd = 1.5)
dev.off()
cat("  ✓ Diag2_qqplot.png xuất xong\n")

# ── D4. ROBUST SE — In lại M3 với Robust SE sau khi confirm ──
cat("\n--- D4. M3 FULL — Kết quả cuối với Robust SE (HC1) ---\n")
print(m3_robust)

# ════════════════════════════════════════════════════════════
# SO SÁNH β₁ VÀ β₅ GIỮA CÁC MODELS
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   SO SÁNH CÁC MODELS               \n")
cat("====================================\n")

cat(sprintf("\n%-15s | %9s | %10s | %5s\n",
            "Model", "β₁(mis)", "β₅(mis×exp)", "N"))
cat(strrep("-", 50), "\n")

for (info in list(
  list("M3 Full",       m3,           nobs(m3)),
  list("S1 Male",       m3_male,      nobs(m3_male)),
  list("S2 Re-entrant", m3_reentrant, nobs(m3_reentrant))
)) {
  m  <- info[[2]]
  b1 <- coef(m)["mismatch"]
  b5 <- coef(m)["mismatch_exp"]
  cat(sprintf("%-15s | %+9.4f | %+10.4f | %5d\n",
              info[[1]], b1, b5, info[[3]]))
}

cat(sprintf("\n  R² train (M3) : %.4f\n", r2_train))
cat(sprintf("  R² test  (M3) : %.4f\n", r2_test))
cat(sprintf("  RMSE test(M3) : %.4f\n", rmse_test))

# ════════════════════════════════════════════════════════════
# XUẤT BẢNG TỔNG HỢP — modelsummary
# ════════════════════════════════════════════════════════════
cat("\n--- Xuất Table3_regression.html ---\n")

modelsummary(
  list(
    "M1 Baseline" = m1,
    "M2 +Exp"     = m2,
    "M3 Full"     = m3,
    "S1 Male"     = m3_male,
    "S2 Re-entry" = m3_reentrant
  ),
  vcov     = "HC1",
  coef_map = c(
    "mismatch"        = "Mismatch (β₁)",
    "exp"             = "Kinh nghiệm",
    "exp2"            = "Kinh nghiệm²",
    "female"          = "Nữ",
    "mismatch_exp"    = "Mismatch × Exp (β₅)",
    "mismatch_female" = "Mismatch × Nữ (β₆)",
    "exp_female"      = "Exp × Nữ (β₇)",
    "triple"          = "Mismatch × Exp × Nữ (β₈)",
    "edu_level"       = "Trình độ học vấn",
    "urban"           = "Đô thị",
    "marital"         = "Có vợ/chồng"
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output  = paste0(OUT_TAB, "Table3_regression.html"),
  title   = "Bảng 3: Kết quả hồi quy OLS (Robust SE, HC1)",
  notes   = paste0(
    "*** p<0.01, ** p<0.05, * p<0.1. Robust SE (HC1). ",
    "Tất cả models kiểm soát sector và industry FE. ",
    "Train set 70% (seed=123). ",
    "R² test (M3) = ", round(r2_test, 4),
    ", RMSE test = ",  round(rmse_test, 4), ".",
    " BP test M3: p=", round(bp$p.value, 4),
    ifelse(hetero, " → có heteroskedasticity.", " → không có heteroskedasticity.")
  )
)
cat("✓ Table3_regression.html xuất xong\n")

# ── EXPORT TOÀN BỘ ───────────────────────────────────────────
saveRDS(
  list(
    m1           = m1,
    m2           = m2,
    m3           = m3,
    m3_male      = m3_male,
    m3_reentrant = m3_reentrant,
    r2_train     = r2_train,
    r2_test      = r2_test,
    rmse_test    = rmse_test,
    bp_pvalue    = bp$p.value,
    hetero       = hetero
  ),
  "data/processed/models_list.rds"
)
cat("✓ models_list.rds xuất xong\n")

cat("\n====================================\n")
cat("✓ 03_models.R hoàn thành!\n")
cat("  Output files:\n")
cat("    data/processed/m1.rds\n")
cat("    data/processed/m2.rds\n")
cat("    data/processed/m3.rds\n")
cat("    data/processed/m3_male.rds\n")
cat("    data/processed/m3_reentrant.rds\n")
cat("    data/processed/models_list.rds\n")
cat("    outputs/tables/Table3_regression.html\n")
cat("    outputs/figures/Diag1_resid_vs_fitted.png\n")
cat("    outputs/figures/Diag2_qqplot.png\n")
cat("  Tiếp theo chạy: 04_margins.R\n")
cat("====================================\n")
