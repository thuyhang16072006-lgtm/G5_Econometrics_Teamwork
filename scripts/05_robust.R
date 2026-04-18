# ============================================================
# 05_robust.R — Robustness Checks
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# Pipeline: 03_models.R → 04_margins.R → [05_robust.R]
#
# Nội dung:
#   A1 — Thay edu_level bằng schooling years (biến liên tục)
#   A4 — Lọc full-time only (C46 >= 35 giờ/tuần)
#   A5 — Thay đổi định nghĩa mismatch (C51 == 1 thay vì == 2)
#   OUT — Bảng so sánh β₁/β₅ giữa M3 gốc vs các spec
# ============================================================

source("scripts/00_setup.R")
library(modelsummary)
library(lmtest)
library(sandwich)
library(dplyr)

# ── 1. LOAD ───────────────────────────────────────────────────
df_train   <- readRDS("data/processed/df_train.rds")
models_obj <- readRDS("data/processed/models_list.rds")
m3_base    <- models_obj$m3   # M3 Full gốc — benchmark

cat("====================================\n")
cat("   ROBUSTNESS CHECKS                \n")
cat("====================================\n")
cat(sprintf("  Benchmark M3 N : %d\n", nobs(m3_base)))
cat(sprintf("  β₁ gốc        : %+.4f\n", coef(m3_base)["mismatch"]))
cat(sprintf("  β₅ gốc        : %+.4f\n", coef(m3_base)["mismatch_exp"]))

# ── Formula gốc M3 (dùng lại để so sánh apples-to-apples) ────
f_full_base <- ln_wage ~ mismatch + exp + exp2 + female +
  mismatch_exp + mismatch_female + exp_female + triple +
  edu_level + urban + marital +
  factor(sector) + factor(industry_sub)

# Helper: in kết quả nhanh một model
print_robust <- function(label, model, note = "") {
  rob  <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  b1   <- rob["mismatch",     "Estimate"]
  b1_p <- rob["mismatch",     "Pr(>|t|)"]
  b5   <- rob["mismatch_exp", "Estimate"]
  b5_p <- rob["mismatch_exp", "Pr(>|t|)"]
  sig1 <- ifelse(b1_p < 0.01, "***", ifelse(b1_p < 0.05, "**",
                                            ifelse(b1_p < 0.1, "*", "")))
  sig5 <- ifelse(b5_p < 0.01, "***", ifelse(b5_p < 0.05, "**",
                                            ifelse(b5_p < 0.1, "*", "")))
  cat(sprintf("  β₁ (mismatch)    : %+.4f%s (p=%.4f)\n", b1, sig1, b1_p))
  cat(sprintf("  β₅ (mismatch_exp): %+.4f%s (p=%.4f)\n", b5, sig5, b5_p))
  cat(sprintf("  Wage penalty     : %.1f%%\n", (exp(b1) - 1) * 100))
  cat(sprintf("  R²               : %.4f\n", summary(model)$r.squared))
  cat(sprintf("  N                : %d\n", nobs(model)))
  if (note != "") cat(sprintf("  Ghi chú          : %s\n", note))
  
  # So sánh với benchmark
  b1_base <- coef(m3_base)["mismatch"]
  delta   <- b1 - b1_base
  cat(sprintf("  Δβ₁ vs M3 gốc   : %+.4f (%s)\n", delta,
              ifelse(abs(delta) < 0.02, "✓ ổn định",
                     ifelse(abs(delta) < 0.05, "⚠ lệch nhẹ", "✗ lệch nhiều"))))
}

# ════════════════════════════════════════════════════════════
# A1 — THAY edu_level BẰNG schooling YEARS (biến liên tục)
# Mục đích: kiểm tra xem kết quả có nhạy cảm với cách đo
#           trình độ học vấn không
# Thay: edu_level (7/8/9 categorical) → schooling (14/16/18 continuous)
# Lưu ý: schooling đã có trong df_train từ 01_clean.R
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   A1 — EDU: schooling years        \n")
cat("====================================\n")

# Kiểm tra biến schooling có trong data không
if (!"schooling" %in% names(df_train)) {
  # Nếu chưa có — tạo lại từ edu_level
  cat("  ⚠ schooling chưa có trong df_train — tạo lại từ C17\n")
  df_train <- df_train |>
    mutate(schooling = case_when(
      C17 == 7 ~ 14,
      C17 == 8 ~ 16,
      C17 == 9 ~ 18,
      TRUE     ~ NA_real_
    ))
}

cat(sprintf("  schooling distribution:\n"))
print(table(df_train$schooling, useNA = "ifany"))

f_a1 <- ln_wage ~ mismatch + exp + exp2 + female +
  mismatch_exp + mismatch_female + exp_female + triple +
  schooling +           # ← thay edu_level
  urban + marital +
  factor(sector) + factor(industry_sub)

m_a1 <- lm(f_a1, data = df_train)
print_robust("A1 schooling", m_a1,
             "schooling years (14/16/18) thay edu_level (7/8/9)")

saveRDS(m_a1, "data/processed/m_a1.rds")
cat("  ✓ m_a1.rds xuất xong\n")

# ════════════════════════════════════════════════════════════
# A4 — LỌC FULL-TIME ONLY (C46 >= 35 giờ/tuần)
# Mục đích: loại lao động bán thời gian — tránh bias lương
#           do số giờ làm khác nhau giữa matched/mismatched
# Lưu ý: C46 là giờ làm thông thường/tuần trong df_train
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   A4 — FULL-TIME ONLY (C46 >= 35)  \n")
cat("====================================\n")

if (!"C46" %in% names(df_train)) {
  stop("C46 không có trong df_train — kiểm tra lại 01_clean.R (cần giữ C46)")
}

cat(sprintf("  Phân phối C46 (giờ làm/tuần):\n"))
cat(sprintf("  < 35h  : %d (%.1f%%)\n",
            sum(df_train$C46 < 35,  na.rm = TRUE),
            mean(df_train$C46 < 35, na.rm = TRUE) * 100))
cat(sprintf("  >= 35h : %d (%.1f%%)\n",
            sum(df_train$C46 >= 35,  na.rm = TRUE),
            mean(df_train$C46 >= 35, na.rm = TRUE) * 100))
cat(sprintf("  NA     : %d\n", sum(is.na(df_train$C46))))

df_fulltime <- df_train |> filter(!is.na(C46), C46 >= 35)
cat(sprintf("  N sau filter full-time: %d\n", nrow(df_fulltime)))

if (nrow(df_fulltime) < 500) {
  cat("  ⚠ N < 500 sau filter — kết quả có thể không ổn định\n")
}

m_a4 <- lm(f_full_base, data = df_fulltime)
print_robust("A4 full-time", m_a4, "Chỉ giữ C46 >= 35h/tuần")

saveRDS(m_a4, "data/processed/m_a4.rds")
cat("  ✓ m_a4.rds xuất xong\n")

# ════════════════════════════════════════════════════════════
# A5 — THAY ĐỔI ĐỊNH NGHĨA MISMATCH
# Mục đích: kiểm tra xem β₁ có nhạy cảm với cách đo mismatch
#
# Định nghĩa gốc  : mismatch = (C51 == 2) — tự báo cáo over-qualified
# Định nghĩa A5a  : mismatch_strict = (C51 == 2) & (edu_level == 9)
#                   → chỉ lấy overqualified + trình độ cao nhất (SĐH)
#                   → kiểm tra xem penalty tập trung ở nhóm học cao không
# Định nghĩa A5b  : mismatch_broad = (C51 %in% c(2, 3))
#                   → nếu C51==3 là under-qualified, gộp cả 2 loại mismatch
#                   → Lưu ý: chỉ dùng nếu C51==3 có trong data sau filter
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   A5 — ALTERNATIVE MISMATCH DEF    \n")
cat("====================================\n")

# Phân phối C51 gốc trong df_train
cat("  Phân phối C51 trong df_train:\n")
print(table(df_train$C51, useNA = "ifany"))

# ── A5a: Strict — overqualified + SĐH ────────────────────────
cat("\n--- A5a: Strict mismatch (C51==2 & edu_level==9) ---\n")

df_a5a <- df_train |>
  mutate(
    mismatch        = as.integer(C51 == 2 & edu_level == 9),
    mismatch_exp    = mismatch * exp,
    mismatch_female = mismatch * female,
    exp_female      = exp * female,
    triple          = mismatch * exp * female
  )

cat(sprintf("  N mismatch_strict=1 : %d (%.1f%%)\n",
            sum(df_a5a$mismatch),
            mean(df_a5a$mismatch) * 100))

if (sum(df_a5a$mismatch) < 50) {
  cat("  ⚠ N mismatch_strict < 50 — quá ít, bỏ qua A5a\n")
  m_a5a <- NULL
} else {
  m_a5a <- lm(f_full_base, data = df_a5a)
  print_robust("A5a strict", m_a5a,
               "mismatch = C51==2 & edu_level==9 (SĐH overqualified)")
  saveRDS(m_a5a, "data/processed/m_a5a.rds")
  cat("  ✓ m_a5a.rds xuất xong\n")
}

# ── A5b: Broad — gộp cả over & under qualified ────────────────
cat("\n--- A5b: Broad mismatch (C51 %in% c(2,3)) ---\n")
cat("  Lưu ý: trong sample gốc đã filter C51 %in% c(1,2)\n")
cat("  → C51==3 không có trong df_train\n")
cat("  → A5b sẽ đọc lại từ data_processed.rds (toàn mẫu)\n")

df_all <- tryCatch(
  readRDS("data/processed/data_processed.rds"),
  error = function(e) {
    cat("  ⚠ data_processed.rds không tìm thấy — bỏ qua A5b\n")
    NULL
  }
)

if (!is.null(df_all) && "C51" %in% names(df_all)) {
  
  cat(sprintf("  Phân phối C51 (toàn mẫu): "))
  print(table(df_all$C51, useNA = "ifany"))
  
  has_c51_3 <- any(df_all$C51 == 3, na.rm = TRUE)
  
  if (!has_c51_3) {
    cat("  ⚠ C51==3 không xuất hiện sau sample restriction → bỏ qua A5b\n")
    m_a5b <- NULL
  } else {
    # Tạo lại sample với C51 %in% c(1,2,3) rồi redefine mismatch
    set.seed(123)
    df_broad <- df_all |>
      filter(C51 %in% c(1, 2, 3)) |>
      mutate(
        mismatch        = as.integer(C51 %in% c(2, 3)),
        mismatch_exp    = mismatch * exp,
        mismatch_female = mismatch * female,
        exp_female      = exp * female,
        triple          = mismatch * exp * female
      )
    
    train_idx_broad <- sample(nrow(df_broad),
                              size = floor(0.7 * nrow(df_broad)))
    df_broad_train  <- df_broad[train_idx_broad, ]
    
    cat(sprintf("  N broad train: %d\n", nrow(df_broad_train)))
    cat(sprintf("  N mismatch_broad=1: %d (%.1f%%)\n",
                sum(df_broad_train$mismatch),
                mean(df_broad_train$mismatch) * 100))
    
    m_a5b <- lm(f_full_base, data = df_broad_train)
    print_robust("A5b broad", m_a5b,
                 "mismatch = C51 %in% c(2,3) — over & under qualified")
    saveRDS(m_a5b, "data/processed/m_a5b.rds")
    cat("  ✓ m_a5b.rds xuất xong\n")
  }
} else {
  m_a5b <- NULL
}

# ════════════════════════════════════════════════════════════
# BẢNG TỔNG HỢP — So sánh β₁ giữa M3 gốc vs các spec
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   BẢNG SO SÁNH ROBUSTNESS          \n")
cat("====================================\n")

# Console table nhanh
cat(sprintf("\n%-22s | %9s | %9s | %5s\n",
            "Spec", "β₁(mis)", "β₅(mis×exp)", "N"))
cat(strrep("-", 55), "\n")

specs <- list(
  list("M3 Gốc (benchmark)", m3_base),
  list("A1 schooling years",  m_a1),
  list("A4 full-time",        m_a4)
)
if (!is.null(m_a5a)) specs <- c(specs, list(list("A5a strict mis", m_a5a)))
if (!is.null(m_a5b)) specs <- c(specs, list(list("A5b broad mis",  m_a5b)))

for (s in specs) {
  label <- s[[1]]
  m     <- s[[2]]
  b1    <- coef(m)["mismatch"]
  b5    <- coef(m)["mismatch_exp"]
  cat(sprintf("%-22s | %+9.4f | %+9.4f | %5d\n",
              label, b1, b5, nobs(m)))
}

# ── modelsummary HTML ─────────────────────────────────────────
model_list_robust <- list("M3 Gốc" = m3_base,
                          "A1 Schooling" = m_a1,
                          "A4 Full-time" = m_a4)
if (!is.null(m_a5a)) model_list_robust[["A5a Strict"]] <- m_a5a
if (!is.null(m_a5b)) model_list_robust[["A5b Broad"]]  <- m_a5b

modelsummary(
  model_list_robust,
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
    "edu_level"       = "Trình độ học vấn (edu_level)",
    "schooling"       = "Số năm học (schooling)",
    "urban"           = "Đô thị",
    "marital"         = "Có vợ/chồng"
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output  = paste0(OUT_TAB, "Table5_robustness.html"),
  title   = "Bảng 5: Robustness Checks — So sánh β₁ giữa các Specification",
  notes   = paste0(
    "*** p<0.01, ** p<0.05, * p<0.1. Robust SE (HC1). ",
    "M3 Gốc: benchmark từ 03_models.R. ",
    "A1: schooling years (14/16/18) thay edu_level. ",
    "A4: chỉ lao động full-time (C46 >= 35h/tuần). ",
    "A5a: mismatch = C51==2 & edu_level==9 (SĐH overqualified). ",
    "A5b: mismatch = C51 %in% c(2,3) nếu C51==3 tồn tại trong data."
  )
)
cat("\n✓ Table5_robustness.html xuất xong\n")

# ── KẾT LUẬN TỰ ĐỘNG ─────────────────────────────────────────
cat("\n====================================\n")
cat("   KẾT LUẬN ROBUSTNESS              \n")
cat("====================================\n")

b1_base <- coef(m3_base)["mismatch"]

for (s in specs[-1]) {   # bỏ benchmark
  label  <- s[[1]]
  m      <- s[[2]]
  b1_alt <- coef(m)["mismatch"]
  delta  <- b1_alt - b1_base
  status <- ifelse(abs(delta) < 0.02, "✓ ổn định",
                   ifelse(abs(delta) < 0.05, "⚠ lệch nhẹ", "✗ lệch nhiều"))
  same_sign <- sign(b1_alt) == sign(b1_base)
  cat(sprintf("  %-22s: Δβ₁=%+.4f | %s | Dấu %s\n",
              label, delta, status,
              ifelse(same_sign, "✓ nhất quán", "✗ đổi chiều")))
}

cat("\n✓ 05_robust.R hoàn thành!\n")
cat("  Output files:\n")
cat("    data/processed/m_a1.rds\n")
cat("    data/processed/m_a4.rds\n")
cat("    data/processed/m_a5a.rds  (nếu N đủ)\n")
cat("    data/processed/m_a5b.rds  (nếu C51==3 tồn tại)\n")
cat("    outputs/tables/Table5_robustness.html\n")
cat("====================================\n")