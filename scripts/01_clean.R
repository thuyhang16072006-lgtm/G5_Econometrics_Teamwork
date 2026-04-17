# ============================================================
# 01_clean.R — Sample filtering & variable construction
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# ============================================================

library(tidyverse)
library(haven)

# ── 1. LOAD DATA ─────────────────────────────────────────────
df_raw <- read_csv("data/raw/LFS_2018.csv")
cat("Raw N:", nrow(df_raw), "\n")

# ── 2. SAMPLE FILTERING ──────────────────────────────────────
df <- df_raw

# Bước 1: Tuổi lao động 15–65
df <- df |> filter(C5 >= 15, C5 <= 65)
cat("Sau filter tuổi:", nrow(df), "\n")

# Bước 2: Đang có việc làm
df <- df |> filter(C21 == 1 | C22 == 1 | C23 == 1)
cat("Sau filter có việc:", nrow(df), "\n")

# Bước 3: Làm công ăn lương
df <- df |> filter(C35 == 5)
cat("Sau filter làm công:", nrow(df), "\n")

# Bước 4: Ngành chế biến chế tạo VSIC C (10–33)
df <- df |>
  mutate(vsic2 = C30C %/% 100) |>
  filter(vsic2 >= 10, vsic2 <= 33)
cat("Sau filter ngành CB-CT:", nrow(df), "\n")

# Bước 5: Trình độ Cao đẳng trở lên (C17 >= 7)
df <- df |> filter(C17 >= 7)
cat("Sau filter trình độ:", nrow(df), "\n")

# Bước 6: Thu nhập > 0
df <- df |> filter(C44 > 0, !is.na(C44))
cat("Sau filter thu nhập:", nrow(df), "\n")

# Bước 7: C51 hợp lệ
df <- df |> filter(C51 %in% c(1, 2))
cat("Sau filter C51:", nrow(df), "\n")

# ── KIỂM TRA CELL SIZE ───────────────────────────────────────
cat("\n--- Cell size check: mismatch × female ---\n")
print(table(
  mismatch = ifelse(df$C51 == 2, 1, 0),
  female   = ifelse(df$C3  == 2, 1, 0)
))

# ── 3. VARIABLE CONSTRUCTION ─────────────────────────────────
df <- df |>
  mutate(
    # Biến phụ thuộc
    ln_wage = log(C44 / 1000),
    
    # Biến độc lập chính
    mismatch = ifelse(C51 == 2, 1, 0),
    
    # Giới tính
    female = ifelse(C3 == 2, 1, 0),
    
    # Số năm học theo C17
    schooling = case_when(
      C17 == 7 ~ 14,   # Cao đẳng
      C17 == 8 ~ 16,   # Đại học
      C17 == 9 ~ 18,   # Trên đại học
      TRUE     ~ NA_real_
    ),
    
    # Kinh nghiệm Mincer
    exp  = C5 - schooling - 6,
    exp2 = exp^2,
    
    # Biến kiểm soát
    # GIỮ NGUYÊN C9 GỐC để dùng cho re_entrant
    marital      = ifelse(C9 == 2, 1, 0),  # 1 = đang có vợ/chồng
    urban        = ifelse(TTNT == 1, 1, 0),
    edu_level    = C17,
    industry_sub = vsic2,
    
    sector = case_when(
      C31 %in% 7:10 ~ "state",
      C31 == 11     ~ "fdi",
      TRUE          ~ "private"
    ),
    
    # Interaction terms
    mismatch_exp    = mismatch * exp,
    mismatch_female = mismatch * female,
    exp_female      = exp * female,
    triple          = mismatch * exp * female,
    
    # ── RE-ENTRANT PROXY ─────────────────────────────────────
    # Định nghĩa: nữ >= 30 tuổi, từng/đang có gia đình (C9=2,3,4),
    # ít kinh nghiệm thực tế (exp <= 10) dù tuổi đã cao
    # → proxy cho nhóm đã nghỉ việc vì gia đình rồi quay lại
    # Lưu ý: C9=2 có vợ/chồng, C9=3 góa, C9=4 ly hôn/ly thân
    re_entrant = as.integer(
      female == 1 &
        C5 >= 30 &
        C9 %in% c(2, 3, 4) &   # ← SỬA: dùng C9 gốc thay vì marital
        exp <= 10
    )
  ) |>
  
  # Loại exp âm
  filter(exp >= 0, !is.na(exp))

cat("\nSau tạo biến, loại exp < 0:", nrow(df), "\n")

# ── 4. WINSORIZE ln_wage ──────────────────────────────────────
q <- quantile(df$ln_wage, c(0.01, 0.99), na.rm = TRUE)
df <- df |>
  mutate(ln_wage = pmax(pmin(ln_wage, q[2]), q[1]))

# ── 5. KIỂM TRA CUỐI ─────────────────────────────────────────
cat("\n--- Final sample summary ---\n")
cat("N cuối (df_clean):", nrow(df), "\n")
cat("Missing ln_wage:",   sum(is.na(df$ln_wage)), "\n")
cat("Missing mismatch:",  sum(is.na(df$mismatch)), "\n")
cat("Missing exp:",       sum(is.na(df$exp)), "\n")

cat("\nPhân phối mismatch:\n")
print(prop.table(table(df$mismatch)))

cat("\nPhân phối female:\n")
print(prop.table(table(df$female)))

cat("\nSummary ln_wage:\n")
print(summary(df$ln_wage))

cat("\nSummary exp:\n")
print(summary(df$exp))

# ── 6. SUBSAMPLES ────────────────────────────────────────────
# Subsample 1: Nam — baseline không có career gap
df_male <- df |> filter(female == 0)
cat("\nN subsample Male:", nrow(df_male), "\n")

# Subsample 2: Re-entrants — phụ nữ quay lại sau career break
df_reentrant <- df |> filter(re_entrant == 1)
cat("N subsample Re-entrant:", nrow(df_reentrant), "\n")

# Kiểm tra N re-entrant
if (nrow(df_reentrant) < 100) {
  cat("⚠ Re-entrant N < 100 — nới lỏng điều kiện exp <= 15\n")
  df_reentrant <- df |>
    filter(female == 1, C5 >= 30, C9 %in% c(2, 3, 4), exp <= 15)
  cat("N re-entrant sau nới lỏng:", nrow(df_reentrant), "\n")
}

# Thống kê mô tả nhanh để verify 2 subsample
cat("\n--- So sánh nhanh 3 nhóm ---\n")
df |>
  mutate(group = case_when(
    re_entrant == 1 ~ "Re-entrant",
    female == 0     ~ "Male",
    TRUE            ~ "Female (other)"
  )) |>
  group_by(group) |>
  summarise(
    N            = n(),
    pct_mismatch = round(mean(mismatch) * 100, 1),
    mean_lnwage  = round(mean(ln_wage), 3),
    mean_exp     = round(mean(exp), 1),
    mean_age     = round(mean(C5), 1),
    .groups      = "drop"
  ) |>
  print()

# ── 7. TRAIN/TEST SPLIT ───────────────────────────────────────
set.seed(123)
train_idx <- sample(nrow(df), size = floor(0.7 * nrow(df)))
df_train  <- df[ train_idx, ]
df_test   <- df[-train_idx, ]

cat("\nN train:", nrow(df_train), "\n")
cat("N test: ", nrow(df_test),  "\n")

# ── 8. EXPORT ─────────────────────────────────────────────────
saveRDS(df,           "data/processed/data_processed.rds")
saveRDS(df_male,      "data/processed/df_male.rds")
saveRDS(df_reentrant, "data/processed/df_reentrant.rds")
saveRDS(df_train,     "data/processed/df_train.rds")
saveRDS(df_test,      "data/processed/df_test.rds")

cat("\n✓ Export xong 5 files:\n")
cat("  data_processed.rds  — toàn mẫu\n")
cat("  df_male.rds         — subsample 1: Nam\n")
cat("  df_reentrant.rds    — subsample 2: Re-entrants\n")
cat("  df_train.rds        — 70% train\n")
cat("  df_test.rds         — 30% test\n")
cat("\n✓ 01_clean.R hoàn thành! Tiếp theo chạy 02_eda.R\n")