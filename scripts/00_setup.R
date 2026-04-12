
# ============================================================
# 00_setup.R — Environment setup
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# Chạy file này MỘT LẦN duy nhất khi mới clone repo
# ============================================================

# ── 1. PACKAGES ──────────────────────────────────────────────
packages <- c(
  # Data I/O
  "haven",            # đọc file .csv (Stata) — dùng read.csv()
  "tidyverse",        # dplyr, ggplot2, tidyr, readr, purrr
  
  # Regression & inference
  "lmtest",           # coeftest() — apply robust SE lên model
  "sandwich",         # vcovHC() — tính HC1 robust variance-covariance
  "car",              # vif() — kiểm tra multicollinearity
  
  # Marginal effects
  "marginaleffects",  # slopes(), predictions() — dùng trong 04_margins.R
  
  # Tables & output
  "modelsummary",     # datasummary_skim(), datasummary_balance(),
  # modelsummary() — export bảng Word-friendly
  "scales",           # label_number(), label_comma() — format trục ggplot2
  
  # Optional nhưng hữu ích
  "fixest"            # feols() — OLS với FE, nhanh hơn lm() nhiều
)

# Chỉ cài những package chưa có
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  cat("Đang cài packages:", paste(packages[!installed], collapse=", "), "\n")
  install.packages(packages[!installed])
}

# Load tất cả để kiểm tra không lỗi
invisible(lapply(packages, library, character.only = TRUE))
cat("Tất cả packages đã load OK!\n")

# ── 2. ĐƯỜNG DẪN CHUNG ───────────────────────────────────────
# Dùng relative path — không dùng absolute path kiểu C:/Users/...
# Chỉ chạy đúng khi mở project qua file .Rproj

DATA_RAW  <- "data/raw/LFS_2018.csv"     # hoặc LFS_2018.dta
DATA_PROC <- "data/processed/"
OUT_FIG   <- "output/figures/"
OUT_TAB   <- "output/tables/"

# Tạo folder nếu chưa có
dir.create(DATA_PROC, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG,   recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB,   recursive = TRUE, showWarnings = FALSE)

cat("Folder structure OK!\n")

# ── 3. GHI LẠI ENVIRONMENT ───────────────────────────────────
# Dùng để debug khi kết quả chạy khác nhau giữa các máy
cat("\n=== Session Info ===\n")
cat("R version:", R.version$version.string, "\n")
cat("OS:", .Platform$OS.type, "\n")
cat("Key packages:\n")
for (p in c("tidyverse","lmtest","sandwich","modelsummary","marginaleffects")) {
  cat(" ", p, ":", as.character(packageVersion(p)), "\n")
}

# ── 4. GLOBAL OPTIONS ────────────────────────────────────────
options(
  scipen    = 999,    # tắt scientific notation (1234 thay vì 1.23e3)
  digits    = 4,      # 4 chữ số thập phân mặc định
  dplyr.summarise.inform = FALSE  # tắt thông báo group_by
)

cat("\n00_setup.R hoàn thành! Tiếp theo chạy 01_clean.R\n")