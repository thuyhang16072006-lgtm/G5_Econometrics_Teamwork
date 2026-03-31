# ============================================================
# 00_setup.R
# Mục đích: cài packages, định nghĩa paths, constants
# Chạy file này đầu tiên, mọi script khác đều source() file này
# ============================================================

# Cài packages 
packages <- c(
  "haven",            # đọc file .dta
  "dplyr",            # data manipulation
  "ggplot2",          # biểu đồ
  "lmtest",           # BP test, coeftest
  "sandwich",         # robust SE
  "stargazer",        # xuất bảng hồi quy
  "marginaleffects",  # marginal effects plot
  "car",              # VIF test
  "gtsummary"         # summary statistics table
)

installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0) install.packages(to_install)

lapply(packages, library, character.only = TRUE)

# Paths
PATH_RAW       <- "data/raw/"
PATH_PROCESSED <- "data/processed/"
PATH_TABLES    <- "output/tables/"
PATH_FIGURES   <- "output/figures/"

# Constants 
SEED <- 123
TRAIN_RATIO <- 0.7

# Bảng quy đổi C17A → số năm đi học
EDU_YEARS_MAP <- c(
  "1" = 0,   # Chưa bao giờ đi học
  "2" = 5,   # Chưa học xong tiểu học
  "3" = 5,   # Tiểu học
  "4" = 9,   # THCS
  "5" = 12,  # THPT
  "6" = 14,  # Trung cấp
  "7" = 16,  # Cao đẳng
  "8" = 16,  # Đại học
  "9" = 19   # Trên đại học
)

cat("Setup hoàn tất\n")