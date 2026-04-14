
# ================================
# diagnostics.R (FULL AUTO REPORT)
# ================================

library(lmtest)
library(sandwich)
library(car)
library(ggplot2)

cat("\n====================================\n")
cat("        DIAGNOSTIC TESTS REPORT     \n")
cat("====================================\n")

# ================================
# 1. VIF TEST
# ================================

# ================================
# 1.1. SAFE VIF FUNCTION
# ================================
safe_vif <- function(model) {
  
  # Lấy design matrix
  X <- model.matrix(model)
  
  # Loại cột constant
  X <- X[, apply(X, 2, var) != 0, drop = FALSE]
  
  # Loại intercept
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  
  # Giữ cột độc lập tuyến tính
  qrX <- qr(X)
  X_clean <- X[, qrX$pivot[1:qrX$rank], drop = FALSE]
  
  # Fit lại model giả để tính VIF
  df_temp <- as.data.frame(X_clean)
  df_temp$y <- rnorm(nrow(df_temp))
  
  model_temp <- lm(y ~ ., data = df_temp)
  
  return(vif(model_temp))
}

# ================================
# 1.2. RUN SAFE VIF
# ================================
cat("\n--- VIF TEST (SAFE) ---\n")

vif_values <- safe_vif(model)
print(vif_values)

cat("\nKết luận VIF:\n")
if (max(vif_values) < 5) {
  cat("→ Không có đa cộng tuyến đáng kể\n")
} else if (max(vif_values) < 10) {
  cat("→ Đa cộng tuyến mức vừa\n")
} else {
  cat("→ Đa cộng tuyến nghiêm trọng\n")
}



# ================================
# 2. BREUSCH-PAGAN TEST
# ================================
cat("\n--- BREUSCH-PAGAN TEST (Heteroskedasticity) ---\n")

bp_test <- bptest(model)
print(bp_test)

cat("\nKết luận BP test:\n")
if (bp_test$p.value < 0.05) {
  cat("→ Có heteroskedasticity (p-value =", round(bp_test$p.value,4), ")\n")
  hetero <- TRUE
} else {
  cat("→ Không có heteroskedasticity (p-value =", round(bp_test$p.value,4), ")\n")
  hetero <- FALSE
}

# ================================
# 3. RESIDUAL PLOTS
# ================================

# Residual vs Fitted
plot(model$fitted.values, resid(model),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

cat("\nResidual vs Fitted:\n")
cat("→ Kiểm tra xem residual có phân tán ngẫu nhiên quanh 0 không\n")

# Q-Q plot
qqnorm(resid(model))
qqline(resid(model), col = "red")

cat("\nQ-Q Plot:\n")
cat("→ Nếu điểm nằm gần đường thẳng → residual gần phân phối chuẩn\n")

# ================================
# 4. FINAL CONCLUSION
# ================================
cat("\n====================================\n")
cat("            FINAL CONCLUSION        \n")
cat("====================================\n")

if (hetero) {
  cat("→ Model có heteroskedasticity\n")
  cat("→ KẾT LUẬN: NÊN sử dụng ROBUST STANDARD ERRORS\n\n")
  
  cat("→ Kết quả với robust SE:\n")
  print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
} else {
  cat("→ Model không có heteroskedasticity\n")
  cat("→ KẾT LUẬN: Có thể dùng OLS standard errors\n")
}

cat("\n====================================\n")
cat("          END OF REPORT             \n")
cat("====================================\n")

