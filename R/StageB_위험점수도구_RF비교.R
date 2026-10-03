# Stage B. 청년 자살생각 위험점수도구 개발 + 랜덤 포레스트 비교
#
# 사전 결정 사항
# - 주 모델: 로지스틱 회귀 (위험점수도구 산출)
# - 보조 모델: 랜덤 포레스트 (예측 성능 비교 + 변수 중요도)
# - 점수 단위: Framingham 방식 (가장 작은 |β|를 1점)
# - 위험 그룹: 분포 33%/67% 백분위수
# - AUC 임계값: 양호 0.75 / 중단 0.65


# 패키지 설치 (최초 1회)
# install.packages(c("randomForest", "ranger"))

library(randomForest)   # 랜덤 포레스트


# =================================================================
# Part 1. 로지스틱 회귀 (주 모델 - 위험점수도구)
# =================================================================


# Step 1. 모델 학습 (Train)

# Train 데이터로 svyglm 회귀 (Stage A와 동일 14개 변수)
model_train <- svyglm(
  suicide_ideation ~ sex + one_person_hh + factor(education) +
    factor(income_quartile) + economic_active +
    factor(health_subj) + factor(body_image) +
    phq8 + stress_perc +
    high_risk_drink + current_smoker + walking +
    factor(sleep_quality) + dementia_family,
  design = design_train,
  family = quasibinomial()
)

cat("\n=== 로지스틱 회귀 Train 모델 학습 완료 ===\n")
summary(model_train)


# Step 2. 점수화 변환 (Framingham 방식)

# 회귀계수 추출 (Intercept 제외)
betas <- coef(model_train)
betas_no_intercept <- betas[-1]

cat("\n=== 회귀계수 (β) ===\n")
print(round(betas_no_intercept, 3))

# 기준 단위 = 가장 작은 |β| (0.05 이상만)
betas_meaningful <- betas_no_intercept[abs(betas_no_intercept) >= 0.05]
unit <- min(abs(betas_meaningful))
cat("\n기준 단위 (1점 = β):", round(unit, 4), "\n")

# 점수 변환
scores <- round(betas_no_intercept / unit)
cat("\n=== 정수 점수 변환 ===\n")
print(scores)

# 점수표 정리
score_table <- data.frame(
  변수 = names(betas_no_intercept),
  베타 = round(betas_no_intercept, 3),
  점수 = scores
)
cat("\n=== 점수표 ===\n")
print(score_table, row.names = FALSE)


# Step 3. Train·Test 점수 계산

# 점수 계산 함수
calc_score <- function(data, model, unit) {
  # 회귀 예측 (link scale, 즉 logit)
  linear_pred <- predict(model, newdata = data, type = "link")
  
  # 정수 점수 변환 (linear predictor / unit, intercept 제외)
  data$risk_score <- round(as.numeric(linear_pred - coef(model)[1]) / unit)
  
  # 예측 확률
  data$pred_prob_logit <- predict(model, newdata = data, type = "response")
  
  return(data)
}

# Train 점수 계산
train_data <- calc_score(train_data, model_train, unit)

# Test 점수 계산
test_data <- calc_score(test_data, model_train, unit)

cat("\n=== Train 점수 분포 ===\n")
summary(train_data$risk_score)

cat("\n=== Test 점수 분포 ===\n")
summary(test_data$risk_score)


# Step 4. 위험 그룹 분류 (33%/67% cutpoint)

# Train 점수 기준 cutpoint 결정
cutpoints <- quantile(train_data$risk_score,
                      probs = c(0.33, 0.67),
                      na.rm = TRUE)

cat("\n=== Cutpoint (Train 기준) ===\n")
cat("33% 백분위수:", cutpoints[1], "\n")
cat("67% 백분위수:", cutpoints[2], "\n")

# Train·Test에 위험 그룹 부여
train_data$risk_group <- cut(train_data$risk_score,
                              breaks = c(-Inf, cutpoints[1], cutpoints[2], Inf),
                              labels = c("저위험", "중위험", "고위험"))

test_data$risk_group <- cut(test_data$risk_score,
                             breaks = c(-Inf, cutpoints[1], cutpoints[2], Inf),
                             labels = c("저위험", "중위험", "고위험"))

# 그룹별 자살생각률 (Train)
cat("\n=== Train 그룹별 자살생각률 ===\n")
train_group_rates <- aggregate(suicide_ideation ~ risk_group,
                                data = train_data,
                                FUN = function(x) {
                                  c(N = length(x),
                                    자살생각자 = sum(x),
                                    비율 = round(mean(x) * 100, 2))
                                })
print(train_group_rates)

# 그룹별 자살생각률 (Test)
cat("\n=== Test 그룹별 자살생각률 ===\n")
test_group_rates <- aggregate(suicide_ideation ~ risk_group,
                               data = test_data,
                               FUN = function(x) {
                                 c(N = length(x),
                                   자살생각자 = sum(x),
                                   비율 = round(mean(x) * 100, 2))
                               })
print(test_group_rates)


# Step 5. 로지스틱 모델 평가 (Test)

# AUC 계산
roc_logit <- roc(test_data$suicide_ideation, test_data$pred_prob_logit,
                 quiet = TRUE)
auc_logit <- auc(roc_logit)
auc_logit_ci <- ci.auc(roc_logit)

cat("\n=== 로지스틱 회귀 AUC (Test) ===\n")
cat("AUC =", round(auc_logit, 3), "\n")
cat("95% CI:", round(auc_logit_ci[1], 3), "-", round(auc_logit_ci[3], 3), "\n")

# AUC 임계값 판단
if (auc_logit >= 0.75) {
  cat("→ 양호 수준 (AUC ≥ 0.75)\n")
} else if (auc_logit >= 0.65) {
  cat("→ 수용 가능 수준 (0.65 ≤ AUC < 0.75)\n")
} else {
  cat("→ 임상 유용성 부족 (AUC < 0.65)\n")
}


# Step 6. 민감도·특이도 (고위험 vs 저·중위험)

test_data$predicted_positive <- ifelse(test_data$risk_group == "고위험", 1, 0)

# 혼동 행렬
confusion_logit <- table(
  실제 = test_data$suicide_ideation,
  예측 = test_data$predicted_positive
)
cat("\n=== 로지스틱 - 혼동 행렬 ===\n")
print(confusion_logit)

# 진단 지표
TP <- confusion_logit["1", "1"]
FN <- confusion_logit["1", "0"]
FP <- confusion_logit["0", "1"]
TN <- confusion_logit["0", "0"]

sens_logit <- TP / (TP + FN)
spec_logit <- TN / (TN + FP)
ppv_logit  <- TP / (TP + FP)
npv_logit  <- TN / (TN + FN)

cat("\n=== 로지스틱 - 진단 성능 ===\n")
cat("민감도:", round(sens_logit * 100, 2), "%\n")
cat("특이도:", round(spec_logit * 100, 2), "%\n")
cat("양성예측도:", round(ppv_logit * 100, 2), "%\n")
cat("음성예측도:", round(npv_logit * 100, 2), "%\n")


# =================================================================
# Part 2. 랜덤 포레스트 (보조 모델 - 예측 성능 비교)
# =================================================================


# Step 7. 랜덤 포레스트 학습 (Train)

# 주의: RF는 복합표본 가중치 미적용 (학부 수준)
# 변수를 factor로 변환 (RF에서 범주형 처리)
train_rf <- train_data
test_rf  <- test_data

# 종속변수를 factor로
train_rf$suicide_ideation <- factor(train_rf$suicide_ideation,
                                     levels = c(0, 1),
                                     labels = c("No", "Yes"))
test_rf$suicide_ideation <- factor(test_rf$suicide_ideation,
                                    levels = c(0, 1),
                                    labels = c("No", "Yes"))

# 범주형 변수를 factor로 (RF에서 더 잘 처리)
factor_vars <- c("sex", "one_person_hh", "education", "income_quartile",
                 "economic_active", "health_subj", "body_image",
                 "stress_perc", "high_risk_drink", "current_smoker",
                 "walking", "sleep_quality", "dementia_family")

for (v in factor_vars) {
  train_rf[[v]] <- as.factor(train_rf[[v]])
  test_rf[[v]]  <- as.factor(test_rf[[v]])
}

# cutoff 0.03~0.05 시도
cutoffs <- c(0.02, 0.025, 0.03, 0.035, 0.04, 0.045, 0.05)
results <- data.frame()

for (cut in cutoffs) {
  pred_class <- ifelse(test_rf$pred_prob_rf >= cut, "Yes", "No")
  pred_class <- factor(pred_class, levels = c("No", "Yes"))
  
  cm <- table(test_rf$suicide_ideation, pred_class)
  TP <- cm["Yes", "Yes"]
  FN <- cm["Yes", "No"]
  FP <- cm["No", "Yes"]
  TN <- cm["No", "No"]
  
  results <- rbind(results, data.frame(
    cutoff = cut,
    민감도 = round(TP/(TP+FN)*100, 2),
    특이도 = round(TN/(TN+FP)*100, 2),
    PPV = round(TP/(TP+FP)*100, 2)
  ))
}

print(results)

grid <- expand.grid(
  mtry = c(3, 4, 5, 6),
  nodesize = c(1, 5, 10, 20),
  cutoff_yes = c(0.03, 0.04, 0.05)
)

results_grid <- data.frame()

for (i in 1:nrow(grid)) {
  set.seed(2026)
  
  m <- randomForest(
    suicide_ideation ~ sex + one_person_hh + education +
      income_quartile + economic_active +
      health_subj + body_image +
      phq8 + stress_perc +
      high_risk_drink + current_smoker + walking +
      sleep_quality + dementia_family,
    data = train_rf,
    ntree = 500,
    mtry = grid$mtry[i],
    nodesize = grid$nodesize[i],
    cutoff = c(1 - grid$cutoff_yes[i], grid$cutoff_yes[i])
  )
  
  pred <- predict(m, newdata = test_rf, type = "response")
  cm <- table(test_rf$suicide_ideation, pred)
  TP <- cm["Yes", "Yes"]
  FN <- cm["Yes", "No"]
  FP <- cm["No", "Yes"]
  TN <- cm["No", "No"]
  
  results_grid <- rbind(results_grid, data.frame(
    mtry = grid$mtry[i],
    nodesize = grid$nodesize[i],
    cutoff_yes = grid$cutoff_yes[i],
    민감도 = round(TP/(TP+FN)*100, 2),
    특이도 = round(TN/(TN+FP)*100, 2),
    PPV = round(TP/(TP+FP)*100, 2),
    Youden = round(TP/(TP+FN) + TN/(TN+FP) - 1, 4)
  ))
  
  cat("진행:", i, "/", nrow(grid), "\n")
}


# 결과 정렬 (민감도 80% 이상 + 특이도 높은 순)
results_grid_filtered <- results_grid[results_grid$민감도 >= 70, ]
results_grid_filtered <- results_grid_filtered[order(-results_grid_filtered$특이도), ]
print(results_grid_filtered)



# 랜덤 포레스트 학습
set.seed(2026)

model_rf <- randomForest(
  suicide_ideation ~ sex + one_person_hh + education +
    income_quartile + economic_active +
    health_subj + body_image +
    phq8 + stress_perc +
    high_risk_drink + current_smoker + walking +
    sleep_quality + dementia_family,
  data = train_rf,
  ntree = 500,           # 트리 500개
  mtry = 5,              # gird 방식 사용한 최적의 조합(민감도가 제일 높음)
  nodesize = 1,
  cutoff = c(0.97, 0.03),
  importance = TRUE      # 변수 중요도 산출
)

cat("\n=== 랜덤 포레스트 학습 완료 ===\n")
print(model_rf)


# Step 8. 변수 중요도 (Variable Importance)

importance_rf <- importance(model_rf)
cat("\n=== 변수 중요도 ===\n")
print(round(importance_rf, 2))

# 중요도 시각화
png("rf_importance.png", width = 800, height = 600)
varImpPlot(model_rf,
           main = "Random Forest 변수 중요도",
           pch = 19,
           col = "darkblue")
dev.off()
cat("\n변수 중요도 그래프 저장: rf_importance.png\n")


# Step 9. 랜덤 포레스트 평가 (Test)

# 예측 확률 (Yes 클래스 확률)
test_rf$pred_prob_rf <- predict(model_rf, newdata = test_rf, type = "prob")[, "Yes"]

# AUC 계산
roc_rf <- roc(test_rf$suicide_ideation, test_rf$pred_prob_rf,
              quiet = TRUE)
auc_rf <- auc(roc_rf)
auc_rf_ci <- ci.auc(roc_rf)

cat("\n=== 랜덤 포레스트 AUC (Test) ===\n")
cat("AUC =", round(auc_rf, 3), "\n")
cat("95% CI:", round(auc_rf_ci[1], 3), "-", round(auc_rf_ci[3], 3), "\n")

# 예측 클래스 (기본 0.5 cutoff)
test_rf$pred_class_rf <- predict(model_rf, newdata = test_rf, type = "response")

# 혼동 행렬
confusion_rf <- table(
  실제 = test_rf$suicide_ideation,
  예측 = test_rf$pred_class_rf
)
cat("\n=== 랜덤 포레스트 혼동 행렬 ===\n")
print(confusion_rf)

# 진단 지표
TP_rf <- confusion_rf["Yes", "Yes"]
FN_rf <- confusion_rf["Yes", "No"]
FP_rf <- confusion_rf["No", "Yes"]
TN_rf <- confusion_rf["No", "No"]

sens_rf <- TP_rf / (TP_rf + FN_rf)
spec_rf <- TN_rf / (TN_rf + FP_rf)
ppv_rf  <- TP_rf / (TP_rf + FP_rf)
npv_rf  <- TN_rf / (TN_rf + FN_rf)

cat("\n=== 랜덤 포레스트 진단 성능 ===\n")
cat("민감도:", round(sens_rf * 100, 2), "%\n")
cat("특이도:", round(spec_rf * 100, 2), "%\n")
cat("양성예측도:", round(ppv_rf * 100, 2), "%\n")
cat("음성예측도:", round(npv_rf * 100, 2), "%\n")


# =================================================================
# Part 3. 두 모델 비교
# =================================================================


# Step 10. AUC 비교

cat("\n=== AUC 비교 ===\n")
auc_comparison <- data.frame(
  모델       = c("로지스틱 회귀", "랜덤 포레스트"),
  AUC        = c(round(auc_logit, 3), round(auc_rf, 3)),
  CI_하한    = c(round(auc_logit_ci[1], 3), round(auc_rf_ci[1], 3)),
  CI_상한    = c(round(auc_logit_ci[3], 3), round(auc_rf_ci[3], 3)),
  민감도     = c(round(sens_logit * 100, 2), round(sens_rf * 100, 2)),
  특이도     = c(round(spec_logit * 100, 2), round(spec_rf * 100, 2))
)
print(auc_comparison, row.names = FALSE)


# Step 11. ROC 곡선 두 모델 함께 그리기

png("roc_comparison.png", width = 700, height = 600)
plot(roc_logit,
     main = "ROC Curve 비교",
     col = "blue",
     lwd = 2)
lines(roc_rf, col = "red", lwd = 2)
legend("bottomright",
       legend = c(paste0("로지스틱 (AUC=", round(auc_logit, 3), ")"),
                  paste0("랜덤 포레스트 (AUC=", round(auc_rf, 3), ")")),
       col = c("blue", "red"),
       lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()
cat("\nROC 비교 그래프 저장: roc_comparison.png\n")


# Step 12. 변수 중요도 vs 회귀계수 비교

# 랜덤 포레스트 변수 중요도 (MeanDecreaseGini)
rf_importance <- data.frame(
  변수 = rownames(importance_rf),
  RF_중요도 = round(importance_rf[, "MeanDecreaseGini"], 2)
)
rf_importance <- rf_importance[order(-rf_importance$RF_중요도), ]

# 로지스틱 회귀 절댓값 OR (변수별 최대값)
or_train <- exp(coef(model_train))[-1]
or_abs <- pmax(or_train, 1/or_train)  # 위험·보호 모두 절댓값

# 변수 단위로 묶기 (factor 변수는 최대값 사용)
get_var_name <- function(coef_name) {
  # factor(varname)2 같은 형태에서 varname만 추출
  gsub("factor\\(([^)]+)\\)[0-9]+", "\\1", coef_name)
}

logit_var_or <- aggregate(or_abs,
                           by = list(변수 = sapply(names(or_train), get_var_name)),
                           FUN = max)
names(logit_var_or)[2] <- "로지스틱_OR"

cat("\n=== 변수 중요도 비교 (로지스틱 vs RF) ===\n")
cat("로지스틱: 절댓값 OR (위험·보호 모두 1로부터의 거리)\n")
cat("RF: MeanDecreaseGini (높을수록 중요)\n\n")
print(rf_importance, row.names = FALSE)
cat("\n")
print(logit_var_or, row.names = FALSE)


# Step 13. Calibration (로지스틱 모델만 - 점수도구가 주 모델이라서)

test_data$pred_decile <- cut(test_data$pred_prob_logit,
                              breaks = quantile(test_data$pred_prob_logit,
                                                probs = seq(0, 1, 0.1)),
                              labels = 1:10,
                              include.lowest = TRUE)

calibration <- aggregate(
  cbind(예측확률 = pred_prob_logit, 실제비율 = suicide_ideation) ~ pred_decile,
  data = test_data,
  FUN = mean
)

cat("\n=== Calibration (10분위) - 로지스틱 ===\n")
print(round(calibration, 4))

# Calibration plot
png("calibration_plot.png", width = 600, height = 600)
plot(calibration$예측확률,
     calibration$실제비율,
     xlab = "예측 확률",
     ylab = "실제 자살생각률",
     main = "Calibration Plot (Test) - 로지스틱",
     pch = 19,
     col = "blue",
     xlim = c(0, max(calibration$실제비율, calibration$예측확률) * 1.1),
     ylim = c(0, max(calibration$실제비율, calibration$예측확률) * 1.1))
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()
cat("\nCalibration plot 저장: calibration_plot.png\n")


# =================================================================
# Step 14. 결과 저장
# =================================================================


# 모델 객체
saveRDS(model_train,  "model_train_logit.rds")
saveRDS(model_rf,     "model_train_rf.rds")
saveRDS(roc_logit,    "roc_logit.rds")
saveRDS(roc_rf,       "roc_rf.rds")

# 점수표 + 결과
write.csv(score_table,         "score_table.csv",        row.names = FALSE)
write.csv(train_group_rates,   "train_group_rates.csv",  row.names = FALSE)
write.csv(test_group_rates,    "test_group_rates.csv",   row.names = FALSE)
write.csv(calibration,         "calibration.csv",        row.names = FALSE)
write.csv(auc_comparison,      "auc_comparison.csv",     row.names = FALSE)
write.csv(rf_importance,       "rf_importance.csv",      row.names = FALSE)
write.csv(logit_var_or,        "logit_var_or.csv",       row.names = FALSE)

# 진단 성능 요약
performance_summary <- data.frame(
  지표 = c("AUC", "AUC 하한", "AUC 상한",
           "민감도", "특이도", "양성예측도", "음성예측도"),
  로지스틱 = c(round(auc_logit, 3),
              round(auc_logit_ci[1], 3),
              round(auc_logit_ci[3], 3),
              round(sens_logit * 100, 2),
              round(spec_logit * 100, 2),
              round(ppv_logit * 100, 2),
              round(npv_logit * 100, 2)),
  랜덤포레스트 = c(round(auc_rf, 3),
                  round(auc_rf_ci[1], 3),
                  round(auc_rf_ci[3], 3),
                  round(sens_rf * 100, 2),
                  round(spec_rf * 100, 2),
                  round(ppv_rf * 100, 2),
                  round(npv_rf * 100, 2))
)
write.csv(performance_summary, "performance_summary.csv", row.names = FALSE)

cat("\n=== Stage B 완료 ===\n")
cat("\n[로지스틱 회귀 - 주 모델]\n")
cat("- score_table.csv (점수표)\n")
cat("- train_group_rates.csv / test_group_rates.csv\n")
cat("- calibration.csv\n")
cat("- calibration_plot.png\n")

cat("\n[랜덤 포레스트 - 보조 모델]\n")
cat("- rf_importance.csv (변수 중요도)\n")
cat("- rf_importance.png (변수 중요도 그래프)\n")

cat("\n[비교]\n")
cat("- auc_comparison.csv (AUC 비교)\n")
cat("- roc_comparison.png (ROC 곡선 비교)\n")
cat("- performance_summary.csv (진단 성능 종합)\n")
