# Stage A 보강 분석
# 다중비교 보정 + 민감도 분석


# 다중비교 보정 - 청년 모델

# 청년 회귀에서 p값 추출
coef_summary_youth <- summary(model_youth)$coefficients
p_values_youth <- coef_summary_youth[, "Pr(>|t|)"]

# Intercept 제외 (관심 변수만)
p_values_youth <- p_values_youth[-1]

cat("\n=== 청년 - 원본 p값 ===\n")
print(round(p_values_youth, 4))

# Bonferroni 보정
p_bonf_youth <- p.adjust(p_values_youth, method = "bonferroni")
cat("\n=== 청년 - Bonferroni 보정 후 p값 ===\n")
print(round(p_bonf_youth, 4))

# FDR (Benjamini-Hochberg) 보정
p_fdr_youth <- p.adjust(p_values_youth, method = "BH")
cat("\n=== 청년 - FDR(BH) 보정 후 p값 ===\n")
print(round(p_fdr_youth, 4))

# 청년 종합표 (OR + p값 + 보정 p값)
youth_full <- data.frame(
  변수            = names(p_values_youth),
  OR              = round(or_youth[-1, 1], 3),
  CI_lower        = round(or_youth[-1, 2], 3),
  CI_upper        = round(or_youth[-1, 3], 3),
  p_원본          = round(p_values_youth, 4),
  p_Bonferroni    = round(p_bonf_youth, 4),
  p_FDR           = round(p_fdr_youth, 4),
  유의함_원본     = ifelse(p_values_youth < 0.05, "Yes", "No"),
  유의함_Bonf     = ifelse(p_bonf_youth < 0.05, "Yes", "No"),
  유의함_FDR      = ifelse(p_fdr_youth < 0.05, "Yes", "No")
)
cat("\n=== 청년 - 종합 결과표 ===\n")
print(youth_full, row.names = T)


# 다중비교 보정 - 중년 모델

coef_summary_middle <- summary(model_middle)$coefficients
p_values_middle <- coef_summary_middle[, "Pr(>|t|)"][-1]

p_bonf_middle <- p.adjust(p_values_middle, method = "bonferroni")
p_fdr_middle  <- p.adjust(p_values_middle, method = "BH")

middle_full <- data.frame(
  변수            = names(p_values_middle),
  OR              = round(or_middle[-1, 1], 3),
  CI_lower        = round(or_middle[-1, 2], 3),
  CI_upper        = round(or_middle[-1, 3], 3),
  p_원본          = round(p_values_middle, 4),
  p_Bonferroni    = round(p_bonf_middle, 4),
  p_FDR           = round(p_fdr_middle, 4),
  유의함_원본     = ifelse(p_values_middle < 0.05, "Yes", "No"),
  유의함_Bonf     = ifelse(p_bonf_middle < 0.05, "Yes", "No"),
  유의함_FDR      = ifelse(p_fdr_middle < 0.05, "Yes", "No")
)
cat("\n=== 중년 - 종합 결과표 ===\n")
print(middle_full, row.names = FALSE)


# 다중비교 보정 - 노년 모델

coef_summary_elder <- summary(model_elder)$coefficients
p_values_elder <- coef_summary_elder[, "Pr(>|t|)"][-1]

p_bonf_elder <- p.adjust(p_values_elder, method = "bonferroni")
p_fdr_elder  <- p.adjust(p_values_elder, method = "BH")

elder_full <- data.frame(
  변수            = names(p_values_elder),
  OR              = round(or_elder[-1, 1], 3),
  CI_lower        = round(or_elder[-1, 2], 3),
  CI_upper        = round(or_elder[-1, 3], 3),
  p_원본          = round(p_values_elder, 4),
  p_Bonferroni    = round(p_bonf_elder, 4),
  p_FDR           = round(p_fdr_elder, 4),
  유의함_원본     = ifelse(p_values_elder < 0.05, "Yes", "No"),
  유의함_Bonf     = ifelse(p_bonf_elder < 0.05, "Yes", "No"),
  유의함_FDR      = ifelse(p_fdr_elder < 0.05, "Yes", "No")
)
cat("\n=== 노년 - 종합 결과표 ===\n")
print(elder_full, row.names = FALSE)


# 결과 저장
write.csv(youth_full,  "youth_full_results.csv",  row.names = FALSE)
write.csv(middle_full, "middle_full_results.csv", row.names = FALSE)
write.csv(elder_full,  "elder_full_results.csv",  row.names = FALSE)


# ===============================
# 이 아래부터는 민감도 분석 입니다
# ===============================


# 민감도 분석 1. PHQ-8 제외 모델 (청년)

# PHQ-8이 가장 강력한 변수라서 다른 변수 효과 가렸을 가능성 검증
# PHQ-8 빼고 회귀 다시 돌림

model_youth_no_phq <- svyglm(
  suicide_ideation ~ sex + one_person_hh + factor(education) +
    factor(income_quartile) + economic_active +
    factor(health_subj) + factor(body_image) +
    stress_perc +
    high_risk_drink + current_smoker + walking +
    factor(sleep_quality) + dementia_family,
  design = design_youth,
  family = quasibinomial()
)

cat("\n=== 청년 - PHQ-8 제외 모델 ===\n")
summary(model_youth_no_phq)

# OR + 95% CI
or_youth_no_phq <- exp(cbind(
  OR = coef(model_youth_no_phq),
  confint(model_youth_no_phq)
))
cat("\n=== 청년 - PHQ-8 제외 OR ===\n")
print(round(or_youth_no_phq, 3))


# PHQ-8 포함 vs 제외 OR 비교 (청년)

# 두 모델 모두에 있는 변수만 비교
common_vars <- intersect(rownames(or_youth), rownames(or_youth_no_phq))
common_vars <- common_vars[common_vars != "(Intercept)"]

phq_compare_youth <- data.frame(
  변수             = common_vars,
  OR_PHQ포함       = round(or_youth[common_vars, 1], 3),
  OR_PHQ제외       = round(or_youth_no_phq[common_vars, 1], 3),
  변화량           = round(or_youth_no_phq[common_vars, 1] - 
                            or_youth[common_vars, 1], 3)
)
cat("\n=== 청년 - PHQ-8 포함 vs 제외 OR 비교 ===\n")
print(phq_compare_youth, row.names = FALSE)


# 민감도 분석 2. 성별 층화 분석 (청년 남성 vs 여성)

# 청년 남성 design
design_youth_male <- subset(design_youth, sex == 1)

# 청년 여성 design
design_youth_female <- subset(design_youth, sex == 2)

# 청년 남성 회귀
model_youth_male <- svyglm(
  suicide_ideation ~ one_person_hh + factor(education) +
    factor(income_quartile) + economic_active +
    factor(health_subj) + factor(body_image) +
    phq8 + stress_perc +
    high_risk_drink + current_smoker + walking +
    factor(sleep_quality) + dementia_family,
  design = design_youth_male,
  family = quasibinomial()
)

# 청년 여성 회귀
model_youth_female <- svyglm(
  suicide_ideation ~ one_person_hh + factor(education) +
    factor(income_quartile) + economic_active +
    factor(health_subj) + factor(body_image) +
    phq8 + stress_perc +
    high_risk_drink + current_smoker + walking +
    factor(sleep_quality) + dementia_family,
  design = design_youth_female,
  family = quasibinomial()
)

cat("\n=== 청년 남성 모델 ===\n")
summary(model_youth_male)

cat("\n=== 청년 여성 모델 ===\n")
summary(model_youth_female)


# 청년 남성 OR
or_youth_male <- exp(cbind(
  OR = coef(model_youth_male),
  confint(model_youth_male)
))
cat("\n=== 청년 남성 OR ===\n")
print(round(or_youth_male, 3))

# 청년 여성 OR
or_youth_female <- exp(cbind(
  OR = coef(model_youth_female),
  confint(model_youth_female)
))
cat("\n=== 청년 여성 OR ===\n")
print(round(or_youth_female, 3))


# 청년 남성 vs 여성 OR 비교

common_vars_sex <- intersect(rownames(or_youth_male), rownames(or_youth_female))
common_vars_sex <- common_vars_sex[common_vars_sex != "(Intercept)"]

sex_compare_youth <- data.frame(
  변수            = common_vars_sex,
  OR_남성         = round(or_youth_male[common_vars_sex, 1], 3),
  OR_여성         = round(or_youth_female[common_vars_sex, 1], 3),
  차이            = round(or_youth_female[common_vars_sex, 1] - 
                           or_youth_male[common_vars_sex, 1], 3)
)
cat("\n=== 청년 남성 vs 여성 OR 비교 ===\n")
print(sex_compare_youth, row.names = FALSE)


# 결과 저장
saveRDS(model_youth_no_phq,    "model_youth_no_phq.rds")
saveRDS(model_youth_male,      "model_youth_male.rds")
saveRDS(model_youth_female,    "model_youth_female.rds")

write.csv(phq_compare_youth,   "phq_compare_youth.csv",   row.names = FALSE)
write.csv(sex_compare_youth,   "sex_compare_youth.csv",   row.names = FALSE)
write.csv(or_youth_no_phq,     "or_youth_no_phq.csv")
write.csv(or_youth_male,       "or_youth_male.csv")
write.csv(or_youth_female,     "or_youth_female.csv")


# 분석 완료
cat("\n=== 다중비교 보정 + 민감도 분석 완료 ===\n")
cat("산출 파일:\n")
cat("- youth_full_results.csv (청년 종합 결과)\n")
cat("- middle_full_results.csv (중년 종합 결과)\n")
cat("- elder_full_results.csv (노년 종합 결과)\n")
cat("- phq_compare_youth.csv (PHQ-8 포함/제외 비교)\n")
cat("- sex_compare_youth.csv (청년 남성/여성 비교)\n")
