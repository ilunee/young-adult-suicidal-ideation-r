
# Stage A-2. 연령군별 영향요인 회귀 분석 (svyglm)
# 청년·중년·노년 각 집단에서 다변량 로지스틱 회귀

# 단일 집락 조정 (KCHS 복합표본 특성)
options(survey.lonely.psu = "adjust")


# 청년 회귀
model_youth <- svyglm(
  suicide_ideation ~ sex + one_person_hh + factor(education) +
    factor(income_quartile) + economic_active +
    factor(health_subj) + factor(body_image) +
    phq8 + stress_perc +
    high_risk_drink + current_smoker + walking +
    factor(sleep_quality) + dementia_family,
  design = design_youth,
  family = quasibinomial()
)

# 청년 회귀 결과 - 회귀계수 + p값
summary(model_youth)


# 청년 OR + 95% CI
or_youth <- exp(cbind(
  OR = coef(model_youth),
  confint(model_youth)
))
cat("\n=== 청년 OR + 95% CI ===\n")
print(round(or_youth, 3))


# 중년 회귀
model_middle <- svyglm(
  suicide_ideation ~ sex + one_person_hh + factor(education) +
    factor(income_quartile) + economic_active +
    factor(health_subj) + factor(body_image) +
    phq8 + stress_perc +
    high_risk_drink + current_smoker + walking +
    factor(sleep_quality) + dementia_family,
  design = design_middle,
  family = quasibinomial()
)

# 중년 회귀 결과
summary(model_middle)

# 중년 OR + 95% CI
or_middle <- exp(cbind(
  OR = coef(model_middle),
  confint(model_middle)
))
cat("\n=== 중년 OR + 95% CI ===\n")
print(round(or_middle, 3))


# 노년 회귀
model_elder <- svyglm(
  suicide_ideation ~ sex + one_person_hh + factor(education) +
    factor(income_quartile) + economic_active +
    factor(health_subj) + factor(body_image) +
    phq8 + stress_perc +
    high_risk_drink + current_smoker + walking +
    factor(sleep_quality) + dementia_family,
  design = design_elder,
  family = quasibinomial()
)

# 노년 회귀 결과
summary(model_elder)

# 노년 OR + 95% CI
or_elder <- exp(cbind(
  OR = coef(model_elder),
  confint(model_elder)
))
cat("\n=== 노년 OR + 95% CI ===\n")
print(round(or_elder, 3))


# 세 집단 비교표 (OR만)
or_compare <- data.frame(
  변수    = rownames(or_youth),
  청년_OR = round(or_youth[, 1], 3),
  중년_OR = round(or_middle[, 1], 3),
  노년_OR = round(or_elder[, 1], 3)
)
cat("\n=== 세 집단 OR 비교표 ===\n")
print(or_compare)


# 결과 저장
saveRDS(model_youth,  "model_youth.rds")
saveRDS(model_middle, "model_middle.rds")
saveRDS(model_elder,  "model_elder.rds")

write.csv(or_youth,   "or_youth.csv")
write.csv(or_middle,  "or_middle.csv")
write.csv(or_elder,   "or_elder.csv")
write.csv(or_compare, "or_compare.csv", row.names = FALSE)
