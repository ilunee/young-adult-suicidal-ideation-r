
# 복합표본 카이제곱 검정 (H0: 청년·노년 자살생각률은 차이가 없다.)
options(survey.lonely.psu = "adjust") # 단일 집락만 남은 것을 조절(0005) 

# 청년과 노년층만 남긴 데이터
young_old_data <- analysis_data %>%
  filter(age_group %in% c(1, 3))

table(young_old_data$age_group)


# 확인
table(analysis_data$age_group)
table(young_old_data$age_group)
table(young_old_data$suicide_ideation)

# 청년, 노년 데이터 기준으로 다시 가중치 적용
survey_young_old <- svydesign(
  data = young_old_data,
  strata = ~kstrata,
  weights = ~wt_p,
  ids = ~SPOT_NO,
  nest = TRUE
)

# 카이제곱 검정
x_result <- svychisq(
  ~suicide_ideation + age_group,
  survey_young_old
)

x_result
# [수정] p-value 해석 표현 정정
# p-value < 2.2e-16 -> α = 0.05 기준 매우 유의함
# 귀무가설 기각 & 대립가설 채택 -> 연령군에 따른 자살생각률 차이가 있다.

svyby(
  ~I(suicide_ideation == 1),
  ~age_group,
  survey_young_old,
  svymean,
  na.rm = TRUE
)
# 청년 자살생각률 : 5.5 % (0.05578161)  SE(표준오차) : 0.001313204
# 노년 자살생각률 : 10.2% (0.10202921)  SE(표준오차) : 0.001587536


# [추가] Cohen's h 계산 (사전등록한 핵심 판단 기준)
# 표본 23만 명에서 p값은 작은 차이도 매우 유의하게 나옴
# 효과크기(Cohen's h)로 실질적 차이의 크기를 평가해야 정확
p_youth <- 0.05578161
p_elder <- 0.10202921

cohen_h <- 2 * asin(sqrt(p_youth)) - 2 * asin(sqrt(p_elder))
abs_h   <- abs(cohen_h)

cat("\nCohen's h =", round(cohen_h, 4), "\n")
cat("|h| =", round(abs_h, 4), "\n")

# 해석 (Cohen 1988 기준)
if (abs_h < 0.2) {
  cat("Small effect (|h| < 0.2): 청년·노년 자살생각률 동등한 수준\n")
} else if (abs_h < 0.5) {
  cat("Medium effect (0.2 ≤ |h| < 0.5): 의미 있는 차이\n")
} else {
  cat("Large effect (|h| ≥ 0.5): 명확한 차이\n")
}


# [추가] 절대 차이와 비율도 함께 보고 (정직한 해석을 위해)
abs_diff <- p_elder - p_youth
cat("\n절대 차이 (노년 - 청년) =", round(abs_diff * 100, 2), "%p\n")

ratio <- p_youth / p_elder
cat("청년/노년 비율 =", round(ratio, 3), "\n")
