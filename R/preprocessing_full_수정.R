# Step 0. 환경 준비

install.packages(c("haven", "dplyr", "survey", "pwr", "pROC"))
install.packages("car")

# 패키지 로드
library(haven)     # SAS 파일 읽기
library(dplyr)     # 데이터 처리
library(survey)    # 복합표본분석
library(pwr)       # Cohen's h 계산
library(pROC)      # AUC 평가

# Step 1. 데이터 로드 + 기본 검증

# SAS 파일 로드 (시간 소요됨, 1-3분)
raw_data <- read_sas("chs24_all.sas7bdat")

# 데이터 차원 확인
cat("데이터 차원:", nrow(raw_data), "행 ×", ncol(raw_data), "열\n")

# 표본설계 변수 존재 확인 (SPOT_NO 대문자 주의)
design_vars <- c("kstrata", "SPOT_NO", "wt_p")
cat("\n표본설계 변수 존재 확인:\n")
for (v in design_vars) {
  cat(sprintf("  %s: %s\n", v, ifelse(v %in% names(raw_data), "존재", "없음")))
}

# 핵심 변수 존재 여부 확인
core_vars <- c(
  "mtd_01z1",
  "sex", "age", "mbhld_co", "sob_01z1", "sob_02z1",
  "fma_12z1", "fma_13z1", "fma_14z1", "soa_01z1",
  "qoa_01z1", "oba_01z1",
  "mta_01z1",
  paste0("mtb_07", letters[1:8], "1"),
  "dra_01z1", "drb_01z3", "drb_03z1",
  "smf_01z1", "sma_01z1", "sma_03z2",
  "phb_01z1", "phb_02z1", "phb_03z1",
  "mtc_13z1", "fma_27z1"
)

missing_vars <- core_vars[!core_vars %in% names(raw_data)]
if (length(missing_vars) > 0) {
  cat("\n[경고] 누락된 변수:\n")
  print(missing_vars)
} else {
  cat("\n모든 핵심 변수 존재 확인 완료\n")
}


# Step 2. 분석용 데이터셋 분리 (원본 보존)

# raw_data는 절대 수정 X, work_data로 복사해서 작업
work_data <- raw_data %>%
  select(all_of(c(design_vars, core_vars)))

cat("작업용 데이터셋:", nrow(work_data), "행 ×", ncol(work_data), "열\n")


# Step 3. 무효값을 NA로 변환

# 종속변수
work_data$mtd_01z1[work_data$mtd_01z1 %in% c(7, 9)] <- NA

# 인구사회 (sex, age, mbhld_co는 결측 없음)
work_data$sob_01z1[work_data$sob_01z1 %in% c(77, 99)] <- NA
work_data$sob_02z1[work_data$sob_02z1 %in% c(7, 9)] <- NA

# 경제
work_data$fma_12z1[work_data$fma_12z1 %in% c(7, 9)] <- NA
work_data$fma_13z1[work_data$fma_13z1 %in% c(77777, 99999)] <- NA
work_data$fma_14z1[work_data$fma_14z1 %in% c(77777, 99999)] <- NA
work_data$soa_01z1[work_data$soa_01z1 %in% c(7, 9)] <- NA

# 건강인식
work_data$qoa_01z1[work_data$qoa_01z1 %in% c(7, 9)] <- NA
work_data$oba_01z1[work_data$oba_01z1 %in% c(7, 9)] <- NA

# 정신건강
work_data$mta_01z1[work_data$mta_01z1 %in% c(7, 9)] <- NA

# PHQ-8 8문항
phq8_vars <- paste0("mtb_07", letters[1:8], "1")
for (v in phq8_vars) {
  work_data[[v]][work_data[[v]] %in% c(7, 9)] <- NA
}

# 건강행태 보조변수
work_data$dra_01z1[work_data$dra_01z1 %in% c(7, 9)] <- NA
work_data$drb_01z3[work_data$drb_01z3 %in% c(7, 9)] <- NA
work_data$drb_03z1[work_data$drb_03z1 %in% c(7, 9)] <- NA
work_data$smf_01z1[work_data$smf_01z1 %in% c(7, 9)] <- NA
work_data$sma_01z1[work_data$sma_01z1 %in% c(7, 9)] <- NA
work_data$sma_03z2[work_data$sma_03z2 %in% c(7, 9)] <- NA

# phb_01z1: 0-7 모두 정상값 (처리 불필요)
work_data$phb_02z1[work_data$phb_02z1 %in% c(77, 99)] <- NA
work_data$phb_03z1[work_data$phb_03z1 %in% c(77, 99)] <- NA

# 수면·가족
work_data$mtc_13z1[work_data$mtc_13z1 %in% c(7, 9)] <- NA
work_data$fma_27z1[work_data$fma_27z1 %in% c(7, 9)] <- NA


# Step 4. 변수 처리 - 인구사회

# youth_subgroup 변수 제거 (청년 전체 분석으로 결정, 20대/30대 분리 X)
work_data <- work_data %>%
  mutate(
    # 연령군 3분류 (청년/중년/노년)
    age_group = case_when(
      age >= 19 & age <= 39 ~ 1L,
      age >= 40 & age <= 64 ~ 2L,
      age >= 65             ~ 3L,
      TRUE                  ~ NA_integer_
    ),
    
    # 1인가구 이분형
    one_person_hh = case_when(
      mbhld_co == 1 ~ 1L,
      mbhld_co >= 2 ~ 0L,
      TRUE          ~ NA_integer_
    ),
    
    # 교육수준 3분류 (이용지침서 educ 정의)
    education = case_when(
      sob_01z1 %in% 1:4                          ~ 1L,
      sob_01z1 %in% 5:8 & sob_02z1 == 1          ~ 2L,
      sob_01z1 %in% 5:8 & sob_02z1 %in% c(2,3,4) ~ 3L,
      TRUE                                        ~ NA_integer_
    )
  )


# Step 5. 변수 처리 - 경제

# 가구소득 통합 (연 응답은 12로 나눔, 월 응답은 그대로)
work_data <- work_data %>%
  mutate(
    income_monthly = case_when(
      fma_12z1 == 1 & !is.na(fma_13z1) ~ fma_13z1 / 12,
      fma_12z1 == 2 & !is.na(fma_14z1) ~ fma_14z1,
      TRUE                              ~ NA_real_
    )
  )

# 가구소득 4분위 변환
income_breaks <- quantile(work_data$income_monthly, 
                           probs = c(0, 0.25, 0.5, 0.75, 1), 
                           na.rm = TRUE)

work_data <- work_data %>%
  mutate(
    income_quartile = cut(income_monthly,
                           breaks = income_breaks,
                           labels = c(1, 2, 3, 4),
                           include.lowest = TRUE) %>% as.integer()
  )

# 경제활동 이분형 (1=예, 0=아니오)
work_data <- work_data %>%
  mutate(
    economic_active = case_when(
      soa_01z1 == 1 ~ 1L,
      soa_01z1 == 2 ~ 0L,
      TRUE          ~ NA_integer_
    )
  )


# Step 6. 변수 처리 - 건강 인식

work_data <- work_data %>%
  mutate(
    # 주관적 건강 3분류
    health_subj = case_when(
      qoa_01z1 %in% c(1, 2) ~ 1L,
      qoa_01z1 == 3         ~ 2L,
      qoa_01z1 %in% c(4, 5) ~ 3L,
      TRUE                  ~ NA_integer_
    ),
    
    # 본인 인지 체형 3분류
    body_image = case_when(
      oba_01z1 %in% c(1, 2) ~ 1L,
      oba_01z1 == 3         ~ 2L,
      oba_01z1 %in% c(4, 5) ~ 3L,
      TRUE                  ~ NA_integer_
    )
  )


# Step 7. 변수 처리 - 정신건강

# PHQ-8 산출 (1-4 응답을 0-3으로 변환 후 합산)
# 빠른 벡터 연산 사용 (rowwise 안 씀)
phq8_temp <- as.matrix(work_data[, phq8_vars]) - 1   # 1-4 → 0-3

work_data$phq8 <- ifelse(
  rowSums(is.na(phq8_temp)) > 0,                      # NA 하나라도 있으면
  NA_real_,                                            # NA
  rowSums(phq8_temp)                                   # 아니면 합계
)

# PHQ-8 이분형 (≥10점 = 임상적 우울)
work_data <- work_data %>%
  mutate(
    phq8_clinical = case_when(
      phq8 >= 10              ~ 1L,
      phq8 < 10 & !is.na(phq8) ~ 0L,
      TRUE                     ~ NA_integer_
    )
  )

# 스트레스인지율 (mt_a0100 이용지침서 정의)
work_data <- work_data %>%
  mutate(
    stress_perc = case_when(
      mta_01z1 %in% c(1, 2) ~ 1L,
      mta_01z1 %in% c(3, 4) ~ 0L,
      TRUE                  ~ NA_integer_
    )
  )


# Step 8. 변수 처리 - 건강행태 (이용지침서 파생변수)

# dr_a0500 고위험음주율
work_data <- work_data %>%
  mutate(
    high_risk_drink = case_when(
      # 평생 음주 안 함
      dra_01z1 == 2 ~ 0L,
      # 1년간 안 마심
      dra_01z1 == 1 & drb_01z3 == 1 ~ 0L,
      # 낮은 빈도 (월 1회 미만 ~ 주 1회)
      dra_01z1 == 1 & drb_01z3 %in% 2:4 ~ 0L,
      # 주 2회 이상, 남자, 6잔 이하
      dra_01z1 == 1 & drb_01z3 %in% 5:6 & sex == 1 & drb_03z1 %in% 1:3 ~ 0L,
      # 주 2회 이상, 남자, 7잔 이상 = 고위험
      dra_01z1 == 1 & drb_01z3 %in% 5:6 & sex == 1 & drb_03z1 %in% 4:5 ~ 1L,
      # 주 2회 이상, 여자, 4잔 이하
      dra_01z1 == 1 & drb_01z3 %in% 5:6 & sex == 2 & drb_03z1 %in% 1:2 ~ 0L,
      # 주 2회 이상, 여자, 5잔 이상 = 고위험
      dra_01z1 == 1 & drb_01z3 %in% 5:6 & sex == 2 & drb_03z1 %in% 3:5 ~ 1L,
      TRUE ~ NA_integer_
    )
  )

# sm_a0100 현재흡연율
work_data <- work_data %>%
  mutate(
    current_smoker = case_when(
      # 평생 사용 안 함
      smf_01z1 == 2 ~ 0L,
      # 5갑 미만 또는 안 피움
      smf_01z1 == 1 & sma_01z1 %in% c(1, 3) ~ 0L,
      # 5갑 이상 + 현재 매일/가끔 = 흡연자
      smf_01z1 == 1 & sma_01z1 == 2 & sma_03z2 %in% c(1, 2) ~ 1L,
      # 5갑 이상 + 과거 흡연
      smf_01z1 == 1 & sma_01z1 == 2 & sma_03z2 == 3 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# ph_b0200 걷기실천율
# 1단계: 걷기 시간(분) 계산
work_data <- work_data %>%
  mutate(
    walk_minutes = case_when(
      !is.na(phb_02z1) & !is.na(phb_03z1) ~ phb_02z1 * 60 + phb_03z1,
      !is.na(phb_02z1) & is.na(phb_03z1)  ~ phb_02z1 * 60,
      is.na(phb_02z1) & !is.na(phb_03z1)  ~ phb_03z1,
      TRUE                                 ~ NA_real_
    )
  )

# 2단계: 걷기실천율 판정 (주 5일 이상 + 30분 이상)
work_data <- work_data %>%
  mutate(
    walking = case_when(
      phb_01z1 %in% 0:4                          ~ 0L,
      phb_01z1 %in% 5:7 & walk_minutes < 30      ~ 0L,
      phb_01z1 %in% 5:7 & walk_minutes >= 30     ~ 1L,
      TRUE                                        ~ NA_integer_
    )
  )


# Step 9. 변수 처리 - 수면·가족

work_data <- work_data %>%
  mutate(
    # 수면의 질 3분류
    sleep_quality = case_when(
      mtc_13z1 %in% c(1, 2) ~ 1L,
      mtc_13z1 == 3         ~ 2L,
      mtc_13z1 %in% c(4, 5) ~ 3L,
      TRUE                  ~ NA_integer_
    ),
    
    # 치매가족 이분형
    dementia_family = case_when(
      fma_27z1 == 1 ~ 1L,
      fma_27z1 == 2 ~ 0L,
      TRUE          ~ NA_integer_
    )
  )

# 자살생각 종속변수 0/1로 재코딩
work_data <- work_data %>%
  mutate(
    suicide_ideation = case_when(
      mtd_01z1 == 1 ~ 1L,
      mtd_01z1 == 2 ~ 0L,
      TRUE          ~ NA_integer_
    )
  )


# Step 10. 분석 데이터셋 최종 정리

# youth_subgroup 제거 (청년 전체 분석으로 결정)
analysis_data <- work_data %>%
  select(
    # 표본설계
    kstrata, SPOT_NO, wt_p,
    # 종속변수
    suicide_ideation,
    # 인구사회
    sex, age, age_group, one_person_hh, education,
    # 경제
    income_quartile, economic_active,
    # 건강인식
    health_subj, body_image,
    # 정신건강
    phq8, phq8_clinical, stress_perc,
    # 건강행태
    high_risk_drink, current_smoker, walking,
    # 수면·가족
    sleep_quality, dementia_family
  )

cat("분석용 데이터셋:", nrow(analysis_data), "행 ×", ncol(analysis_data), "열\n")


# Step 11. 명시적 listwise 제거

# 19세 미만 제외 (KCHS는 19세+이지만 안전장치)
analysis_data <- analysis_data %>%
  filter(age >= 19)

# 표본 크기 흐름 기록 (보고서·발표용)
n_total <- nrow(analysis_data)
cat("19세 이상 표본 N:", n_total, "\n")

# 모든 변수 유효한 행만 추출
analysis_data_complete <- analysis_data %>%
  filter(complete.cases(.))

n_complete <- nrow(analysis_data_complete)
cat("Listwise 후 N:", n_complete, "\n")
cat("보존율:", round(n_complete / n_total * 100, 2), "%\n")

# 청년·중년·노년 표본 크기
cat("\n연령군별 listwise 후 N:\n")
print(table(analysis_data_complete$age_group, useNA = "ifany"))


# Step 12. 다중공선성 사전 검정 (VIF)

# car::vif() 사용 시 GVIF 정확한 결과 산출
# car 미설치 시 VIF 검정 생략 (기존 검증 결과 사용)
#
# 어제 car::vif() 검증 결과 (이미 확인 완료):
# - 청년·중년·노년 모두에서 모든 변수 GVIF 1.0 ~ 1.5
# - VIF 임계값 10에 한참 미달
# - 다중공선성 문제 없음 → 추가 변수 처리 불필요

  # 청년 표본 VIF
  youth_for_vif <- analysis_data_complete %>%
    filter(age_group == 1)
  
  vif_model_youth <- glm(
    suicide_ideation ~ sex + one_person_hh + factor(education) +
      factor(income_quartile) + economic_active +
      factor(health_subj) + factor(body_image) +
      phq8 + stress_perc +
      high_risk_drink + current_smoker + walking +
      factor(sleep_quality) + dementia_family,
    data = youth_for_vif,
    family = binomial()
  )
  
  vif_results_youth <- car::vif(vif_model_youth)
  cat("\n=== VIF 청년 (GVIF) ===\n")
  print(round(vif_results_youth, 3))
  
  
  # 중년 표본 VIF
  middle_for_vif <- analysis_data_complete %>%
    filter(age_group == 2)
  
  vif_model_middle <- glm(
    suicide_ideation ~ sex + one_person_hh + factor(education) +
      factor(income_quartile) + economic_active +
      factor(health_subj) + factor(body_image) +
      phq8 + stress_perc +
      high_risk_drink + current_smoker + walking +
      factor(sleep_quality) + dementia_family,
    data = middle_for_vif,
    family = binomial()
  )
  
  vif_results_middle <- car::vif(vif_model_middle)
  cat("\n=== VIF 중년 (GVIF) ===\n")
  print(round(vif_results_middle, 3))
  
  
  # 노년 표본 VIF
  elder_for_vif <- analysis_data_complete %>%
    filter(age_group == 3)
  
  vif_model_elder <- glm(
    suicide_ideation ~ sex + one_person_hh + factor(education) +
      factor(income_quartile) + economic_active +
      factor(health_subj) + factor(body_image) +
      phq8 + stress_perc +
      high_risk_drink + current_smoker + walking +
      factor(sleep_quality) + dementia_family,
    data = elder_for_vif,
    family = binomial()
  )
  
  vif_results_elder <- car::vif(vif_model_elder)
  cat("\n=== VIF 노년 (GVIF) ===\n")
  print(round(vif_results_elder, 3))
  



# Step 13. 전처리 결과 검증

# 1차 검증: 변수 분포 자동 확인
cat("\n[PHQ-8 분포]\n")
summary(analysis_data_complete$phq8)

cat("\n[자살생각 분포]\n")
print(table(analysis_data_complete$suicide_ideation, useNA = "ifany"))

cat("\n[1인가구 분포]\n")
print(table(analysis_data_complete$one_person_hh, useNA = "ifany"))

cat("\n[교육수준 분포]\n")
print(table(analysis_data_complete$education, useNA = "ifany"))

cat("\n[고위험음주 분포]\n")
print(table(analysis_data_complete$high_risk_drink, useNA = "ifany"))

cat("\n[현재흡연 분포]\n")
print(table(analysis_data_complete$current_smoker, useNA = "ifany"))

cat("\n[걷기실천 분포]\n")
print(table(analysis_data_complete$walking, useNA = "ifany"))

# 2차 검증: 무작위 5명 추출 (수동 검증용)
set.seed(2026)
sample_check <- analysis_data_complete %>%
  sample_n(5)
cat("\n[무작위 5명 표본 - 수동 검증용]\n")
print(sample_check)

# 분석 데이터셋 저장
saveRDS(analysis_data_complete, "analysis_data_complete.rds")


# Step 14. Stage A - 복합표본 설계 정의

# 단일 집락 조정 (KCHS 복합표본 특성)
options(survey.lonely.psu = "adjust")

# 전체 design 객체
design_full <- svydesign(
  ids = ~SPOT_NO,
  strata = ~kstrata,
  weights = ~wt_p,
  data = analysis_data_complete,
  nest = TRUE
)

# 청년·중년·노년 부분집합
design_youth  <- subset(design_full, age_group == 1)
design_middle <- subset(design_full, age_group == 2)
design_elder  <- subset(design_full, age_group == 3)

cat("\n복합표본 design 정의 완료\n")
cat("청년 design N:",  nrow(design_youth$variables),  "\n")
cat("중년 design N:",  nrow(design_middle$variables), "\n")
cat("노년 design N:",  nrow(design_elder$variables),  "\n")

# Design 저장
saveRDS(design_full,   "design_full.rds")
saveRDS(design_youth,  "design_youth.rds")
saveRDS(design_middle, "design_middle.rds")
saveRDS(design_elder,  "design_elder.rds")


# Step 15. Stage B - 청년 부분집합 추출

youth_data <- analysis_data_complete %>%
  filter(age_group == 1)

cat("\n청년 표본 N:", nrow(youth_data), "\n")
cat("청년 자살생각자 N:", sum(youth_data$suicide_ideation == 1), "\n")
cat("청년 자살생각률:", 
    round(mean(youth_data$suicide_ideation) * 100, 2), "%\n")


# Step 16. Stage B - Train/Test 분할

# rsample 안 쓰고 base R sample()로 stratified split

# set.seed(2026) - 재현성 보장
set.seed(2026)

# 자살생각 1과 0 분리
youth_pos <- youth_data %>% filter(suicide_ideation == 1)
youth_neg <- youth_data %>% filter(suicide_ideation == 0)

# 각각 70%씩 추출 (자살생각 비율 유지 = stratified)
train_pos_idx <- sample(nrow(youth_pos), size = floor(0.7 * nrow(youth_pos)))
train_neg_idx <- sample(nrow(youth_neg), size = floor(0.7 * nrow(youth_neg)))

# Train·Test 데이터 합치기
train_data <- rbind(
  youth_pos[train_pos_idx, ],
  youth_neg[train_neg_idx, ]
)

test_data <- rbind(
  youth_pos[-train_pos_idx, ],
  youth_neg[-train_neg_idx, ]
)

# 분할 결과 확인
cat("\nTrain N:", nrow(train_data), "\n")
cat("Test N:",  nrow(test_data),  "\n")

# 자살생각 비율 비교 (거의 동일해야 함)
cat("Train 자살생각률:", 
    round(mean(train_data$suicide_ideation) * 100, 2), "%\n")
cat("Test 자살생각률:",  
    round(mean(test_data$suicide_ideation)  * 100, 2), "%\n")


# Step 17. Stage B - 복합표본 설계 정의 (Train·Test)

# Train design
design_train <- svydesign(
  ids = ~SPOT_NO,
  strata = ~kstrata,
  weights = ~wt_p,
  data = train_data,
  nest = TRUE
)

# Test design
design_test <- svydesign(
  ids = ~SPOT_NO,
  strata = ~kstrata,
  weights = ~wt_p,
  data = test_data,
  nest = TRUE
)

cat("\nTrain·Test design 정의 완료\n")

# 저장
saveRDS(youth_data,   "youth_data.rds")
saveRDS(train_data,   "train_data.rds")
saveRDS(test_data,    "test_data.rds")
saveRDS(design_train, "design_train.rds")
saveRDS(design_test,  "design_test.rds")


# 전처리 완료
cat("\n전처리 전체 완료\n")
cat("Stage A: design_full / design_youth / design_middle / design_elder\n")
cat("Stage B: design_train / design_test\n")
