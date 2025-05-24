## 1. 데이터 불러오기 및 특성 파악 (Question 1)
SeoulBike<- read.csv("SeoulBikeData.csv")
str(SeoulBike)
summary(SeoulBike)

## 2. 변수 이름 수정 (Question 2)
names(SeoulBike) <- c(
  "Date",          # 날짜
  "Rented",        # 대여량
  "Hour",          # 시간
  "Temp",          # 온도
  "Humidity",      # 습도
  "WindSp",        # 풍속
  "Visibility",    # 가시거리
  "Dew",           # 이슬점
  "Solar",         # 일사량
  "Rain",          # 강수량
  "Snow",          # 적설량
  "Holiday",       # 공휴일 여부
  "FunctioningDay" # 운영일 여부
)
str(SeoulBike)

## 3. 데이터 타입 변환 (Question 3)
# (1) Date: 문자 -> Date 유형
SeoulBike$Date <- as.Date(SeoulBike$Date, format = "%d/%m/%Y")

# (2) Holiday: "No Holiday"→0, "Holiday"→1
SeoulBike$Holiday <- ifelse(SeoulBike$Holiday == "No Holiday", 0, 1)

# (3) FunctioningDay: "Yes"→1, "No"→0
SeoulBike$FunctioningDay <- ifelse(SeoulBike$FunctioningDay == "Yes", 1, 0)

str(SeoulBike)

## 4. 불필요 변수 선정 (Question 4)
# Dew: 온도(Temp), 습도(Humidity)와 강한 다중공선성(중복 정보) 존재
# FunctioningDay: 값의 분산이 매우 낮아 정보 가치 없음

## 다중공선성 확인하여 근거 제시
library(car)
lm_initial <- lm(Rented ~ Hour + Temp + Humidity + WindSp + Visibility + Dew + Solar + Rain + Snow + Holiday + FunctioningDay, data = SeoulBike)
vif(lm_initial) # Dew 값 매우 높음(VIF > 10 예상), FunctioningDay 분산 낮음(거의 1)

## 5. 불필요 변수 제거 (Question 5)
Bike2 <- subset(SeoulBike, select = -c(Dew, FunctioningDay))
str(Bike2)

## 6. 연구 가설 세우기 (Question 6)
# Temp (+): 온도 상승 → 대여량 증가
# Humidity (-): 습도 상승 → 대여량 감소
# WindSp (-): 풍속 증가 → 대여량 감소
# Visibility (+): 가시거리 증가 → 대여량 증가
# Solar (+): 일사량 증가 → 대여량 증가
# Rain (-): 비 증가 → 대여량 감소
# Snow (-): 눈 증가 → 대여량 감소
# Hour: 출퇴근 시간대(7-9시, 17-19시) → 대여량 증가
# Holiday (+): 공휴일 → 대여량 증가 예상

## 7. 다중선형회귀모델 적합 (Question 7)
lm_model <- lm(
  Rented ~ Hour + Temp + Humidity + WindSp + Visibility + Solar + Rain + Snow + Holiday,
  data = Bike2
)
summary(lm_model)

## 8. 다중공선성 재확인 (Question 8)
vif_values <- vif(lm_model)
print(vif_values)

## 9. 모델 성능 확인 및 시각화 (Question 9)
# (1) RMSE 계산
Bike2$pred <- predict(lm_model, Bike2)
rmse <- sqrt(mean((Bike2$Rented - Bike2$pred)^2))
cat("RMSE:", round(rmse, 2), "\n")

# (2) 결정계수(R²), 조정된 R² 계산
r2 <- summary(lm_model)$r.squared
adj_r2 <- summary(lm_model)$adj.r.squared
cat("R2:", round(r2, 4), "\n")
cat("Adjusted R2:", round(adj_r2, 4), "\n")

# (3) ggplot2로 실제 vs 예측값 산점도
library(ggplot2)
ggplot(Bike2, aes(x = Rented, y = pred)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "actual vs pred",
    x = "actual",
    y = "pred"
  )
## 10. 중간평가 및 해석 (수정 버전)

# (1) 변수별 해석
# - Temp(양의 관계): 온도가 올라가면 대여량 증가
# - Rain(음의 관계): 비가 오면 대여량 감소
# - Humidity(음의 관계): 습도가 높으면 대여량 감소
# - Hour: 출퇴근 시간대에 대여량 증가
# - Holiday(양의 관계): 공휴일 대여량 증가

# (2) 모델 전체 평가
# - Adjusted R²는 약 0.47 수준으로, 전체 변동의 절반 미만만 설명
# - 이는 계절성, 요일, 비선형 패턴 등이 포함되지 않아 생긴 한계임
# - RMSE는 약 450~470 수준으로, 다소 높은 편

# (3) 결론 및 추가 분석 제안
# 계절성(요일, 월 등 날짜 변수)을 추가하면 성능이 더 개선될 수 있음.
# 다항항(Temp^2 등), 교호작용(Temp*Hour 등)을 추가하면 추가 성능 향상 가능.
# 잔차분석을 통한 정규성, 등분산성 진단이 권장됨.

## 11. 성능 향상 모델 구축 (Question 11 – 개선 모델)
# 2. 변수 제거 및 파생
Bike2 <- subset(SeoulBike, select = -c(Dew, FunctioningDay))
Bike2$HourF <- factor(Bike2$Hour)
Bike2$Month <- as.factor(format(Bike2$Date, "%m"))
Bike2$Weekday <- as.factor(weekdays(Bike2$Date))
Bike2$MonthNum <- as.numeric(format(Bike2$Date, "%m"))
Bike2$Season <- ifelse(Bike2$MonthNum %in% c(3,4,5), "spring",
                       ifelse(Bike2$MonthNum %in% c(6,7,8), "summer",
                              ifelse(Bike2$MonthNum %in% c(9,10,11), "fall", "winter")))
Bike2$SeasonF <- as.factor(Bike2$Season)

# 3. RushHour 생성
day_names <- weekdays(Bike2$Date)
if (all(grepl("[가-힣]", day_names))) {
  work_days <- c("월요일", "화요일", "수요일", "목요일", "금요일")
} else {
  Sys.setlocale("LC_TIME", "C")
  Bike2$Weekday <- weekdays(Bike2$Date)
  work_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
}
Bike2$Holiday <- ifelse(Bike2$Holiday %in% c(0, "No", "N", "0", FALSE), "No", "Yes")
Bike2$RushHour <- with(Bike2, ifelse(Holiday == "No" & Weekday %in% work_days & Hour >= 7 & Hour <= 9, "Yes", "No"))
Bike2$RushHour <- factor(Bike2$RushHour)

# 4. 로그 변환
Bike2$Solar_log <- log1p(as.numeric(Bike2$Solar))
Bike2$Rain_log <- log1p(as.numeric(Bike2$Rain))
Bike2$Snow_log <- log1p(as.numeric(Bike2$Snow))
Bike2$Vis_diff_log <- log1p(2000 - as.numeric(Bike2$Visibility))
Bike2 <- subset(Bike2, select = -c(Solar, Rain, Snow, Visibility))

# 5. 최종 모델 구성 (전체)
lm_final <- lm(
  Rented ~ Temp + I(Temp^2) + Humidity + I(Humidity^2) + WindSp +
    Solar_log + Rain_log + Snow_log + Vis_diff_log +
    Holiday + HourF + Temp:HourF + Vis_diff_log:HourF +
    Rain_log:Holiday + Month + Weekday + SeasonF + RushHour +
    Rain_log:HourF + Holiday:Weekday + Humidity:SeasonF +
    RushHour:Rain_log + RushHour:Temp + WindSp:HourF,
  data = Bike2
)

# 6. 평가
Bike2$pred_final <- predict(lm_final, newdata = Bike2)
cat("전체 RMSE:", round(sqrt(mean((Bike2$Rented - Bike2$pred_final)^2)), 2), "\n")
cat("전체 R2:", round(summary(lm_final)$r.squared, 4), "\n")
cat("전체 Adjusted R²:", round(summary(lm_final)$adj.r.squared, 4), "\n")

# 7. Cook's Distance 이상치 제거
lm_interaction_model <- lm(
  Rented ~ Temp + I(Temp^2) + Humidity + I(Humidity^2) +
    Rain_log + Holiday + HourF + Temp:HourF +
    SeasonF + Weekday + Rain_log:HourF + Holiday:Weekday + Humidity:SeasonF,
  data = Bike2
)
cooks_d <- cooks.distance(lm_interaction_model)
threshold <- 4 / nrow(Bike2)
Bike2_clean <- Bike2[cooks_d < threshold, ]
cat("제거된 이상치 수:", nrow(Bike2) - nrow(Bike2_clean), "\n")

# 8. 이상치 제거 후 모델 재학습
lm_clean <- lm(
  Rented ~ Temp + I(Temp^2) + Humidity + I(Humidity^2) + WindSp +
    Solar_log + Rain_log + Snow_log + Vis_diff_log +
    Holiday + HourF + Temp:HourF + Vis_diff_log:HourF +
    Rain_log:Holiday + Month + Weekday + SeasonF + RushHour +
    Rain_log:HourF + Holiday:Weekday + Humidity:SeasonF +
    RushHour:Rain_log + RushHour:Temp + WindSp:HourF,
  data = Bike2_clean
)
Bike2_clean$pred_clean <- predict(lm_clean, newdata = Bike2_clean)
cat("[이상치 제거] RMSE:", round(sqrt(mean((Bike2_clean$Rented - Bike2_clean$pred_clean)^2)), 2), "\n")
cat("[이상치 제거] R2:", round(summary(lm_clean)$r.squared, 4), "\n")
cat("[이상치 제거] Adjusted R2:", round(summary(lm_clean)$adj.r.squared, 4), "\n")

# 9. 시각화
ggplot(Bike2_clean, aes(x = Rented, y = pred_clean)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "after lm_interaction_clean: actual vs pred",
    x = "actual",
    y = "pred"
  )
