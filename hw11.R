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

# (1) Hour를 범주형 변수로 변환
Bike2$HourF <- factor(Bike2$Hour)

# (2) 월(Month), 요일(Weekday) 변수 추가 (계절성 반영)
Bike2$Month <- as.factor(format(SeoulBike$Date, "%m"))
Bike2$Weekday <- as.factor(weekdays(SeoulBike$Date))

# (3) 분석에 사용할 데이터 구성
Bike2 <- subset(SeoulBike, select = -c(Dew, FunctioningDay))

# (4) 시간, 월, 요일 변수 생성
Bike2$HourF <- factor(Bike2$Hour)
Bike2$Month <- as.factor(format(SeoulBike$Date, "%m"))
Bike2$Weekday <- as.factor(weekdays(SeoulBike$Date))

# (5) 계절 변수 생성
Bike2$MonthNum <- as.numeric(format(SeoulBike$Date, "%m"))
Bike2$Season <- ifelse(Bike2$MonthNum %in% c(3,4,5), "spring",
                       ifelse(Bike2$MonthNum %in% c(6,7,8), "summer",
                              ifelse(Bike2$MonthNum %in% c(9,10,11), "fall", "winter")))
Bike2$SeasonF <- as.factor(Bike2$Season)

# (6) 출근 시간대 변수 생성
Sys.setlocale("LC_TIME", "C")  # 요일 영어로 변환
Bike2$Weekday <- weekdays(Bike2$Date)
work_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
Bike2$Holiday <- ifelse(Bike2$Holiday == 0, "No", "Yes")
Bike2$RushHour <- with(Bike2, ifelse(
  Holiday == "No" & Weekday %in% work_days & Hour >= 7 & Hour <= 9,
  "Yes", "No"))
Bike2$RushHour <- factor(Bike2$RushHour)

# (7) 개선된 회귀 모델 적합 (비선형항 + 교호작용 + 계절/요일/출근시간 포함)
lm_model3 <- lm(
  Rented ~ Temp + I(Temp^2) + Humidity + I(Humidity^2) +
    WindSp + Visibility + Solar + Rain + Snow + Holiday +
    HourF + Temp:HourF + Visibility:HourF + Rain:Holiday +
    Month + Weekday + SeasonF + RushHour +
    Rain:HourF + Holiday:Weekday + Humidity:SeasonF,
  data = Bike2
)

# (8) 모델 성능 평가
Bike2$pred3 <- predict(lm_model3, Bike2)
rmse3 <- sqrt(mean((Bike2$Rented - Bike2$pred3)^2))
r2_3 <- summary(lm_model3)$r.squared
adj_r2_3 <- summary(lm_model3)$adj.r.squared

cat("최종 모델 RMSE:", round(rmse3, 2), "\n")
cat("최종 모델 R²:", round(r2_3, 4), "\n")
cat("최종 모델 Adjusted R²:", round(adj_r2_3, 4), "\n")

# (9) 다중공선성 확인
vif_values3 <- vif(lm_model3)
print(vif_values3)

# (10) 시각화: 실제 대여량 vs 예측 대여량
ggplot(Bike2, aes(x = Rented, y = pred3)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "purple") +
  labs(
    title = "최종 모델: 실제 대여량 vs 예측 대여량",
    x = "실제 대여량",
    y = "예측 대여량"
  )
