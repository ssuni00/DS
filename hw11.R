## Data Science 01분반 Team 4 HW 11 ##

# library 불러오기
library(ggplot2)
library(car)


## Question 1. 데이터 불러오기 및 특성 파악
setwd('/Users/ssuni/Desktop/HGU/25-1/DS/hw11')

SeoulBike<- read.csv("SeoulBikeData.csv")
str(SeoulBike)
summary(SeoulBike)

## Question 2. 변수 이름 수정
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

## Question 3. 데이터 타입 변환 
# 3-1. Date: 문자 -> Date 유형
SeoulBike$Date <- as.Date(SeoulBike$Date, format = "%d/%m/%Y")

# 3-2. Holiday: "No Holiday" -> 0, "Holiday" -> 1
SeoulBike$Holiday <- ifelse(SeoulBike$Holiday == "No Holiday", 0, 1)

# 3-3. FunctioningDay: "Yes" -> 1, "No" -> 0
SeoulBike$FunctioningDay <- ifelse(SeoulBike$FunctioningDay == "Yes", 1, 0)

str(SeoulBike)

## Question 4. 불필요 변수 선정 
# 4-1. 다중공선성(vif) 확인
lm_initial <- lm(Rented ~ Hour + Temp + Humidity + WindSp + Visibility + Dew + Solar + Rain + Snow + Holiday + FunctioningDay, data = SeoulBike)
vif(lm_initial)

# 결과
# Hour           Temp          Humidity         WindSp        Visibility      Dew            Solar           Rain           Snow 
# 1.183737      87.129486      20.365555       1.276262       1.569368     115.714249       2.021445       1.084453       1.096246 
# Holiday   FunctioningDay 
# 1.006521       1.005853 

# Dew: 115.714249 = 강한 다중공선성, 변수 제거필요

# 4-2. 수치형 변수들의 분산 계산
numeric_vars <- sapply(SeoulBike, is.numeric)  # 수치형 변수 추출
sapply(SeoulBike[, numeric_vars], var)

# 결과
# sapply(SeoulBike[, numeric_vars], var)
# Rented           Hour           Temp          Humidity      WindSp          Visibility        Dew          Solar 
# 4.160217e+05   4.792214e+01   1.426788e+02   4.146279e+02   1.073918e+00   3.700273e+05   1.705732e+02   7.547200e-01 
# Rain            Snow            Holiday      FunctioningDay 
# 1.272819e+00   1.907472e-01   4.688845e-02   3.254545e-02 

# FunctioningDay: 값의 분산이 변수 중 가장 낮고, 값 자체도 낮음. 정보 가치 없음, 변수 제거필요

# 4-3. 추가적으로 table 확인
table(SeoulBike$FunctioningDay)
#  0    1 
# 295 8465 
# table 확인했을 때도 0과 1의 분산이 극명함

## Question 5. 불필요 변수 제거 
Bike2 <- subset(SeoulBike, select = -c(Dew, FunctioningDay))
str(Bike2)

## Question 6. 가설 세우기
# Temp (+): 온도 상승 -> 대여량 증가
# Humidity (-): 습도 상승 -> 대여량 감소
# WindSp (-): 풍속 증가 -> 대여량 감소
# Visibility (+): 가시거리 증가 -> 대여량 증가
# Solar (+): 일사량 증가 -> 대여량 증가
# Rain (-): 비 증가 -> 대여량 감소
# Snow (-): 눈 증가 -> 대여량 감소
# Hour (+): 출퇴근 시간대(7-9시, 17-19시) -> 대여량 증가
# Holiday (+): 공휴일 -> 대여량 증가

## Question 7. 다중선형회귀모델 적합
lm_model <- lm(
  Rented ~ Hour + Temp + Humidity + WindSp + 
    Visibility + Solar + Rain + Snow + Holiday,
  data = Bike2
)
summary(lm_model)

## Question 8. 다중공선성 재확인
vif_values <- vif(lm_model)
print(vif_values)

## Question 9. 모델 성능 확인 및 시각화
# 9-1. RMSE 계산
Bike2$pred <- predict(lm_model, Bike2)
rmse <- sqrt(mean((Bike2$Rented - Bike2$pred)^2))
cat("RMSE:", round(rmse, 2), "\n")

# 9-2. 결정계수(R2), 조정된 R2 계산
r2 <- summary(lm_model)$r.squared
adj_r2 <- summary(lm_model)$adj.r.squared
cat("R2:", round(r2, 4), "\n")
cat("Adjusted R2:", round(adj_r2, 4), "\n")

# 9-3. ggplot2로 실제 vs 예측값 산점도
ggplot(Bike2, aes(x = Rented, y = pred)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "actual vs pred",
    x = "actual",
    y = "pred"
  )

## Question 10. 중간평가 및 해석
# 10-1. 변수별 해석
# - Temp(양의 관계): 온도가 올라가면 대여량 증가
# - Rain(음의 관계): 비가 오면 대여량 감소
# - Humidity(음의 관계): 습도가 높으면 대여량 감소
# - Hour(양의 관계): 출퇴근 시간대에 대여량 증가
# - Holiday(양의 관계): 공휴일 대여량 증가

# 10-2. 모델 전체 평가
# - Adjusted R2: 0.47 -> 전체 변동의 절반 미만만 설명하는데,
#   -> 계절성, 요일등이 포함되지 않아 생긴 한계라고 판단함
# - RMSE: 468.02, 11번에서 진행할 모델과 비교 필요


## Question 11. 성능 향상 모델 구축
# 11-1. 변수 전처리 및 파생 변수 생성
Bike2 <- subset(SeoulBike, select = -c(Dew, FunctioningDay))
Bike2$HourF <- factor(Bike2$Hour)
Bike2$Month <- as.factor(format(Bike2$Date, "%m"))
Bike2$Weekday <- as.factor(weekdays(Bike2$Date))
Bike2$MonthNum <- as.numeric(format(Bike2$Date, "%m"))
Bike2$Season <- ifelse(Bike2$MonthNum %in% c(3,4,5), "spring",
                       ifelse(Bike2$MonthNum %in% c(6,7,8), "summer",
                              ifelse(Bike2$MonthNum %in% c(9,10,11), "fall", "winter")))
Bike2$SeasonF <- as.factor(Bike2$Season)

day_names <- weekdays(Bike2$Date)
if (all(grepl("[ㄱ-ㅎ]", day_names))) {
  work_days <- c("월요일", "화요일", "수요일", "목요일", "금요일")
} else {
  Sys.setlocale("LC_TIME", "C")
  Bike2$Weekday <- weekdays(Bike2$Date)
  work_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
}
Bike2$Holiday <- ifelse(Bike2$Holiday %in% c(0, "No", "N", "0", FALSE), "No", "Yes")
Bike2$RushHour <- with(Bike2, ifelse(
  Holiday == "No" & Weekday %in% work_days & (Hour %in% c(7:9, 17:19)),
  "Yes", "No"
))
Bike2$RushHour <- factor(Bike2$RushHour)
Bike2$Solar_log <- log1p(as.numeric(Bike2$Solar))
Bike2$Rain_log <- log1p(as.numeric(Bike2$Rain))
Bike2$Snow_log <- log1p(as.numeric(Bike2$Snow))
Bike2$Vis_diff_log <- log1p(2000 - as.numeric(Bike2$Visibility))
Bike2 <- subset(Bike2, select = -c(Solar, Rain, Snow, Visibility))

# 11-2. Cook's Distance로 이상치 제거
lm_interaction_model <- lm(
  Rented ~ Temp + I(Temp^2) + Humidity +
    Rain_log + Holiday + HourF + Temp:HourF +
    SeasonF + Weekday,
  data = Bike2
)
cooks_d <- cooks.distance(lm_interaction_model)
threshold <- 4 / nrow(Bike2)
Bike2_clean <- Bike2[cooks_d < threshold, ]
cat("제거된 이상치 수:", nrow(Bike2) - nrow(Bike2_clean), "\n")

# 11-3. 회귀 모델
lm_clean <- lm(
  Rented ~ Temp + I(Temp^2) + Humidity +
    Solar_log + Rain_log + Vis_diff_log +
    Holiday + HourF + Temp:HourF +
    SeasonF + RushHour,
  data = Bike2_clean
)

# 11-4. 예측 및 평가
Bike2_clean$pred_clean <- predict(lm_clean, newdata = Bike2_clean)
cat("RMSE:", round(sqrt(mean((Bike2_clean$Rented - Bike2_clean$pred_clean)^2)), 2), "\n")
cat("R2:", round(summary(lm_clean)$r.squared, 4), "\n")
cat("Adjusted R2:", round(summary(lm_clean)$adj.r.squared, 4), "\n")

# 11-5. 시각화
ggplot(Bike2_clean, aes(x = Rented, y = pred_clean)) +
  geom_point(alpha = 0.4, color = "forestgreen") +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(
    title = "after cleaning: actual vs pred",
    x = "actual",
    y = "pred"
  )
