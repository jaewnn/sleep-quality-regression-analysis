library(alr4)
library(lmtest)
library(car)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(psych)
library(AID)
#data1 : sleepquality ~ all(except step)
data1<-read.csv("C:/Users/82104/Desktop/과제/2-2/회귀/sleep250.csv")
str(data1)
data1<-data1[,-1]
#step 열 제거 // 나중에 따로 단순선형회귀 진행
data1<-data1[,-6]

#데이터 파악(raw데이터)
str(data1)
raw<-lm(Sleep.quality~.,data=data1)
summary(raw)
par(mfrow=c(2,2))
plot(raw)
pairs(data1[,c(13,1,4,7,8,10)])
avPlots(raw)
#skew 측정
hist(data1$Sleep.quality)
describe(data1)

#회귀가정 검증(raw데이터 모형)
#선형성 > X
residualPlots(raw)
# >>  fitted value curvature 관측

#정규성 > X
shapiro.test(raw$residuals)
qqPlot(raw)

#등분산성 > x
ncvTest(raw)

#독립성 > o
dwtest(raw)

#다중공선성 >> wp_well과 wp_bad이 8에 가깝게
# 높게 나와서 차후 수정할 수도 있다. > anova
vif(raw)

# 무언가 조치를 취해야한다.
# 변수선택 > 다중공선성 확인 
# > 변수 변환 > 이상치 제거 순으로 진행

# 변수선택
step(raw,direction='both')
m1<-lm(Sleep.quality ~ Start + Time.in.bed + Drank.coffee + 
         Drank.tea + wp_well, data = data1)
anova(raw,m1)
# >> anova를 취했을 때 f통계량의 pvalue가 상당히 높게 나오므로
# 필요없는 변수를 제거를 해도 통계적으로 유의하다.
vif(m1)
# 다중 공선성도 전체적으로 낮아졌다.

#회귀가정 검증 > 여전히 가정은 만족 x
summary(m1)
residualPlots(m1)
shapiro.test(m1$residuals)
ncvTest(m1)
dwtest(m1)

# 변수변환
summary(car::powerTransform(data1$Sleep.quality))
# response를 지수변환을 했을 때 가장 최적의 지수값은 2.3712
data1$sq_transform = data1$Sleep.quality^2.3712
hist(data1$sq_transform)
skew(data1$Sleep.quality)
skew(data1$sq_transform)
#> sleep quality의 skew가 어느정도 해결
m12<-lm(formula = sq_transform ~ Start + Time.in.bed + Drank.coffee + 
          Drank.tea + wp_well, data = data1)
par(mfrow=c(2,2))
plot(m12)
summary(m12)
# >> 설명력은 다소 감소 

# 회귀가정 검증 
# 선형성: X >> 오히려 pvalue 감소
# 정규성 : O
# 등분산성 : X > pvalue 증가
# 독립성 : O
residualPlots(m12)
shapiro.test(m12$residuals)
ncvTest(m12)
dwtest(m12)

# 이상치와 영향관측치 처리 필요
outlierTest(m12)
influenceIndexPlot(m12)
#이상치 체크
outlierTest(m1)
influencePlot(m1)
influenceIndexPlot(m1)
# 53 outlier candidate & 2,138 influence point 
influencePlot(m12)
data1[c(2,53,138),]
# 53만 제거해서 회귀식 접합
m13<-lm(formula = sq_transform ~ Start + Time.in.bed + Drank.coffee + 
          Drank.tea + wp_well, data = data1[-53,])
summary(m13)
residualPlots(m13)
shapiro.test(m13$residuals)
ncvTest(m13)
dwtest(m13)


#data2 : sleepquality~step
data2<-read.csv("C:/Users/82104/Desktop/과제/2-2/회귀/sleep411_step.csv")
str(data2)
pairs(data2)]
describe(data2)
data2<-data2[,c(4,8)]
data2[c(210,278,433),]
summary(car::powerTransform(data2$Activity..steps.))
data2$sq_transform = data2$Sleep.quality^2.3712
m2<-lm(sq_transform~Activity..steps.,data=data2[-c(210,278,433)])
summary(m2)
plot(m2)
residualPlots(m2)
shapiro.test(m2$residuals)
ncvTest(m2)
outlierTest(m2)
influenceIndexPlot(m2)

par(mfrow=c(2,2))
