library(readr)
library(dplyr)
library(tidyr)
library(purrr)
data = read.csv("C:/Users/82104/Desktop/과제/2-2/회귀/sleepdata.csv",header = TRUE)
head(data)
summary(data)
str(data)
# SleepQuality % 제거
data = data %>%
  mutate(Sleep.quality = gsub('\\%', '', Sleep.quality))
# time in bed를 분으로 환산(시간*60+분)
word_fun <- function(i){
  split <-  unlist(strsplit(data$Time.in.bed[i],":"))
  word <-   data.frame(
    H = split[1] , M = split[2]
  )
  return(word)
}
x <- 1:nrow(data)

mylist <- x %>% 
  map(word_fun)
head(mylist)

tmbed_split <- as.data.frame(do.call(rbind,mylist))
data <- cbind(tmbed_split, data)
head(data)
# numeric화(H,M)
data$H<-as.numeric(data$H)
data$M<-as.numeric(data$M)
#데이터 병합
data = data %>%
  mutate(tm_in_bd_min = 60*H + M)
# wake up 양적자료로 변환(":)=1, :|=-1, null=0") > wp_feel
# null값을 0으로 반환했는데 이렇게 했을 때 괜찮을지가...>>평균치로 대체해보자
data = data %>%
  mutate(wp_feel = ifelse(data$Wake.up==':)',1,ifelse(data$Wake.up==':|',-1,0)))
head(data)
#sleep.notes 그룹화(각 factor로 분리)
word_fun2 <- function(i){
  split <-  unlist(strsplit(data$Sleep.Notes[i],":"))
  word <-   data.frame(
    n1 = split[1] , n2 = split[2] , n3 = split[3]
  )
  return(word)
}
x <- 1:nrow(data)

mylist2 <- x %>% 
  map(word_fun2)
head(mylist2)

mylist2_split <- as.data.frame(do.call(rbind,mylist2))
mylist2_split
data <- cbind(mylist2_split, data)
head(data)
#필요없는 변수 제거
data_1 = data[,c(-4,-5,-9,-10,-11)]
head(data_1)
# Sleep_note 더미변수화
data_1=data_1 %>%
  mutate(str_day = ifelse(n1=="Stressful day"| n2 == "Stressful day" | n3 == "Stressful day",1,0),
        dr_cof=ifelse(n1=="Drank coffee"| n2 == "Drank coffee" | n3 == "Drank coffee",1,0),
        dr_tea=ifelse(n1=="Drank tea"| n2 == "Drank tea" | n3 == "Drank tea",1,0),
        Wk_out=ifelse(n1=="Worked out"| n2 == "Worked out" | n3 == "Worked out",1,0),
        Ate_late=ifelse(n1=="Ate late"| n2 == "Ate late" | n3 == "Ate late",1,0))
head(data_1)
data2=data_1[,c(-1,-2,-3)]
head(data2)

# 더미변수 결측치 0으로 처리
data2 <- mutate_at(data2, c("str_day", "dr_cof","dr_tea","Wk_out","Ate_late"), ~replace(., is.na(.), 0))
head(data2)

#heart.rate 결측치 처리(나중엔 아예 안되어있어서 0으로 처리하긴 좀 그런상황)
#start end값 처리(그냥 HH:MM에서 H만 따서 처리함)
str(data2)
data2$start = as.POSIXct(data2$Start, format = "%Y-%m-%d %H:%M")
data2$end = as.POSIXct(data2$End, format = "%Y-%m-%d %H:%M")
data2$start_h = format(data2$start,"%H:%M")
data2$end_h = format(data2$end,"%H:%M")
data_f = data2[,c(-1,-2,-12,-13)]
head(data_f)
#데이터타입 numeric 변경
str(data_f)
data_f$Sleep.quality = as.numeric(data_f$Sleep.quality)
data_f$start_h = as.numeric(data_f$start_h)
data_f$end_h = as.numeric(data_f$end_h)
str(data_f)
pairs(data_f)

# 저장
write.csv(data_f,file="C:/Users/82104/Desktop/과제/2-2/회귀/sleep2.csv")

