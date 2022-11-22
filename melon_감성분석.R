###############################
### 감성분석
library(plyr)
library(stringr)
library(tidyverse) # 깔끔한 데이터 처리 및 분석을 위한 패키지


setwd("C:/Users/hsh97/Desktop/data")

positive <- readLines("C:/Users/hsh97/Desktop/data/positive.txt", encoding = "UTF-8")
positive=positive[-1]

negative <- readLines("negative.txt", encoding = "UTF-8")
negative=negative[-1]

sentimental = function(sentences, positive, negative){
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence = gsub('[[:punct:]]', '', sentence) # 문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) # 특수문자 제거
    sentence = gsub('\\d+', '', sentence)        # 숫자 제거
    
    word.list = str_split(sentence, '\\s+')      # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list)                    # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, positive)         # words의 단어를 positive(긍정어사전)에서 매칭
    neg.matches = match(words, negative)         # words의 단어를 negative(부정어사전)에서 매칭
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정 = 점수   
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

txt = melon_90$가사[1:3]
txt = as.character(txt)
txt

result=sentimental(txt, positive, negative)
str(result)

result$color[result$score >=1] = "blue"
result$color[result$score ==0] = "green"
result$color[result$score < 0] = "red"

result$remark[result$score >=1] = "긍정"

result$remark[result$score ==0] = "중립"

result$remark[result$score < 0] = "부정"


sentiment_result= table(result$remark)
sentiment_result= data.frame(sentiment_result)
sentiment_result$비율 = round(sentiment_result$Freq/sum(sentiment_result$Freq), 3)


pie(sentiment_result$Freq, main="감성분석 결과", labels = paste(sentiment_result$Var1, sentiment_result$비율,"%"),
    col=c("blue","red","green"), radius=0.8)
text(0, 1, "Learner", cex = 2)

sentiment_result


emotion <- function(melon_year){
  txt = as.character(melon_year$가사)
  result=sentimental(txt, positive, negative)
  
  result$color[result$score >=1] = "blue"
  result$color[result$score ==0] = "green"
  result$color[result$score < 0] = "red"
  
  result$remark[result$score >=1] = "긍정"
  result$remark[result$score ==0] = "중립"
  result$remark[result$score < 0] = "부정"
  
  sentiment_result= table(result$remark)
  sentiment_result= data.frame(sentiment_result)
  sentiment_result$비율 = round(sentiment_result$Freq/sum(sentiment_result$Freq), 3)
  
  
  pie(sentiment_result$Freq, labels = paste(sentiment_result$Var1, sentiment_result$비율*100,"%"),
      col=c("blue","red","green"), radius=0.8)
  text(0, 1, "20년대", cex = 1.5)
  
  sentiment_result
}

emotion(melon_20)
emo_20 = emotion(melon_20)
emo_10 = emotion(melon_10)
emo_00 = emotion(melon_00)
emo_90 = emotion(melon_90)
emo_80 = emotion(melon_80)

names(test_80)[2] = 'p_1980' # p라는 컬럼명을 년도까지 붙여서 변경

names(emo_20)[3] = '2020'
emo_20 = emo_20[-2]

names(emo_10)[3] = '2010'
emo_10 = emo_10[-2]

names(emo_00)[3] = '2000'
emo_00 = emo_00[-2]

names(emo_90)[3] = '1990'
emo_90 = emo_90[-2]

names(emo_80)[3] = '1980'
emo_80 = emo_80[-2]


melon_emotion <- full_join(emo_80,emo_90,by='Var1') # word 컬럼명을 기준으로 full join 하기
melon_emotion <- full_join(melon_emotion,emo_00,by='Var1')
melon_emotion <- full_join(melon_emotion,emo_10,by='Var1')
melon_emotion <- full_join(melon_emotion,emo_20,by='Var1')
melon_emotion

melon_emotion.long <- melon_emotion %>% 
  gather(year, 비율, -Var1) %>%
  filter(year %in% c('Var1', '1980', '1990', '2000', '2010', '2020')) %>%
  print

ggplot(melon_emotion.long, aes(x=year, y=비율, group=Var1)) +
  geom_line(aes(color=Var1), size = 1)+
  geom_point(aes(color=Var1), size = 3)
