library(KoNLP) # 한국어 형태소 분석 R 패키지
library(rJava) # KoNLP위한 패키지
library(tidyverse) # 깔끔한 데이터 처리 및 분석을 위한 패키지
library(urltools) # 인코딩 된 문자열을 디코딩하여 확인
library(RColorBrewer) # 다양한 색 포함하는 패키지
library(wordcloud) # 덱스트클라우드를 위한 패키지
library(wordcloud2) # 덱스트클라우드를 위한 패키지2
library(ggplot2) # 그래프를 위한 패키지

# 크롤링한 멜론 노래 파일 가져오기
melon_fin <- read.csv('C:/Users/hsh97/Desktop/data/melon_fin_ANSI.csv', encoding = 'utf-8', header = T)
str(melon_fin)

# 결측치 확인
sum(is.na(melon_fin)) # 0

# 명사 추출을 위해 KoNLP에 저장되어있는 사전을 실행
useSejongDic()

# 명사를 추출해주는 함수 
extract_words <- function(lyric){
  keyword <- lyric %>% 
    as.character()   # 가사가 factor형이므로 분석을 위해 character 변경 
  keyword <- paste(SimplePos09(keyword)) %>% # SimplePos09() : 9개의 품사로 분류
    # paste() : 두개의 argument를 넣어서 붙여주는 함수
    str_match("([a-zA-Z가-힣]+)/.")   # str_match() : pattern에 맞는 값을 반환
  keyword <- keyword[, 2]   # 품사가 적혀있지 않는 2열의 값만 추출
  return(keyword[!is.na(keyword)])  # NA값을 제거한 단어만 반환
}

# for문을 통해서 분리된 단어를 melon_word 벡터에 저장
melon_word <- c()
for(i in 1:nrow(melon_fin)) {
  melon_word <- append(melon_word, extract_words(melon_fin$가사[i]))
}

summary(melon_word)
head(melon_word)

sort(table(melon_word), decreasing = TRUE)
summary(melon_word)
head(melon_word)


# 데이터 전처리
# as.data.frame() : data.frame으로 변환시켜주는 함수
melon_word <- as.data.frame(table(melon_word))
str(melon_word)
melon_word$melon_word <- as.character(melon_word$melon_word) 
melon_word

# 단어 2글자 이상이고 5번 이상 나온 단어만 수집
melon_word <- melon_word %>% 
  rename(word = melon_word) %>%
  dplyr::filter(nchar(word) >= 2 & Freq >= 5) %>% 
  arrange(desc(Freq))

head(melon_word)


# wordcloud
# brewer.pal() 함수를 통해 색 지정
# RColorbrewer 패키지에 다양한 색
pal <- brewer.pal(8, "Dark2")

# 난수 발생
set.seed(1000) # 동일한 임의 순서를 위해

wordcloud(words = melon_word$word,   # 표시할 단어
          freq = melon_word$Freq,    # 단어 빈도
          min.freq = 5,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = FALSE,  # 단어 배치 FALSE(고빈도 단어 중앙 배치)
          rot.per = .1,          # 회전 단어 비율
          scale = c(5, 0.5),     # 단어 크기 범위
          colors = pal)           # 색깔

# wordcloud2
# wordcloud와는 다르게 데이터만 넣어주면 시각화 표현
wordcloud2(melon_word)



# 10년 단위로 단어 수 비교(ex: 85~89, 90~99, ..., 20~21)
melon_80 <- subset(melon_fin, 년도<1990)
melon_90 <- subset(melon_fin, 년도>=1990 & 년도<2000)
melon_00 <- subset(melon_fin, 년도>=2000 & 년도<2010)
melon_10 <- subset(melon_fin, 년도>=2010 & 년도<2020)
melon_20 <- subset(melon_fin, 년도>=2020)


# 명사를 추출해주는 함수 
wordmake <- function(melon_year){
  melon_one <- c()
  for(i in 1:nrow(melon_year)) {
    melon_one <- append(melon_one, extract_words(melon_year$가사[i]))
  }
  sort(table(melon_one), decreasing = TRUE)
  
  melon_one <- as.data.frame(table(melon_one))
  melon_one$melon_one <- as.character(melon_one$melon_one) 
  
  melon_one <- melon_one %>% 
    rename(word = melon_one) %>%
    dplyr::filter(nchar(word) >= 2 & Freq >= 5) %>% 
    arrange(desc(Freq))
}

# 각 년도별 단어 분할
melon_80_word = wordmake(melon_80)
head(melon_80_word)

melon_90_word = wordmake(melon_90)
head(melon_90_word)

melon_00_word = wordmake(melon_00)
head(melon_00_word)

melon_10_word = wordmake(melon_10)
head(melon_10_word)

melon_20_word = wordmake(melon_20)
head(melon_20_word)

# 각 단어 wordcloud2
# wordcloud2가 더 알아보기 쉬움
wordcloud2(melon_80_word)
wordcloud2(melon_90_word)
wordcloud2(melon_00_word)
wordcloud2(melon_10_word)
wordcloud2(melon_20_word)


## 단어 수 비율로 나타내기
options(digits=4) # 소수점 자리 수 설정

# 소수점 자리 기입 함수
wordp <- function(melon_year){
  p <- c()
  for(i in 1:nrow(melon_year)) {
    ex <- melon_year$Freq[i] / sum(melon_year$Freq) *100
    p <- append(p, ex)
  }
  melon_year <- cbind(melon_year, p)
}

melon_80_word <- wordp(melon_80_word)
melon_90_word <- wordp(melon_90_word)
melon_00_word <- wordp(melon_00_word)
melon_10_word <- wordp(melon_10_word)
melon_20_word <- wordp(melon_20_word)



# 각 년도별 가장 많이 나온 단어 6개씩 추출
word_top <- head(melon_80_word)
word_top <- as.character(word_top$word)
word_top <- append(word_top, as.character(head(melon_90_word)$word))
word_top <- append(word_top, as.character(head(melon_00_word)$word))
word_top <- append(word_top, as.character(head(melon_10_word)$word))
word_top <- append(word_top, as.character(head(melon_20_word)$word))

word_top

# 중복값 제거
word_top <- unique(word_top)
word_top

####################################################
## A의 단어 각 년도별로 비율 구하기
test_80 <- data.frame()

for (i in 1:14){
  ex <- melon_80_word[melon_80_word$word == word_top[i], ]
  test_80 <- rbind(test_80, ex)
}
test_80 <- test_80[-2]
names(test_80)[2] = 'p_1980'


test_90 <- data.frame()

for (i in 1:14){
  ex <- melon_90_word[melon_90_word$word == word_top[i], ]
  test_90 <- rbind(test_90, ex)
}
test_90 <- test_90[-2]
names(test_90)[2] = 'p_1990'


test_00 <- data.frame()

for (i in 1:14){
  ex <- melon_00_word[melon_00_word$word == word_top[i], ]
  test_00 <- rbind(test_00, ex)
}
test_00 <- test_00[-2]
names(test_00)[2] = 'p_2000'


test_10 <- data.frame()

for (i in 1:14){
  ex <- melon_10_word[melon_10_word$word == word_top[i], ]
  test_10 <- rbind(test_10, ex)
}
test_10 <- test_10[-2]
names(test_10)[2] = 'p_2010'


test_20 <- data.frame()

for (i in 1:14){
  ex <- melon_20_word[melon_20_word$word == word_top[i], ]
  test_20 <- rbind(test_20, ex)
}
test_20 <- test_20[-2]
names(test_20)[2] = 'p_2020'


melon_word <- full_join(test_80,test_90,by='word')
melon_word <- full_join(melon_word,test_00,by='word')
melon_word <- full_join(melon_word,test_10,by='word')
melon_word <- full_join(melon_word,test_20,by='word')
melon_word

# 결측값은 0으로 기입
melon_word[is.na(melon_word)] <- 0



# 그래프 생성
melon_word.long <- melon_word %>% 
  gather(category, values, -word) %>%
  filter(category %in% c('word', 'p_1980', 'p_1990', 'p_2000', 'p_2010', 'p_2020')) %>%
  print

p<-ggplot(melon_word.long, aes(x=word, y=values, group=category)) +
  geom_line(aes(color=category), size = 1)+
  geom_point(aes(color=category), size = 3)
p






#######################################################
## 장르비교

melon_genre <- melon_fin$장르
melon_genre <- as.character(melon_genre)
melon_genre
unique(melon_genre)

# "-"라는 값이 있으므로 제거
melon_genre <- melon_genre[! melon_genre %in% "-"]
unique(melon_genre)

length(melon_genre)

# 2가지 장르가 들어가 있는 값은 ', '를 기준으로 나누어서 저장
melon_genre_slice <- c()
for (i in 1:length(melon_genre)){
  genre_slice <- strsplit(melon_genre, split = ', ')
  for (q in 1:length(genre_slice[[i]]))
    melon_genre_slice <- append(melon_genre_slice, genre_slice[[i]][q])
}

melon_genre_grape <- as.data.frame(sort(table(melon_genre_slice), decreasing = TRUE))

ggplot(data = melon_genre_grape)+
  aes(x = melon_genre_slice, y = Freq, group=1)+
  geom_point()+
  geom_line()+
  geom_text(aes(label = Freq), vjust = -0.8, size = 5)


#################################################
## 년도별 장르 비교
genre_y_s <- function(melon_year){
  melon_genre <- melon_year$장르
  melon_genre <- as.character(melon_genre)
  melon_genre <- melon_genre[! melon_genre %in% "-"]
  melon_genre_slice <- c()
  for (i in 1:length(melon_genre)){
    genre_slice <- strsplit(melon_genre, split = ', ')
    for (q in 1:length(genre_slice[[i]]))
      melon_genre_slice <- append(melon_genre_slice, genre_slice[[i]][q])
  }
  melon_genre_grape <- as.data.frame(sort(table(melon_genre_slice), decreasing = TRUE))
}

melon_80_genre <- genre_y_s(melon_80)
melon_90_genre <- genre_y_s(melon_90)
melon_00_genre <- genre_y_s(melon_00)
melon_10_genre <- genre_y_s(melon_10)
melon_20_genre <- genre_y_s(melon_20)

# 나눠진 장르 퍼센트 구하기
melon_80_genre <- wordp(melon_80_genre)
melon_90_genre <- wordp(melon_90_genre)
melon_00_genre <- wordp(melon_00_genre)
melon_10_genre <- wordp(melon_10_genre)
melon_20_genre <- wordp(melon_20_genre)


# 필요없는(원래 갯수) 제거 후 퍼센트 이름 바꾸기
melon_80_genre <- melon_80_genre[-2]
names(melon_80_genre)[2] = "p_1980"

melon_90_genre <- melon_90_genre[-2]
names(melon_90_genre)[2] = "p_1990"

melon_00_genre <- melon_00_genre[-2]
names(melon_00_genre)[2] = "p_2000"

melon_10_genre <- melon_10_genre[-2]
names(melon_10_genre)[2] = "p_2010"

melon_20_genre <- melon_20_genre[-2]
names(melon_20_genre)[2] = "p_2020"


melon_genre <- full_join(melon_80_genre, melon_90_genre,by='melon_genre_slice')
melon_genre <- full_join(melon_genre, melon_00_genre,by='melon_genre_slice')
melon_genre <- full_join(melon_genre, melon_10_genre,by='melon_genre_slice')
melon_genre <- full_join(melon_genre, melon_20_genre,by='melon_genre_slice')
melon_genre

# 결측값 0으로 바꾸고 컬럼 명 쉽게 보기위해 바꾸기
melon_genre[is.na(melon_genre)] <- 0
names(melon_genre)[1] = '장르'
melon_genre


# 그래프 생성
melon_genre.long <- melon_genre %>% 
  gather(category, values, -장르) %>%
  filter(category %in% c('장르', 'p_1980', 'p_1990', 'p_2000', 'p_2010', 'p_2020')) %>%
  print

p<-ggplot(melon_genre.long, aes(x=장르, y=values, group=category)) +
  geom_line(aes(color=category), size = 1)+
  geom_point(aes(color=category), size = 3)
p



####################################
## 가수 비교
####################################
melon_artist <- sort(table(melon_fin$가수), decreasing = TRUE)
head(melon_artist, 10)

melon_artist <- as.data.frame(melon_artist)
str(melon_artist)
melon_artist$Var1 <- as.character(melon_artist$Var1) 
names(melon_artist)[1] = c('artist')
str(melon_artist)
head(melon_artist)

## 년도별 top5 가수
melon_80_artist <- as.data.frame(sort(table(as.character(melon_80$가수)), decreasing = TRUE))
head(melon_80_artist, 6)

melon_90_artist <- as.data.frame(sort(table(as.character(melon_90$가수)), decreasing = TRUE))
head(melon_90_artist, 5)

melon_00_artist <- as.data.frame(sort(table(as.character(melon_00$가수)), decreasing = TRUE))
head(melon_00_artist, 6)

melon_10_artist <- as.data.frame(sort(table(as.character(melon_10$가수)), decreasing = TRUE))
head(melon_10_artist, 5)

melon_20_artist <- as.data.frame(sort(table(as.character(melon_20$가수)), decreasing = TRUE))
head(melon_20_artist, 6)
##################
##여기까지










#####################
## 장르별 단어 연관분석
library(Sejong)
library(hash)
library(tau)
library(RSQLite)
library(KoNLP)
library(rJava)
library(arules)
library(igraph)
library(tidyverse) # 깔끔한 데이터 처리 및 분석을 위한 패키지
library(urltools) # 인코딩 된 문자열을 디코딩하여 확인
library(RColorBrewer) # 다양한 색 포함하는 패키지
library(wordcloud) # 덱스트클라우드를 위한 패키지
library(wordcloud2) # 덱스트클라우드를 위한 패키지2
library(ggplot2)

unique(melon_fin$장르)
unique(melon_genre_slice)
#[1] "포크/블루스"     "록/메탈"         "발라드"          "성인가요/트로트" "댄스"           
#[6] "국내영화"        "국내드라마"      "랩/힙합"         "재즈"            "보컬재즈"       
#[11] "인디음악"        "R&B/Soul"        "애시드/퓨전/팝"  "뉴에이지"        "일렉트로니카"   
#[16] "POP"  

str(melon_fin)
melon_포크 <- melon_fin[grep("포크/블루스", melon_fin$장르),]
str(melon_포크) # 171

melon_록 <- melon_fin[grep("록/메탈", melon_fin$장르),]
str(melon_록) # 382

melon_발라드 <- melon_fin[grep("발라드", melon_fin$장르),]
str(melon_발라드) # 1586

melon_성인가요 <- melon_fin[grep("성인가요/트로트", melon_fin$장르),]
str(melon_성인가요) # 183

melon_댄스 <- melon_fin[grep("댄스", melon_fin$장르),]
str(melon_댄스) # 870

melon_국내영화 <- melon_fin[grep("국내영화", melon_fin$장르),]
str(melon_국내영화) # 15

melon_국내드라마 <- melon_fin[grep("국내드라마", melon_fin$장르),]
str(melon_국내드라마) # 136

melon_랩 <- melon_fin[grep("랩/힙합", melon_fin$장르),]
str(melon_랩) # 335

melon_재즈 <- melon_fin[grep("재즈", melon_fin$장르),]
str(melon_재즈) # 2

melon_보컬재즈 <- melon_fin[grep("보컬재즈", melon_fin$장르),]
str(melon_보컬재즈) # 1

melon_인디음악 <- melon_fin[grep("인디음악", melon_fin$장르),]
str(melon_인디음악) # 74

melon_알앤비 <- melon_fin[grep("R&B/Soul", melon_fin$장르),]
str(melon_알앤비) # 216

melon_애시드 <- melon_fin[grep("애시드/퓨전/팝", melon_fin$장르),]
str(melon_애시드) # 1

melon_뉴에이지 <- melon_fin[grep("뉴에이지", melon_fin$장르),]
str(melon_뉴에이지) # 1

melon_일렉 <- melon_fin[grep("일렉트로니카", melon_fin$장르),]
str(melon_일렉) # 8

melon_팝 <- melon_fin[grep("POP", melon_fin$장르),]
str(melon_팝) # 1


# 포크, 록, 발라드, 성인가요, 댄스, 랩, 인디음악, 알앤비


marketing2 <- as.character(melon_알앤비$가사)

lword <- Map(extractNoun, marketing2)
lword <- unique(lword)

for (i in 1:length(lword)){
  lword[[i]] <- unique(lword[[i]])
}

filter1 <- function(x){
  nchar(x) >= 2
}

filter2 <- function(x){
  Filter(filter1, x)
}

lword <- sapply(lword, filter2)
wordtran <- as(lword, "transactions")
wordtable <- crossTable(wordtran)


transrlues <- apriori(wordtran, parameter = list(support = 0.1, conf = 0.05))

inspect(transrlues)
rules <- labels(transrlues, ruleSep = " ")
head(rules, 20)
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rules
class(rules)
rulemat <- do.call("rbind", rules)
rulemat
relueg <- graph.edgelist(rulemat[c(45:179),], directed = F)
relueg
plot.igraph(relueg)

# 포크[26:41]
# 록[24:47]
# 발라드[32:129]
# 성인가요[21:82]
# 댄스[57:137]
# 랩[95:532]
# 인디[39:110]
# 알앤비[45:179]


###############################
### 감성분석
library(plyr)
library(stringr)


setwd("C:/Users/hsh97/Desktop/data")

positive <- readLines("C:/Users/hsh97/Desktop/data/positive.txt", encoding = "UTF-8")
positive=positive[-1]

negative <- readLines("negative.txt", encoding = "UTF-8")
negative=negative[-1]
negative

sentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence = gsub('[[:punct:]]', '', sentence) # 문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) # 특수문자 제거
    sentence = gsub('\\d+', '', sentence)        # 숫자 제거
    
    word.list = str_split(sentence, '\\s+')      # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list)                    # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, positive)           # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

txt = melon_20$가사
txt = as.character(txt)

result=sentimental(txt, positive, negative)

result$color[result$score >=1] = "blue"
result$color[result$score ==0] = "green"
result$color[result$score < 0] = "red"

result$remark[result$score >=1] = "긍정"

result$remark[result$score ==0] = "중립"

result$remark[result$score < 0] = "부정"


sentiment_result= table(result$remark)



pie(sentiment_result, main="감성분석 결과",
    col=c("blue","red","green"), radius=0.8)


sentiment_result


sample_test <- function(melon_year){
  txt = as.character(melon_year$가사)
  result=sentimental(txt, positive, negative)
  
  result$color[result$score >=1] = "blue"
  result$color[result$score ==0] = "green"
  result$color[result$score < 0] = "red"
  
  result$remark[result$score >=1] = "긍정"
  result$remark[result$score ==0] = "중립"
  result$remark[result$score < 0] = "부정"
  
  sentiment_result= table(result$remark)
  
  pie(sentiment_result, main="감성분석 결과",
      col=c("blue","red","green"), radius=0.8)
  
  sentiment_result
}

sample_test(melon_00)
