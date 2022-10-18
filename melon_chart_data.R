library(KoNLP) # 한국어 형태소 분석 R 패키지
library(rJava) # KoNLP위한 패키지
library(tidyverse) # 깔끔한 데이터 처리 및 분석을 위한 패키지
library(urltools) # 인코딩 된 문자열을 디코딩하여 확인
library(RColorBrewer) # 다양한 색 포함하는 패키지
library(wordcloud) # 덱스트클라우드를 위한 패키지
library(wordcloud2) # 덱스트클라우드를 위한 패키지2
library(ggplot2) # 그래프를 위한 패키지

### 데이터 전처리
## 크롤링한 멜론 노래 파일 가져오기
melon_fin <- read.csv('C:/Users/hsh97/Desktop/data/melon_fin_ANSI.csv', encoding = 'utf-8', header = T)
str(melon_fin)
# 'data.frame':	3699 obs. of  7 variables:
# $ 제목    : Factor w/ 3169 levels "'친구'라 말할 수 있는 건",..: 2657 2362 3162 2263 1588 1696 3015 1673 1552 2622 ...
# $ 가수    : Factor w/ 1067 levels "#안녕","(여자)아이들",..: 1024 887 158 836 1024 221 527 196 1020 717 ...
# $ 장르    : Factor w/ 42 levels "-","POP","R&B/Soul",..: 41 16 30 8 41 41 16 33 41 41 ...
# $ 가사    : Factor w/ 3269 levels "","- 쏘리 쏘리 (Sorry, Sorry) -\n\nSorry Sorry Sorry Sorry\n내가 내가 내가 먼저\n네게 네게 네게 빠져\n빠져 빠져 버"| __truncated__,..: 960 1748 1924 2172 1673 3071 3092 1699 1597 1514 ...
# $ 좋아요  : Factor w/ 3102 levels "0","1","1,000",..: 2785 127 2424 16 2294 152 1597 1636 1364 1866 ...
# $ 년도    : int  1985 1985 1985 1985 1985 1985 1985 1985 1985 1985 ...
# $ 노래번호: int  2036556 68142 3328252 48478 992352 51971 733017 1028281 637620 1569503 ...

## 제목, 가수, 장르, 가사, 좋아요가 Factor형이므로 문자형 변환(좋아요는 숫자형)
melon_fin$제목 <- as.character(melon_fin$제목)
melon_fin$가수 <- as.character(melon_fin$가수)
melon_fin$장르 <- as.character(melon_fin$장르)
melon_fin$가사 <- as.character(melon_fin$가사)
melon_fin$좋아요 <- as.integer(melon_fin$좋아요)
str(melon_fin)
# 'data.frame':	3699 obs. of  7 variables:
# $ 제목    : chr  "이젠 사랑할 수 있어요" "어제, 오늘, 그리고" "희나리" "아직도 어두운 밤인가봐" ...
# $ 가수    : chr  "해바라기" "조용필" "구창모" "전영록" ...
# $ 장르    : chr  "포크/블루스" "록/메탈" "발라드, 성인가요/트로트" "댄스" ...
# $ 가사    : chr  "난 눈물이 메마른 줄 알았어요\n여태 사랑을 다시 못할 줄 알았어요\n오늘 난 자욱한 연기 사이로 사랑의 짝을 보았어"| __truncated__ "바람소리처럼\n멀리 사라져갈 인생길\n우린 무슨 사랑\n어떤 사랑했나\n텅 빈 가슴속에\n가득 채울 것을 찾아서\n우린 "| __truncated__ "사랑함에 세심했던 나의 마음이\n그렇게도 그대에겐 구속이었소\n\n믿지못해 그런 것이 아니었는데\n어쩌다가 헤어지는"| __truncated__ "아직도 어두운 밤인가봐\n하늘엔 반짝이는 별들이\n내 모습을 가끔 쳐다보네\n아직도 어두운 밤인가봐\n지금은 지나버"| __truncated__ ...
# $ 좋아요  : int  2785 127 2424 16 2294 152 1597 1636 1364 1866 ...
# $ 년도    : int  1985 1985 1985 1985 1985 1985 1985 1985 1985 1985 ...
# $ 노래번호: int  2036556 68142 3328252 48478 992352 51971 733017 1028281 637620 1569503 ...

## 결측치 확인
sum(is.na(melon_fin)) # 0



### 단어 분류
## 명사 추출을 위해 KoNLP에 저장되어있는 사전을 실행
useSejongDic()

## 명사를 추출해주는 함수 
extract_words <- function(lyric){
  keyword <- paste(SimplePos09(lyric)) %>% # SimplePos09() : 9개의 품사로 분류
    # paste() : 두개의 argument를 넣어서 붙여주는 함수
    str_match("([a-zA-Z가-힣]+)/.")   # str_match() : pattern에 맞는 값을 반환 # 한글, 영어만 추출
  keyword <- keyword[, 2]   # 품사가 적혀있지 않는 2열의 값만 추출
  return(keyword[!is.na(keyword)])  # NA값을 제거한 단어만 반환
}

## for문을 통해서 분리된 단어를 melon_word 벡터에 저장
melon_word <- c()
for(i in 1:nrow(melon_fin)) {
  melon_word <- append(melon_word, extract_words(melon_fin$가사[i]))
}

head(sort(table(melon_word), decreasing = TRUE))
tail(sort(table(melon_word), decreasing = TRUE))

# melon_word
#melon_word
# 나    너    내    하  사랑    말 
# 25093 13835 10644  8900  8439  8123 

summary(melon_word)
# Length     Class      Mode 
# 628512 character character

## 데이터 전처리
melon_word <- as.data.frame(table(melon_word))
str(melon_word)
# 'data.frame':	25636 obs. of  2 variables:
# $ melon_word: Factor w/ 25636 levels "a","A","AA","AAA",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ Freq      : int  1040 262 2 3 2 1 154 2 1 1 ...

# Factor형을 문자형으로 변환
melon_word$melon_word <- as.character(melon_word$melon_word) 
head(melon_word)
# melon_word Freq
# 1          a 1040
# 2          A  262
# 3         AA    2
# 4        AAA    3
# 5         AB    2
# 6       abou    1

## 단어 2글자 이상이고 5번 이상 나온 단어만 수집
melon_word <- melon_word %>% 
  rename(word = melon_word) %>%
  dplyr::filter(nchar(word) >= 2 & Freq >= 5) %>% 
  arrange(desc(Freq))

head(melon_word)
# word Freq
# 1 사랑 8439
# 2 그대 6061
# 3  you 3269
# 4 우리 2711
# 5 다시 2658
# 6 떠나 2655



### wordcloud를 통해 단어 시각화
## wordcloud
pal <- brewer.pal(8, "Dark2") # brewer.pal() 함수를 통해 색 지정
set.seed(1000) # 동일한 임의 순서를 위해 난수 발생
wordcloud(words = melon_word$word,   # 표시할 단어
          freq = melon_word$Freq,    # 단어 빈도
          min.freq = 5,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = FALSE,  # 단어 배치 FALSE(고빈도 단어 중앙 배치)
          rot.per = .1,          # 회전 단어 비율
          scale = c(5, 0.5),     # 단어 크기 범위
          colors = pal)           # 색깔

## wordcloud2
# wordcloud와는 다르게 데이터만 넣어주면 시각화 표현
wordcloud2(data = melon_word,color = "random-light", backgroundColor = "#3D3D3E",fontFamily = '나눔바른고딕')

### 년도별 비교
## 10년 단위로 단어 수 비교(ex: 85~89, 90~99, ..., 20~21)
melon_80 <- subset(melon_fin, 년도<1990)
melon_90 <- subset(melon_fin, 년도>=1990 & 년도<2000)
melon_00 <- subset(melon_fin, 년도>=2000 & 년도<2010)
melon_10 <- subset(melon_fin, 년도>=2010 & 년도<2020)
melon_20 <- subset(melon_fin, 년도>=2020)


## 명사를 추출해주는 함수 
# 위의 코드를 함수화 한 것
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

## 각 년도별 단어 분할
melon_80_word = wordmake(melon_80)
head(melon_80_word)
# word Freq
# 1 그대 1121
# 2 사랑 1118
# 3 떠나  495
# 4 마음  417
# 5 슬프  324
# 6 우리  308

melon_90_word = wordmake(melon_90)
melon_00_word = wordmake(melon_00)
melon_10_word = wordmake(melon_10)
melon_20_word = wordmake(melon_20)

## 각 년도별 단어 wordcloud2
wordcloud2(melon_80_word,color = "random-light", backgroundColor = "#3D3D3E",fontFamily = '나눔바른고딕')
wordcloud2(melon_90_word,color = "random-light", backgroundColor = "#3D3D3E",fontFamily = '나눔바른고딕')
wordcloud2(melon_00_word,color = "random-light", backgroundColor = "#3D3D3E",fontFamily = '나눔바른고딕')
wordcloud2(melon_10_word,color = "random-light", backgroundColor = "#3D3D3E",fontFamily = '나눔바른고딕')
wordcloud2(melon_20_word,color = "random-light", backgroundColor = "#3D3D3E",fontFamily = '나눔바른고딕')


### 년도별 가장 많이 나온 단어 비율 수로 구하기
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
head(melon_80_word)
# word Freq     p
# 1 그대 1121 5.027
# 2 사랑 1118 5.014
# 3 떠나  495 2.220
# 4 마음  417 1.870
# 5 슬프  324 1.453
# 6 우리  308 1.381
melon_90_word <- wordp(melon_90_word)
melon_00_word <- wordp(melon_00_word)
melon_10_word <- wordp(melon_10_word)
melon_20_word <- wordp(melon_20_word)



## 각 년도별 가장 많이 나온 단어 6개씩 추출
word_top <- head(melon_80_word)
word_top <- word_top$word
str(word_top)
word_top <- append(word_top, head(melon_90_word)$word)
word_top <- append(word_top, head(melon_00_word)$word)
word_top <- append(word_top, head(melon_10_word)$word)
word_top <- append(word_top, head(melon_20_word)$word)

word_top
# [1] "그대" "사랑" "떠나" "마음" "슬프" "우리" "사랑" "그대" "떠나" "마음" "다시" "이제" "사랑" "you"  "me"   "그대" "모르" "Oh"  
# [19] "you"  "사랑" "me"   "the"  "da"   "그대"

## 중복값 제거
word_top <- unique(word_top)
word_top
# [1] "그대" "사랑" "떠나" "마음" "슬프" "우리" "다시" "이제" "you"  "me"   "모르" "Oh"   "the"  "da"  

## word_top의 단어 각 년도별로 비율 구하기
test_80 <- data.frame()
for (i in 1:14){
  ex <- melon_80_word[melon_80_word$word == word_top[i], ] # word_top의 단어 가 포함되어있으면 ex로 지정
  test_80 <- rbind(test_80, ex) # test_80데이터 프레임에 추가하기
}
test_80 <- test_80[-2] # 실제 갯수는 필요 없으므로 제거
names(test_80)[2] = 'p_1980' # p라는 컬럼명을 년도까지 붙여서 변경

## 모든 년도 반복
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

## 만든 test 데이터 프레임 합치기
melon_word <- full_join(test_80,test_90,by='word') # word 컬럼명을 기준으로 full join 하기
melon_word <- full_join(melon_word,test_00,by='word')
melon_word <- full_join(melon_word,test_10,by='word')
melon_word <- full_join(melon_word,test_20,by='word')
melon_word
#    word p_1980 p_1990  p_2000  p_2010 p_2020
# 1  그대 5.0274 2.5631 2.06689 0.83240 1.0586
# 2  사랑 5.0139 3.1485 3.09682 1.58596 1.3824
# 3  떠나 2.2199 1.2173 0.81153 0.40583 0.3625
# 4  마음 1.8701 1.1244 0.57508 0.43487 0.4592
# 5  슬프 1.4530 0.8394 0.38773 0.20914 0.1643
# 6  우리 1.3813 1.0578 0.67026 0.72617 0.8507
# 7  다시 0.9328 1.1089 0.98986 0.50127 0.6864
# 8  이제 0.9732 1.0779 0.65022 0.42408 0.4495
# 9   you 0.1031 0.3252 1.21729 1.27142 1.3969
# 10 모르 0.7176 0.7124 0.79850 0.75854 0.4640
# 11   me     NA 0.1936 0.72136 0.86477 1.3389
# 12   Oh     NA 0.1146 0.22943 0.74443 0.6671
# 13  the     NA 0.2726 0.56105 0.40832 1.2181
# 14   da     NA     NA 0.01803 0.00415 1.0828

## 결측값은 0으로 기입
melon_word[is.na(melon_word)] <- 0


## 선 그래프 생성
## 단어, 년도, 비율로 데이터 프레임 재조립
melon_word.long <- melon_word %>% 
  gather(year, p, -word) %>%
  filter(year %in% c('word', 'p_1980', 'p_1990', 'p_2000', 'p_2010', 'p_2020')) %>%
  print

ggplot(melon_word.long, aes(x=word, y=p, group=year)) +
  geom_line(aes(color=year), size = 1)+
  geom_point(aes(color=year), size = 3)




### 장르비교
## 모든 장르 가져오기
melon_genre <- melon_fin$장르
unique(melon_genre)
# [1] "포크/블루스"                  "록/메탈"                      "발라드, 성인가요/트로트"      "댄스"                        
# [5] "성인가요/트로트"              "발라드"                       "국내영화"                     "발라드, 록/메탈"             
# [9] "댄스, 성인가요/트로트"        "포크/블루스, 국내드라마"      "록/메탈, 포크/블루스"         "발라드, 포크/블루스"         
# [13] "록/메탈, 국내영화"            "-"                            "국내드라마"                   "록/메탈, 국내드라마"         
# [17] "발라드, 국내영화"             "발라드, 국내드라마"           "랩/힙합"                      "재즈, 보컬재즈"              
# [21] "인디음악, 포크/블루스"        "R&B/Soul"                     "발라드, 댄스, 랩/힙합"        "인디음악, 록/메탈"           
# [25] "발라드, 댄스"                 "재즈, 애시드/퓨전/팝"         "댄스, 랩/힙합"                "댄스, R&B/Soul"              
# [29] "뉴에이지"                     "발라드, 랩/힙합"              "발라드, R&B/Soul"             "일렉트로니카"                
# [33] "POP"                          "일렉트로니카, 국내드라마"     "발라드, R&B/Soul, 국내드라마" "댄스, 일렉트로니카"          
# [37] "랩/힙합, 국내드라마"          "랩/힙합, 인디음악"            "발라드, 랩/힙합, 국내드라마"  "발라드, 인디음악"            
# [41] "인디음악, 일렉트로니카"       "R&B/Soul, 인디음악"  
length(unique(melon_genre)) # 42

## "-"라는 값이 있으므로 제거
melon_genre <- melon_genre[! melon_genre %in% "-"]
length(unique(melon_genre)) # 41

## 2가지 장르가 들어가 있는 값은 ', '를 기준으로 나누어서 저장
melon_genre_slice <- c()
for (i in 1:length(melon_genre)){
  genre_slice <- strsplit(melon_genre, split = ', ') # ', '를 기준으로 장르 분리
  for (q in 1:length(genre_slice[[i]]))
    melon_genre_slice <- append(melon_genre_slice, genre_slice[[i]][q]) # 분리된 단어 melon_genre_slice에 기입
}
unique(melon_genre_slice)
# [1] "포크/블루스"     "록/메탈"         "발라드"          "성인가요/트로트" "댄스"            "국내영화"        "국내드라마"     
# [8] "랩/힙합"         "재즈"            "보컬재즈"        "인디음악"        "R&B/Soul"        "애시드/퓨전/팝"  "뉴에이지"       
# [15] "일렉트로니카"    "POP" 

## 데이터 프레임으로 변환 후 그래프 만들어 결과 확인
melon_genre_grape <- as.data.frame(sort(table(melon_genre_slice), decreasing = TRUE))

ggplot(data = melon_genre_grape)+
  aes(x = melon_genre_slice, y = Freq, group=1)+
  geom_point()+
  geom_line()+
  geom_text(aes(label = Freq), vjust = -0.8, size = 5)



### 년도별 장르 비교
## 위의 장르 분류기 함수로 만들기
genre_y_s <- function(melon_year){
  melon_genre <- melon_year$장르 # 각 년도별 장르 뽑아오기
  melon_genre <- melon_genre[! melon_genre %in% "-"] # '-' 제외
  melon_genre_slice <- c() # 아래부터 위의 과정과 같음
  for (i in 1:length(melon_genre)){
    genre_slice <- strsplit(melon_genre, split = ', ')
    for (q in 1:length(genre_slice[[i]]))
      melon_genre_slice <- append(melon_genre_slice, genre_slice[[i]][q])
  }
  melon_genre_grape <- as.data.frame(sort(table(melon_genre_slice), decreasing = TRUE))
}

## 함수 실행
melon_80_genre <- genre_y_s(melon_80)
melon_90_genre <- genre_y_s(melon_90)
melon_00_genre <- genre_y_s(melon_00)
melon_10_genre <- genre_y_s(melon_10)
melon_20_genre <- genre_y_s(melon_20)

## 나눠진 장르 퍼센트 구하기
# 위의 단어 소수점 만드는 함수 사용
melon_80_genre <- wordp(melon_80_genre)
melon_90_genre <- wordp(melon_90_genre)
melon_00_genre <- wordp(melon_00_genre)
melon_10_genre <- wordp(melon_10_genre)
melon_20_genre <- wordp(melon_20_genre)


## 필요없는(원래 갯수) 제거 후 퍼센트 이름 바꾸기
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

## full_join으로 합치기
melon_genre <- full_join(melon_80_genre, melon_90_genre,by='melon_genre_slice')
melon_genre <- full_join(melon_genre, melon_00_genre,by='melon_genre_slice')
melon_genre <- full_join(melon_genre, melon_10_genre,by='melon_genre_slice')
melon_genre <- full_join(melon_genre, melon_20_genre,by='melon_genre_slice')
melon_genre

# 결측값 0으로 바꾸고 컬럼 명 쉽게 보기위해 바꾸기
melon_genre[is.na(melon_genre)] <- 0
names(melon_genre)[1] = '장르'
melon_genre
# 장르  p_1980  p_1990   p_2000   p_2010 p_2020
# 1           발라드 38.9105 48.8372 42.01128 31.37778 33.198
# 2  성인가요/트로트 26.2646  3.9729  0.09398  0.08889  2.024
# 3      포크/블루스 13.8132  5.7171  0.56391  2.84444  1.215
# 4          록/메탈 12.0623 13.1783 10.05639  5.24444  7.287
# 5             댄스  8.1712 21.5116 23.49624 27.28889 19.838
# 6         국내영화  0.5837  0.8721  0.28195  0.00000  0.000
# 7       국내드라마  0.1946  1.2597  1.97368  6.31111 12.146
# 8          랩/힙합  0.0000  2.8101  9.68045 15.11111 13.360
# 9         R&B/Soul  0.0000  0.7752 10.80827  7.37778  4.049
# 10        인디음악  0.0000  0.5814  0.75188  3.82222  6.883
# 11            재즈  0.0000  0.1938  0.00000  0.00000  0.000
# 12        뉴에이지  0.0000  0.0969  0.00000  0.00000  0.000
# 13        보컬재즈  0.0000  0.0969  0.00000  0.00000  0.000
# 14  애시드/퓨전/팝  0.0000  0.0969  0.00000  0.00000  0.000
# 15    일렉트로니카  0.0000  0.0000  0.18797  0.53333  0.000
# 16             POP  0.0000  0.0000  0.09398  0.00000  0.000


## 그래프 생성
melon_genre.long <- melon_genre %>% 
  gather(year, p, -장르) %>%
  filter(year %in% c('장르', 'p_1980', 'p_1990', 'p_2000', 'p_2010', 'p_2020')) %>%
  print

ggplot(melon_genre.long, aes(x=장르, y=p, group=year)) +
  geom_line(aes(color=year), size = 1)+
  geom_point(aes(color=year), size = 3)


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
test <- head(melon_artist)
test
str(test)
test_A = melon_fin[melon_fin$가수 == test$artist,]
str(test_A)
test_A = c(test$artist)
test_A
test_B = melon_fin[melon_fin$가수 == test_A,]
str(test_B)
test_B$가수 == '이선희'
head(test_B)

IU <- melon_fin[melon_fin$가수 == '아이유',]
bigbang <- melon_fin[melon_fin$가수 == 'BIGBANG (빅뱅)',]
bts <- melon_fin[melon_fin$가수 == '방탄소년단',]
sg <- melon_fin[melon_fin$가수 == 'SG 워너비',]
dabi <- melon_fin[melon_fin$가수 == '다비치',]
lee <- melon_fin[melon_fin$가수 == '이선희',]
unique(IU$장르)
unique(bigbang$장르)
unique(bts$장르)
unique(sg$장르)
unique(dabi$장르)
unique(lee$장르)

melon_top <- full_join(IU, bigbang)
melon_top <- full_join(melon_top, bts)
melon_top <- full_join(melon_top, sg)
melon_top <- full_join(melon_top, dabi)
melon_top <- full_join(melon_top, lee)

str(melon_top)
melon_top <- melon_top[2:3]
head(melon_top)
unique(melon_top$장르)
