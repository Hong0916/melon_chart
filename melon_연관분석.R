library(Sejong)
library(hash)
library(tau)
library(RSQLite)
library(KoNLP) # 한국어 형태소 분석 R 패키지
library(rJava) # KoNLP위한 패키지
library(arules) # 연관성 분석을 위한 패키지
library(igraph) # 연관화 시각화를 위한 패키지
library(tidyverse) # 깔끔한 데이터 처리 및 분석을 위한 패키지

### 데이터 전처리
## 파일 가져오기
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

### 각 장르 별 노래 뽑아서 저장하기
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


## 갯수가 너무 적은거 제외하고 실행
## 포크, 록, 발라드, 성인가요, 댄스, 랩, 인디음악, 알앤비

## 각 노래별 단어 추출
lyric <- melon_알앤비$가사
lword <- Map(extractNoun, lyric) # 각 가사에서 명사단위로 추출
length(lword) # 216
lword <- unique(lword) # 중복된 가사가 있으면 제거
length(lword) # 199
str(lword)
# List of 199
# $ : chr [1:92] "널" "수" "나" "입술" ...
# $ : chr [1:93] "아침" "거리" "허전" "나" ...

## 추출한 리스트마다 그 안에 중복 단어 제거
for (i in 1:length(lword)){
  lword[[i]] <- unique(lword[[i]])
}
str(lword)
# List of 199
# $ : chr [1:37] "널" "수" "나" "입술" ...
# $ : chr [1:39] "아침" "거리" "허전" "나" ...

head(lword, 2)
# [[1]]
# [1] "널"     "수"     "나"     "입술"   "너"     "어깨"   "우린"   "밤"     "끝"     "사랑"  
# [11] "마지막" "입맞춤" "아쉬움" "손"     "내"     "세상"   "선물"   "거"     "누구"   "행복"  
# [21] "하게"   "이상"   "초라"   "마"     "전"     "우리"   "이별"   "생각"   "테"     "가슴"  
# [31] "간직"   "한"     "채"     "눈물"   "니가"   "겠지"   "지"    
# 
# [[2]]
# [1] "아침"     "거리"     "허전"     "나"       "마음"     "내"       "주변"     "사람"    
# [9] "모두"     "친군"     "머리"     "아픈일에" "때"       "생각"     "너"       "여기"    
# [17] "우리"     "이름"     "친구"     "하루"     "어린"     "시절"     "지난"     "얘기"    
# [25] "속"       "지루"     "널"       "기억"     "추억"     "시간"     "음"       "약속"    
# [33] "서로"     "언제"     "위로"     "수"       "영원"     "한"       "일거"  

## 2글자 이상인 단어만 추출하는 함수
filter1 <- function(x){
  nchar(x) >= 2
}

filter2 <- function(x){
  Filter(filter1, x)
}


## 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2)
head(lword, 2)
# [[1]]
# [1] "입술"   "어깨"   "우린"   "사랑"   "마지막" "입맞춤" "아쉬움" "세상"   "선물"   "누구"  
# [11] "행복"   "하게"   "이상"   "초라"   "우리"   "이별"   "생각"   "가슴"   "간직"   "눈물"  
# [21] "니가"   "겠지"  
# 
# [[2]]
# [1] "아침"     "거리"     "허전"     "마음"     "주변"     "사람"     "모두"     "친군"    
# [9] "머리"     "아픈일에" "생각"     "여기"     "우리"     "이름"     "친구"     "하루"    
# [17] "어린"     "시절"     "지난"     "얘기"     "지루"     "기억"     "추억"     "시간"    
# [25] "약속"     "서로"     "언제"     "위로"     "영원"     "일거"   

## 연관성 분석
wordtran <- as(lword, "transactions")
wordtran
# transactions in sparse format with
# 199 transactions (rows) and
# 2626 items (columns)

## 교차표 작성
wordtable <- crossTable(wordtran)
wordtable
# (It’s (늘지금처럼만) (왜냐구? (이제) ‘bout “Come “나와         ́d         ́ll
# ́m         ́s 010 10 100000 11 24 365 3류도 70 about accompanied addicted
# adore afraid again ah ahead ain ain’t Ain’t air alive all All ALL alone
# already alright always Always am Amazed ambiguous an and And anymore anythin
# anything Anything are arms around Around arrow art as ashake ask at attraction

## 단어 간 연관 규칙 산출
transrlues <- apriori(wordtran, parameter = list(support = 0.1, conf = 0.05))
# writing ... [179 rule(s)] done [0.00s].
# 179개의 규칙 확인

## 결과 보기
inspect(transrlues)

## 연관 단어 시각화를 위해 구조 변경
rules <- labels(transrlues, ruleSep = " ")
head(rules, 20)

## 문자열로 묶인 연관 단어를 행렬 구조 변경
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rules
class(rules) # "list"

## 행 단위로 묶어서 matrix로 반환
rulemat <- do.call("rbind", rules)
rulemat

## 단어끼리 연결된 부분만 추출
relueg <- graph.edgelist(rulemat[c(45:179),], directed = F)
relueg

## 연관그래프
plot.igraph(relueg)

# 포크[26:41]
# 록[24:47]
# 발라드[32:129]
# 성인가요[21:82]
# 댄스[57:137]
# 랩[95:532]
# 인디[39:110]
# 알앤비[45:179]



##########################################################################################
melon_igraph <- function(genre) {
  lyric <- genre$가사
  lword <- Map(extractNoun, lyric)
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

  ## 단어 간 연관 규칙 산출
  transrlues <- apriori(wordtran, parameter = list(support = 0.1, conf = 0.05))
  inspect(transrlues)
  rules <- labels(transrlues, ruleSep = " ")
  rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
  rulemat <- do.call("rbind", rules)
}

A <- melon_igraph(melon_댄스)

relueg <- graph.edgelist(A[c(57:137),], directed = F)
relueg

## 연관그래프
plot.igraph(relueg)


B <- melon_igraph(melon_10)
relueg <- graph.edgelist(B[c(51:104),], directed = F)
relueg

## 연관그래프
plot.igraph(relueg)
