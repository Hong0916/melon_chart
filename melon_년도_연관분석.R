library(KoNLP) # 한국어 형태소 분석 R 패키지
library(rJava) # KoNLP위한 패키지
library(arules) # 연관성 분석을 위한 패키지
library(igraph) # 연관화 시각화를 위한 패키지

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

# 각 년도별로 나누기
melon_80 <- subset(melon_fin, 년도<1990)
melon_90 <- subset(melon_fin, 년도>=1990 & 년도<2000)
melon_00 <- subset(melon_fin, 년도>=2000 & 년도<2010)
melon_10 <- subset(melon_fin, 년도>=2010 & 년도<2020)
melon_20 <- subset(melon_fin, 년도>=2020)

## 각 년도별 단어 추출
lyric <- melon_80$가사 # 80년대 가사 활용
lword <- Map(extractNoun, lyric) # 각 가사에서 명사단위로 추출
length(lword) # 500
lword <- unique(lword) # 중복된 가사가 있으면 제거
length(lword) # 426
str(lword)
# List of 426
# $ : chr [1:32] "눈물" "줄" "사랑" "줄" ...
# $ : chr [1:86] "바람" "소리" "인생" "길" ...

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
# [1] "눈물"  "사랑"  "오늘"  "자욱"  "연기"  "사이"  "어둔"  "기억"  "마음"  "구석"  "불씨"  "*이제"
# [13] "진정"  "얘기"  "눈길"  "그대"  "반복" 
# 
# [[2]]
# [1] "바람" "소리" "인생" "우린" "사랑" "가슴" "정처" "방황" "사람" "시작" "어제" "오늘" "우리" "무엇"
# [15] "미련"


## 연관성 분석
wordtran <- as(lword, "transactions")
wordtran
# transactions in sparse format with
# 426 transactions (rows) and
# 2345 items (columns)

## 교차표 작성
wordtable <- crossTable(wordtran)
wordtable
# *repeat>흰 *이제 2떠나가는 al all alta amor and any arrastra ASIA barco
# beams birthday boughs bread but buy can carino Carino Chiquilla como
# corazon CRACKER crinkle crust daffodils day directo DJ do dollars donde

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
relueg <- graph.edgelist(rulemat[c(18:58),], directed = F)
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
melon_igraph <- function(year) {
  lyric <- year$가사
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

A <- melon_igraph(melon_80)

relueg <- graph.edgelist(A[c(18:58),], directed = F)
relueg

## 연관그래프
plot.igraph(relueg)


B <- melon_igraph(melon_00)
relueg <- graph.edgelist(B[c(42:211),], directed = F)
relueg

## 연관그래프
plot.igraph(relueg)

C <- melon_igraph(melon_20)
relueg <- graph.edgelist(C[c(55:207),], directed = F)
relueg

## 연관그래프
plot.igraph(relueg)
