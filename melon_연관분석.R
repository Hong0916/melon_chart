library(Sejong)
library(hash)
library(tau)
library(RSQLite)
library(KoNLP) # 한국어 형태소 분석 R 패키지
library(rJava) # KoNLP위한 패키지
library(arules)
library(igraph)
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

## 
lyric <- melon_알앤비$가사
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