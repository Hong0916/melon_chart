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