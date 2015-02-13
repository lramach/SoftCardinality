#Computing soft cardinality for a text and a pair of texts. --> generating a set of features we can input to the learner

#We compute the cardinalities of each of the texts individually as well as their union, intersection etc.
#Consider a pair of texts T1 and T2 that are being compared, where T1 could be the student response and T2 the reference text.
#The features we compute would be: |T1|, |T2|, |T1 n T2|, |T1 u T2|,  |T1-T2|, |T2-T1| 

#The soft cardinality and similarity equations used in this file are from the paper by Jimenez et al.:
##Jimenez, Sergio, et al. "SOFTCARDINALITY: hierarchical text overlap for student response analysis." Second Joint Conference on Lexical and Computational Semantics (* SEM). Vol. 2. 2013. 

library(tm)
library(stringr) #to use str_trim to trim texts
library(gtools)
essays = get("spellcorrected.testUA.beetleCore")
responses = as.matrix(essays[,"stem.spellcheck"])
cardFeatures = matrix(ncol=42)
refText = as.matrix(essays[,"referenceAnswer"])
qnText = as.matrix(essays[,"questionText"])

i = 1
while(i < length(responses)+1){ #iterate over the responses
  featureVec = c()
  responses[i] = str_trim(responses[i]) #trimming text
  refText[i] = str_trim(refText[i]) #trimming text
  qnText[i] = str_trim(qnText[i]) #trimming text
  
  #|A|' -- student answer
  ATokens = strsplit(responses[i], split=" ")[[1]]
  ACard = compSimpleCard(ATokens, 1, rep(1, length(ATokens)), 4) 
  #|Q|' -- question text
  QTokens = strsplit(qnText[i], split=" ")[[1]]
  QCard = compSimpleCard(QTokens, 1, rep(1, length(QTokens)), 4)
  #|RA|'' -- reference answers, each answer is treated as a new sentence
  if(grepl("@@", refText[i]) != TRUE){
    RASet = refText[i] 
  }else{
    out = unlist(strsplit(refText[i], split="@@"))
    RASet = out
  }
  RACard = compSentCard(RASet)
  #|QuA|'
  QuATokens = strsplit(paste(qnText[i], responses[i]), split=" ")[[1]]
  QuACard = compSimpleCard(QuATokens, 1, rep(1, length(QuATokens)), 4)
  #|RAuA|''
  RAuASet = c(refText[i], responses[i])
  RAuACard = compSentCard(RAuASet)
  #|RAuQ|''
  RAuQSet = c(refText[i], qnText[i])
  RAuQCard = compSentCard(RAuQSet)
 
  #compute extended features from Table (1) using equations in Table (2)
  #|X| = |Q|', |Y| = |A|', |XuY| = |QuA|'
  Row1ExtFeats = compExtendFeats(QCard, ACard, QuACard)
  #|X| = |A|', |Y| = |RA|'', |XuY| = |RAuA|''
  Row2ExtFeats = compExtendFeats(ACard, RACard, RAuACard)
  #|X| = |Q|', |Y| = |RA|'', |XuY| = |RAuQ|''
  Row3ExtFeats = compExtendFeats(QCard, RACard, RAuQCard)
  #combining all the features
  featureVec = c(ACard, QCard, RACard, QuACard, RAuACard, RAuQCard, Row1ExtFeats, Row2ExtFeats, Row3ExtFeats)
  #binding features to a dataset
  cardFeatures = rbind(cardFeatures, featureVec)
  i=i+1
}
#set colnames
colnames(cardFeatures) = c("Acard", "Qcard", "RACard", "QuACard", "RAuACard", "RAuQCard", paste0("One.",c("XnY", "XbyY", "YbyX", "XnYbyX", "XnYbyY", "XnYbyXuY", "EF7", "EF8", "EF9", "EF10", "EF11", "EF12")), paste0("Two.", c("XnY", "XbyY", "YbyX", "XnYbyX", "XnYbyY", "XnYbyXuY", "EF7", "EF8", "EF9", "EF10", "EF11", "EF12")), paste0("Three.", c("XnY", "XbyY", "YbyX", "XnYbyX", "XnYbyY", "XnYbyXuY", "EF7", "EF8", "EF9", "EF10", "EF11", "EF12")))

#using Equations (2) and (3) from the paper on soft cardinality
compSimpleCard <- function(tokens, p, weights, qgrams){
  toksum = 0
  tokens = unlist(tokens)
  for(token1 in tokens){
    token1 = tolower(token1)
    token1set = unlist(apply(as.matrix(c(1:nchar(token1))), 1, function(x) {out = substr(token1, x, x+(qgrams-1));  if(nchar(out) >= qgrams) out;}))
    sum = 0
    for(token2 in tokens){
      token2 = tolower(token2)
      token2set = unlist(apply(as.matrix(c(1:nchar(token2))), 1, function(x) {out = substr(token2, x, x+(qgrams-1));  if(nchar(out) >= qgrams) out;}))
      tok1ntok2 = intersect(token1set, token2set)
      #compute similarity
      sim = (2 * length(tok1ntok2))/ (length(token1set) + length(token2set)) 
      sum = sum + sim
    }
    if(!is.na(sum) && sum != 0){
      inverse = 1/sum
    } else {
      inverse = 0
    }
    toksum = toksum + inverse
  }
  print(paste("toksum: ", toksum))
  return(toksum)
}

#using Equations (5) and (4) from the paper to compute cardinalities across sentences
compSentCard <- function(sentences){
  sentSum = 0
  for(s1 in sentences){
    s1 = str_trim(tolower(s1))
    #|S1|' 
    s1tokens = strsplit(s1, split=" ")[[1]]
    s1Card = compSimpleCard(s1tokens, 1, rep(1, length(s1tokens)), 4)
    sum = 0
    for(s2 in sentences){
      s2 = str_trim(tolower(s2))
      #|S2|'
      s2tokens = strsplit(s2, split=" ")[[1]]
      s2Card = compSimpleCard(s2tokens, 1, rep(1, length(s2tokens)), 4)
      #|S1uS2|'
      s1us2tokens = strsplit(paste(s1, s2), split=" ")[[1]]
      s1us2Card = compSimpleCard(s1us2tokens, 1, rep(1, length(s1us2tokens)), 4)
      print(paste("s1Card:",s1Card, "s2Card:",s2Card, "s1us2Card:", s1us2Card))
      sim = (2 * (s1Card + s2Card - s1us2Card))/(s1Card + s2Card)
      sum = sum + sim 
    }
    if(!is.na(sum) && sum != 0){
      inverse = 1/sum
    } else {
      inverse = 0
    }
    sentSum = sentSum + (s1Card * inverse)
  }
  print(paste("sentSum:", sentSum))
  return(sentSum)
}

compExtendFeats <- function(X, Y, XuY){
  #|XnY|
  XnY = X+Y-XuY
  #|X\Y|
  XbyY = X-XnY
  #|Y\X|
  YbyX = Y-XnY
  #|XnY|/|X|
  if(X != 0){
    XnYbyX = XnY/X
  }else{
    XnYbyX = 0
  }
  #|XnY|/|Y|
  if(Y != 0){
    XnYbyY = XnY/Y
  }else{
    XnYbyY = 0
  }
  #|XnY|/|XuY|
  if(XuY != 0){
    XnYbyXuY = XnY/XuY
  }else{
    XnYbyXuY = 0
  }
  #EF7 
  if(X+Y != 0){
    EF7 = (2*XnY)/(X+Y)
  }else{
    EF7 = 0
  }
  #EF9
  if(min(X,Y)!=0){
    EF9 = (XnY)/min(X,Y)
  }else{
    EF9 = 0
  }
  #EF10
  if(max(X,Y)!=0){
    EF10 = (XnY)/max(X,Y)
  } else{
    EF10 = 0
  }
  #EF11 && EF8
  if(X!=0 && Y!=0){
    EF8 = (XnY)/sqrt(X*Y)
    EF11 = (XnY * (X+Y))/(2*X*Y)
  }else{
    EF11 = 0
    EF8 = 0
  }
  #EF12
  EF12 = XuY - XnY
  return(c(XnY, XbyY, YbyX, XnYbyX, XnYbyY, XnYbyXuY, EF7, EF8, EF9, EF10, EF11, EF12))
}
