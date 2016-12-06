#Working Directory

setwd("")

###LIBRARIES
library(uba)
library(performanceEstimation)
library(lubridate)
library(e1071)
library(DMwR)
library(randomForest)
library(earth)
library(UBL)
library(qdap)

#### RUN ONCE

# library(openNLP)
# library(NLP)
# 
# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# word_token_annotator <- Maxent_Word_Token_Annotator()
# 
# person_annotator <- Maxent_Entity_Annotator(kind="person",language="en",model='/home/nmoniz/TIST/en-ner-person.bin')
# org_annotator <- Maxent_Entity_Annotator(kind="organization",language="en",model='/home/nmoniz/TIST/en-ner-organization.bin')
# loc_annotator <- Maxent_Entity_Annotator(kind="location",language="en",model='/home/nmoniz/TIST/en-ner-location.bin')

#LOAD DATA
#DATA LOADING
news <- read.csv("Links_clean.csv")
news$PublishDate <- as.POSIXct(news$PublishDate)

#LOAD NEWS
frame_train.headline.economy <- read.csv("HeadlineEconomy.csv")
frame_train.headline.economy$PublishDate <- NULL
frame_train.headline.microsoft <- read.csv("HeadlineMicrosoft.csv")
frame_train.headline.microsoft$PublishDate <- NULL
frame_train.headline.obama <- read.csv("HeadlineObama.csv")
frame_train.headline.obama$PublishDate <- NULL
frame_train.headline.palestine <- read.csv("HeadlinePalestine.csv")
frame_train.headline.palestine$PublishDate <- NULL

frame_train.headline.economy <- frame_train.headline.economy[frame_train.headline.economy$TimesPublishedTwitter>0,]
frame_train.headline.microsoft <- frame_train.headline.microsoft[frame_train.headline.microsoft$TimesPublishedTwitter>0,]
frame_train.headline.obama <- frame_train.headline.obama[frame_train.headline.obama$TimesPublishedTwitter>0,]
frame_train.headline.palestine <- frame_train.headline.palestine[frame_train.headline.palestine$TimesPublishedTwitter>0,]

#LOAD RANKINGS AND ADAPT TO DATASET
# ranktable <- read.csv("Data/RankTable.csv")
ranktable <- read.csv("RankTable.csv")
columnames <- colnames(ranktable)
columnames <- columnames[2:length(columnames)]
ranktable["TgtDummy"] <- factor(sample(columnames,nrow(ranktable),replace=T),levels=columnames)
ranktable$Timestamp <- as.POSIXct(ranktable$Timestamp)

ranktable.economy <- read.csv("RankTable_Economy.csv")
columnames <- colnames(ranktable.economy)
columnames <- columnames[2:length(columnames)]
ranktable.economy["TgtDummy"] <- factor(sample(columnames,nrow(ranktable.economy),replace=T),levels=columnames)
ranktable.microsoft <- read.csv("RankTable_Microsoft.csv")
columnames <- colnames(ranktable.microsoft)
columnames <- columnames[2:length(columnames)]
ranktable.microsoft["TgtDummy"] <- factor(sample(columnames,nrow(ranktable.microsoft),replace=T),levels=columnames)
ranktable.obama <- read.csv("RankTable_Obama.csv")
columnames <- colnames(ranktable.obama)
columnames <- columnames[2:length(columnames)]
ranktable.obama["TgtDummy"] <- factor(sample(columnames,nrow(ranktable.obama),replace=T),levels=columnames)
ranktable.palestine <- read.csv("RankTable_Palestine.csv")
columnames <- colnames(ranktable.palestine)
columnames <- columnames[2:length(columnames)]
ranktable.palestine["TgtDummy"] <- factor(sample(columnames,nrow(ranktable.palestine),replace=T),levels=columnames)


#EVALUATION
eval.stats <- function(form,train,test,preds,ph,ls) {
  
  trues <- resp(form,test)
  
  preds[preds<0] <- 0
  u_new <- utilOneBump(form,train,test,preds,ph,3,0.9,FALSE)
  
  prec <- utilOB(preds,trues,ph,ls,util.control(umetric="P",event.thr=0.9),ut=u_new)
  rec  <- utilOB(preds,trues,ph,ls,util.control(umetric="R",event.thr=0.9),ut=u_new)
  F05  <- utilOB(preds,trues,ph,ls,util.control(umetric="Fm",beta=0.5,event.thr=0.9),ut=u_new)
  F1   <- utilOB(preds,trues,ph,ls,util.control(umetric="Fm",beta=1,event.thr=0.9),ut=u_new)
  F2   <- utilOB(preds,trues,ph,ls,util.control(umetric="Fm",beta=2,event.thr=0.9),ut=u_new)
  
  mad=mean(abs(trues-preds))
  mse=mean((trues-preds)^2)
  mape= mean((abs(trues-preds)/trues))*100
  rmse= sqrt(mean((trues-preds)^2))
  mae_phi= mean(phi(trues,control.parms=ph)*(abs(trues-preds)))
  mape_phi= mean(phi(trues,control.parms=ph)*(abs(trues-preds)/trues))*100
  mse_phi= mean(phi(trues,control.parms=ph)*(trues-preds)^2)
  rmse_phi= sqrt(mean(phi(trues,control.parms=ph)*(trues-preds)^2))
  prec=prec
  rec=rec
  F05=F05
  F1=F1
  F2=F2
  
  c(
    rmse=rmse, rmse_phi=rmse_phi, prec=prec,rec=rec,F1=F1
  )
  
}

#UTILITY SURFACES
source("R/UtilitySurfaces.R")

getEntities <- function(data) {
  
  ent <- list()
  
  for(i in 1:nrow(data)) {
    
    s_title <- as.String(news[news$IDLink==data[i,]$IDLink,]$Title)
    s_headline <- as.String(news[news$IDLink==data[i,]$IDLink,]$Headline)
    
    a2_title <- annotate(s_title, list(sent_token_annotator, word_token_annotator))
    a2_headline <- annotate(s_headline, list(sent_token_annotator, word_token_annotator))
    
    entities <- c()
    entities <- c(entities,s_title[person_annotator(s_title, a2_title)])
    entities <- c(entities,s_title[org_annotator(s_title, a2_title)])
    entities <- c(entities,s_title[loc_annotator(s_title, a2_title)])
    entities <- c(entities,s_headline[person_annotator(s_headline, a2_headline)])
    entities <- c(entities,s_headline[org_annotator(s_headline, a2_headline)])
    entities <- c(entities,s_headline[loc_annotator(s_headline, a2_headline)])
    
    entities <- entities[entities!=""]
    entities <- unique(entities)
    entities <- gsub("\"","",entities)
    
    ent[[i]] <- entities
    
  }
  
  ent
  
}

getSources <- function(data, news) {
  
  sources <- news[news$IDLink %in% data$IDLink,c("Source","TimesPublishedTwitter")]
  sources <- sources[sources$TimesPublishedTwitter>=0,]
  sources <- aggregate(sources,by=list(sources$Source),FUN=mean,na.rm=TRUE)
  sources$Source <- NULL
  colnames(sources) <- c("Source","Mean")
  
  sources
  
}

statsEntities <- function(entities,ent,news) {
  
  stats_entities <- data.frame(Entity=character(0),Average=numeric(0))
  
  for(i in 1:length(entities)) {
    
    e <- entities[i]
    items <- grep(e,ent,fixed=TRUE)
    tweets <- news[items,]$TimesPublishedTwitter
    tweets[tweets<0] <- 0
    stats_entities <- rbind(stats_entities,data.frame(Entity=e,Average=mean(tweets)))
    
  }
  
  stats_entities
  
}

bandari_train <- function(data,sources,n_entities,ent,stats_entities) {
  
  #CREATE DATA SET
  dataset <- data.frame(IDLink=numeric(0),Source=numeric(0),SentimentTitle=numeric(0),SentimentHeadline=numeric(0),
                        NEntities=numeric(0),Highest=numeric(0),Average=numeric(0),TimesPublishedTwitter=numeric(0))
  
  for(i in 1:nrow(data)) {
    
    id <- data[i,]$IDLink
    
    src <- sources[sources$Source %in% news[news$IDLink==id,]$Source,]$Mean
    if(is.null(src)) { src <- 0 }
    
    sent_title <- news[news$IDLink==id,]$SentimentTitle
    sent_headline <- news[news$IDLink==id,]$SentimentHeadline
    
    n_ent <- n_entities[i,]$Number
    
    entities_obj <- ent[[i]]
    high <- numeric(0)
    avg <- numeric(0)
    
    if(length(entities_obj)==0) {
      high <- 0
      avg <- 0
    } else {
      high <- max(stats_entities[stats_entities$Entity %in% entities_obj,]$Average)
      avg <- mean(stats_entities[stats_entities$Entity %in% entities_obj,]$Average)
    }
    
    dataset <- rbind(dataset,data.frame(IDLink=id,Source=src,SentimentTitle=sent_title,SentimentHeadline=sent_headline,
                                        NEntities=n_ent,Highest=high,Average=avg,TimesPublishedTwitter=news[news$IDLink==id,]$TimesPublishedTwitter))
    
  }
  
  dataset
  
}

bandari_test <- function(data,sources,stats_entities) {
  
  ent <- getEntities(data)
  entities <- unlist(ent)
  entities <- unique(entities)
  
  #NUMBER OF NAMED ENTITIES
  n_entities <- data.frame(IDLink=news[match(data$IDLink,news$IDLink),]$IDLink,Number=sapply(ent, length))
  
  #CREATE DATA SET
  dataset <- data.frame(IDLink=numeric(0),Source=numeric(0),SentimentTitle=numeric(0),SentimentHeadline=numeric(0),
                        NEntities=numeric(0),Highest=numeric(0),Average=numeric(0),TimesPublishedTwitter=numeric(0))
  
  for(i in 1:nrow(data)) {
    
    id <- data[i,]$IDLink
    
    src <- sources[sources$Source %in% news[news$IDLink==id,]$Source,]$Mean
    if(is.null(src)) { src <- 0 }
    if(length(src)==0) { src <- 0 }
    
    sent_title <- news[news$IDLink==id,]$SentimentTitle
    sent_headline <- news[news$IDLink==id,]$SentimentHeadline
    
    n_ent <- n_entities[i,]$Number
    
    entities_obj <- ent[[i]]
    high <- numeric(0)
    avg <- numeric(0)
    
    if(length(entities_obj)==0) {
      high <- 0
      avg <- 0
    } else {
      if(nrow(stats_entities[stats_entities$Entity %in% entities_obj,])==0) {
        high <- 0
        avg <- 0
      } else {
        high <- max(stats_entities[stats_entities$Entity %in% entities_obj,]$Average)
        avg <- mean(stats_entities[stats_entities$Entity %in% entities_obj,]$Average)
      }
    }
    
    dataset <- rbind(dataset,data.frame(IDLink=id,Source=src,SentimentTitle=sent_title,SentimentHeadline=sent_headline,
                                        NEntities=n_ent,Highest=high,Average=avg,TimesPublishedTwitter=news[news$IDLink==id,]$TimesPublishedTwitter))
    
  }
  
  dataset
  
}


mc.bandari <- function(form,train,test,...) {
  
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  ent <- getEntities(train)
  entities <- unlist(ent)
  entities <- unique(entities)
  n_entities <- data.frame(IDLink=news[match(train$IDLink,news$IDLink),]$IDLink,Number=sapply(ent, length))
  sources <- getSources(train, news)
  stats_entities <- statsEntities(entities,ent,news)
  new_train <- bandari_train(train,sources,n_entities,ent,stats_entities)
  new_test <- bandari_test(test,sources,stats_entities)
  new_train$IDLink <- NULL
  new_test$IDLink <- NULL
  
  m <- svm(form,new_train,...)
  
  p <- predict(m,new_test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  names(p) <- rownames(test)
  res <- list(trues=responseValues(form,test),preds=p)
  res
  
}

#PREDICTION MODEL WORKFLOWS
mc.svm <- function(form,train,test,cost,gamma,...) {
  
  require(e1071)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- svm(form,train,cost=cost,gamma=gamma,...)
  
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.svm_SMOTE <- function(form,train,test,cost,gamma,un,ov,...) {
  
  require(e1071)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  #train <- SmoteRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- SmoteRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(un,ov))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- svm(form,train,cost=cost,gamma=gamma,...)
  
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.svm_UNDER <- function(form,train,test,un,cost,gamma,...) {
  
  require(e1071)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  #train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05))
  train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(un))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- svm(form,train,cost=cost,gamma=gamma,...)
  
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.svm_OVER <- function(form,train,test,cost,gamma,ov,...) {
  
  require(e1071)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  #train <- RandOverRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(1.1))
  train <- RandOverRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(ov))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- svm(form,train,cost=cost,gamma=gamma,...)
  
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.svm_IS <- function(form,train,test,cost,gamma,un,ov,...) {
  
  require(e1071)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  #train <- ImpSampRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- ImpSampRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(un,ov))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- svm(form,train,cost=cost,gamma=gamma,...)
  
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.mars <- function(form,train,test,nk,degree,thresh,...) {
  
  require(earth)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- earth(form,train,nk=nk,degree=degree,thresh=thresh)
  
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.mars_SMOTE <- function(form,train,test,nk,degree,thresh,un,ov,...) {
  
  require(earth)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  #train <- SmoteRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- SmoteRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(un,ov))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- earth(form,train,nk=nk,degree=degree,thresh=thresh,...)
  
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.mars_UNDER <- function(form,train,test,nk,degree,thresh,un,...) {
  
  require(earth)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  #train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05))
  train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(un))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- earth(form,train,nk=nk,degree=degree,thresh=thresh,...)
  
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.mars_OVER <- function(form,train,test,nk,degree,thresh,ov,...) {
  
  require(earth)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  #train <- RandOverRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(1.1))
  train <- RandOverRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(ov))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- earth(form,train,nk=nk,degree=degree,thresh=thresh,...)
  
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.mars_IS <- function(form,train,test,nk,degree,thresh,un,ov,...) {
  
  require(earth)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  train <- ImpSampRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(un,ov))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL  
  
  m <- earth(form,train,nk=nk,degree=degree,thresh=thresh,...)
  
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.rf <- function(form,train,test,mtry,ntree,...) {
  
  require(randomForest)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  
  train$IDLink <- NULL
  test$IDLink <- NULL 
  
  m <- randomForest(form,train,mtry=mtry,ntree=ntree,...)
  
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.rf_SMOTE <- function(form,train,test,mtry,ntree,un,ov,...) {
  
  require(randomForest)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  train <- SmoteRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(un,ov))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- randomForest(form,train,mtry=mtry,ntree=ntree,...)
  
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.rf_UNDER <- function(form,train,test,mtry,ntree,un,...) {
  
  require(randomForest)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(un))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- randomForest(form,train,mtry=mtry,ntree=ntree,...)
  
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.rf_OVER <- function(form,train,test,mtry,ntree,ov,...) {
  
  require(randomForest)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  train <- RandOverRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(ov))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- randomForest(form,train,mtry=mtry,ntree=ntree,...)
  
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

mc.rf_IS <- function(form,train,test,mtry,ntree,un,ov,...) {
  
  require(randomForest)
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  sources <- getSources(train, news)
  
  src <- sources[match(news[match(train$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  train["SourceRes"] <- src
  
  src <- sources[match(news[match(test$IDLink,news$IDLink),]$Source,sources$Source),]$Mean
  src[is.na(src)] <- 0
  src[is.null(src)] <- 0
  test["SourceRes"] <- src
  
  train <- ImpSampRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(un,ov))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- randomForest(form,train,mtry=mtry,ntree=ntree,...)
  
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  res <- list(trues=responseValues(form,test),preds=p)
  res
}

myWF.time <- function(form,train,test,nws,...) {
  
  ###DATA
  
  #TRAIN
  train.ids <- unique(unlist(train[,2:101]))
  news.trunc <- nws[nws$IDLink %in% train.ids,]
  news.trunc <- news.trunc[news.trunc$PublishDate<=(as.POSIXct(test[1,]$Timestamp)-(2*24*60*60)),]
  train.trunc <- news.trunc[news.trunc$PublishDate>=as.POSIXct(train[1,]$Timestamp),]
  train.trunc.ids <- unique(train.trunc$IDLink)
  train.pred.ids <- train.ids[!(train.ids %in% train.trunc.ids)]
  print(paste0("train ",nrow(train.trunc)))
  
  #print(paste0(length(train.ids)," ",length(train.trunc.ids)," - ",length(train.pred.ids)))
  
  #TEST
  test.ids <- unique(unlist(test[,2:101]))
  test.ids <- test.ids[!(test.ids %in% train.trunc.ids)]
  test.ids <- c(test.ids,train.pred.ids)
  test.trunc <- nws[nws$IDLink %in% test.ids,]
  print(paste0("test ",nrow(test.trunc)))
  
  querytimes <- as.POSIXct(test$Timestamp)  
  
  stats <- data.frame(PRED_AP=numeric(0),GOOGLE_AP=numeric(0),PRED_RP=numeric(0),GOOGLE_RP=numeric(0),PRED_RR=numeric(0),GOOGLE_RR=numeric(0),PRED_NDCG=numeric(0),GOOGLE_NDCG=numeric(0))
  
  for(i in 1:nrow(test)) {
    
    print(paste0(i," - ",nrow(test)))
    
    #GOOGLE RANK
    testrank <- as.numeric(test[i,2:101,])
    google.df <- data.frame(IDLink=testrank,Rank=seq(1,length(testrank),by=1))
    
    #TWITTER RANK
    twitter.df <- data.frame(IDLink=testrank,NTweets=nws[match(testrank,nws$IDLink),]$TimesPublishedTwitter)
    twitter.df <- twitter.df[with(twitter.df,order(twitter.df$NTweets,decreasing=TRUE)),]
    twitter.df["PublishDate"] <- nws[match(twitter.df$IDLink,nws$IDLink),]$PublishDate
    twitter.df["TwitterRank"] <- seq(1,length(testrank),by=1)
    
    #PREDICTION RANK
    pred.df <- data.frame(IDLink=testrank,PublishDate=nws[match(testrank,nws$IDLink),]$PublishDate)
    pred.df <- pred.df[with(pred.df,order(pred.df$PublishDate,decreasing=TRUE)),]
    pred.df["Rank"] <- seq(1,nrow(pred.df),by=1)
    
    if(nrow(pred.df)>100) {
      pred.df <- pred.df[1:100,]
    }
    
    #FINAL RANK TABLE
    finalrank <- twitter.df
    finalrank["GoogleRank"] <- google.df[match(finalrank$IDLink,google.df$IDLink),]$Rank
    finalrank["PredictionRank"] <- pred.df[match(finalrank$IDLink,pred.df$IDLink),]$Rank
    
    if(any(is.na(finalrank$GoogleRank))) { finalrank[is.na(finalrank$GoogleRank),]$GoogleRank <- 101 }
    if(any(is.na(finalrank$PredictionRank))) { finalrank[is.na(finalrank$PredictionRank),]$PredictionRank <- 101 }
    
    google_ap <- AP(finalrank$GoogleRank,10)
    prediction_ap <- AP(finalrank$PredictionRank,10)
    google_rp <- RPREC(finalrank$GoogleRank,10)
    prediction_rp <- RPREC(finalrank$PredictionRank,10)
    google_rr <- RR(finalrank$GoogleRank,10)
    prediction_rr <- RR(finalrank$PredictionRank,10)
    google_ndcg <- NDCGatK(finalrank$GoogleRank,10)
    prediction_ndcg <- NDCGatK(finalrank$PredictionRank,10)
    
    row <- data.frame(PRED_AP=prediction_ap,GOOGLE_AP=google_ap,PRED_RP=prediction_rp,GOOGLE_RP=google_rp,PRED_RR=prediction_rr,GOOGLE_RR=google_rr,PRED_NDCG=prediction_ndcg,GOOGLE_NDCG=google_ndcg)
    stats <- rbind(stats,row)
    
  }
  
  #INFORMATION RETURN
  trues <- responseValues(form,test)
  preds <- sample(levels(trues),nrow(test),replace=TRUE)
  #res <- WFoutput(rownames(test),trues,preds)
  #workflowInformation(res) <- list(eval_3=stats,predictions=predictions)
  res <- list(eval_3=stats)
  res
  
}

myWF.source <- function(form,train,test,nws,...) {
  
  ###DATA
  
  #TRAIN
  train.ids <- unique(unlist(train[,2:101]))
  news.trunc <- nws[nws$IDLink %in% train.ids,]
  news.trunc <- news.trunc[news.trunc$PublishDate<=(as.POSIXct(test[1,]$Timestamp)-(2*24*60*60)),]
  train.trunc <- news.trunc[news.trunc$PublishDate>=as.POSIXct(train[1,]$Timestamp),]
  train.trunc.ids <- unique(train.trunc$IDLink)
  train.pred.ids <- train.ids[!(train.ids %in% train.trunc.ids)]
  print(paste0("train ",nrow(train.trunc)))
  
  #print(paste0(length(train.ids)," ",length(train.trunc.ids)," - ",length(train.pred.ids)))
  
  #TEST
  test.ids <- unique(unlist(test[,2:101]))
  test.ids <- test.ids[!(test.ids %in% train.trunc.ids)]
  test.ids <- c(test.ids,train.pred.ids)
  test.trunc <- nws[nws$IDLink %in% test.ids,]
  print(paste0("test ",nrow(test.trunc)))
  
  sources <- getSources(train.trunc,nws)
  
  test.trunc["SourceRes"] <- sources[match(nws[match(test.trunc$IDLink,nws$IDLink),]$Source,sources$Source),]$Mean
  test.trunc[is.na(test.trunc$SourceRes),]$SourceRes <- 0
  
  querytimes <- as.POSIXct(test$Timestamp)  
  
  stats <- data.frame(PRED_AP=numeric(0),GOOGLE_AP=numeric(0),PRED_RP=numeric(0),GOOGLE_RP=numeric(0),PRED_RR=numeric(0),GOOGLE_RR=numeric(0),PRED_NDCG=numeric(0),GOOGLE_NDCG=numeric(0))
  
  for(i in 1:nrow(test)) {
    
    print(paste0(i," - ",nrow(test)))
    
    #GOOGLE RANK
    testrank <- as.numeric(test[i,2:101,])
    google.df <- data.frame(IDLink=testrank,Rank=seq(1,length(testrank),by=1))
    
    #TWITTER RANK
    twitter.df <- data.frame(IDLink=testrank,NTweets=nws[match(testrank,nws$IDLink),]$TimesPublishedTwitter)
    twitter.df <- twitter.df[with(twitter.df,order(twitter.df$NTweets,decreasing=TRUE)),]
    twitter.df["PublishDate"] <- nws[match(twitter.df$IDLink,nws$IDLink),]$PublishDate
    twitter.df["TwitterRank"] <- seq(1,length(testrank),by=1)
    
    #PREDICTION RANK
    pred.df <- data.frame(IDLink=testrank,PublishDate=nws[match(testrank,nws$IDLink),]$PublishDate,SourceRes=test.trunc[match(testrank,test.trunc$IDLink),]$SourceRes)
    if(any(is.na(pred.df$SourceRes))) { pred.df[is.na(pred.df$SourceRes),]$SourceRes <- 0 }
    pred.df["Rank"] <- rank(-pred.df$SourceRes,ties.method="random")
    pred.df <- pred.df[with(pred.df,order(pred.df$Rank)),]
    
    if(nrow(pred.df)>100) {
      pred.df <- pred.df[1:100,]
    }
    
    #FINAL RANK TABLE
    finalrank <- twitter.df
    finalrank["GoogleRank"] <- google.df[match(finalrank$IDLink,google.df$IDLink),]$Rank
    finalrank["PredictionRank"] <- pred.df[match(finalrank$IDLink,pred.df$IDLink),]$Rank
    
    if(any(is.na(finalrank$GoogleRank))) { finalrank[is.na(finalrank$GoogleRank),]$GoogleRank <- 101 }
    if(any(is.na(finalrank$PredictionRank))) { finalrank[is.na(finalrank$PredictionRank),]$PredictionRank <- 101 }
    
    google_ap <- AP(finalrank$GoogleRank,10)
    prediction_ap <- AP(finalrank$PredictionRank,10)
    google_rp <- RPREC(finalrank$GoogleRank,10)
    prediction_rp <- RPREC(finalrank$PredictionRank,10)
    google_rr <- RR(finalrank$GoogleRank,10)
    prediction_rr <- RR(finalrank$PredictionRank,10)
    google_ndcg <- NDCGatK(finalrank$GoogleRank,10)
    prediction_ndcg <- NDCGatK(finalrank$PredictionRank,10)
    
    row <- data.frame(PRED_AP=prediction_ap,GOOGLE_AP=google_ap,PRED_RP=prediction_rp,GOOGLE_RP=google_rp,PRED_RR=prediction_rr,GOOGLE_RR=google_rr,PRED_NDCG=prediction_ndcg,GOOGLE_NDCG=google_ndcg)
    stats <- rbind(stats,row)
    
  }
  
  #INFORMATION RETURN
  trues <- responseValues(form,test)
  preds <- sample(levels(trues),nrow(test),replace=TRUE)
  #res <- WFoutput(rownames(test),trues,preds)
  #workflowInformation(res) <- list(eval_3=stats,predictions=predictions)
  res <- list(eval_3=stats)
  res
  
}

myWF <- function(form,train,test,nws,frm,trainer,...) {
  
  ###DATA
  
  #TRAIN
  train.ids <- unique(unlist(train[,2:101]))
  news.trunc <- nws[nws$IDLink %in% train.ids,]
  news.trunc <- news.trunc[news.trunc$PublishDate<=(as.POSIXct(test[1,]$Timestamp)-(2*24*60*60)),]
  train.trunc <- news.trunc[news.trunc$PublishDate>=as.POSIXct(train[1,]$Timestamp),]
  train.trunc.ids <- unique(train.trunc$IDLink)
  train.pred.ids <- train.ids[!(train.ids %in% train.trunc.ids)]
  print(paste0("train ",nrow(train.trunc)))
  
  #print(paste0(length(train.ids)," ",length(train.trunc.ids)," - ",length(train.pred.ids)))
  
  #TEST
  test.ids <- unique(unlist(test[,2:101]))
  test.ids <- test.ids[!(test.ids %in% train.trunc.ids)]
  test.ids <- c(test.ids,train.pred.ids)
  test.trunc <- nws[nws$IDLink %in% test.ids,]
  print(paste0("test ",nrow(test.trunc)))
  
  
  ###PREDICTION MODELS
  frm <- frm[frm$TimesPublishedTwitter>0,]
  tr <- frm[frm$IDLink %in% train.trunc$IDLink,]
  ts <- frm[frm$IDLink %in% test.trunc$IDLink,]
  ts.ids <- ts$IDLink
  tr$PublishDate <- NULL
  ts$PublishDate <- NULL
  
  wf <- Workflow(trainer)
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- ts.ids
  
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  querytimes <- as.POSIXct(test$Timestamp)  
  
  stats <- data.frame(PRED_AP=numeric(0),GOOGLE_AP=numeric(0),PRED_RP=numeric(0),GOOGLE_RP=numeric(0),PRED_RR=numeric(0),GOOGLE_RR=numeric(0),PRED_NDCG=numeric(0),GOOGLE_NDCG=numeric(0))
  
  for(i in 1:nrow(test)) {
    
    print(paste0(i," - ",nrow(test)))
    
    #GOOGLE RANK
    testrank <- as.numeric(test[i,2:101,])
    google.df <- data.frame(IDLink=testrank,Rank=seq(1,length(testrank),by=1))
    
    #TWITTER RANK
    twitter.df <- data.frame(IDLink=testrank,NTweets=nws[match(testrank,nws$IDLink),]$TimesPublishedTwitter)
    twitter.df <- twitter.df[with(twitter.df,order(twitter.df$NTweets,decreasing=TRUE)),]
    twitter.df["PublishDate"] <- nws[match(twitter.df$IDLink,nws$IDLink),]$PublishDate
    twitter.df["TwitterRank"] <- seq(1,length(testrank),by=1)
    
    #PREDICTION RANK
    pred.df <- data.frame(IDLink=testrank,PublishDate=nws[match(testrank,nws$IDLink),]$PublishDate)
    news.old <- pred.df[pred.df$PublishDate<=(as.POSIXct(querytimes[i])-(2*24*60*60)),]
    news.new <- pred.df[pred.df$PublishDate>(as.POSIXct(querytimes[i])-(2*24*60*60)),]
    
    news.old["NTweets"] <- nws[match(news.old$IDLink,nws$IDLink),]$TimesPublishedTwitter
    #news.old["NTweets"] <- 0
    news.new["NTweets"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    print(nrow(news.old)/nrow(pred.df))
    
    pred.df <- rbind(news.old,news.new)
    pred.df["Rank"] <- rank(-pred.df$NTweets,ties.method="random")
    pred.df <- pred.df[with(pred.df,order(pred.df$Rank)),]
    
    if(nrow(pred.df)>100) {
      pred.df <- pred.df[1:100,]
    }
    
    #FINAL RANK TABLE
    finalrank <- twitter.df
    finalrank["GoogleRank"] <- google.df[match(finalrank$IDLink,google.df$IDLink),]$Rank
    finalrank["PredictionRank"] <- pred.df[match(finalrank$IDLink,pred.df$IDLink),]$Rank
    
    if(any(is.na(finalrank$GoogleRank))) { finalrank[is.na(finalrank$GoogleRank),]$GoogleRank <- 101 }
    if(any(is.na(finalrank$PredictionRank))) { finalrank[is.na(finalrank$PredictionRank),]$PredictionRank <- 101 }
    
    google_ap <- AP(finalrank$GoogleRank,10)
    prediction_ap <- AP(finalrank$PredictionRank,10)
    google_rp <- RPREC(finalrank$GoogleRank,10)
    prediction_rp <- RPREC(finalrank$PredictionRank,10)
    google_rr <- RR(finalrank$GoogleRank,10)
    prediction_rr <- RR(finalrank$PredictionRank,10)
    google_ndcg <- NDCGatK(finalrank$GoogleRank,10)
    prediction_ndcg <- NDCGatK(finalrank$PredictionRank,10)
    
    row <- data.frame(PRED_AP=prediction_ap,GOOGLE_AP=google_ap,PRED_RP=prediction_rp,GOOGLE_RP=google_rp,PRED_RR=prediction_rr,GOOGLE_RR=google_rr,PRED_NDCG=prediction_ndcg,GOOGLE_NDCG=google_ndcg)
    stats <- rbind(stats,row)
    
  }
  
  #INFORMATION RETURN
  trues <- responseValues(form,test)
  preds <- sample(levels(trues),nrow(test),replace=TRUE)
  #res <- WFoutput(rownames(test),trues,preds)
  #workflowInformation(res) <- list(eval_3=stats,predictions=predictions)
  res <- list(eval_3=stats,predictions=predictions)
  res
  
  
}

source("R/EvalProcedures.R")

source("R/WfsExp3.R")

# [EXAMPLES] PERFORMANCE ESTIMATION EXPERIMENTS (USE OPTIMAL PARAMETRIZATION FROM Exps1.R)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("myWF.bandari", nws=news, frm=frame_train.headline.obama),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             c(Workflow("myWF.time", nws=news),
                               Workflow("myWF.source", nws=news)),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)

###

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             c(Workflow("myWF.svm", nws=news, frm=frame_train.headline.microsoft,cost=150,gamma=0.001),
                               Workflow("myWF.svm_UNDER", nws=news, frm=frame_train.headline.microsoft,cost=10,gamma=0.001,un=0.1),
                               Workflow("myWF.svm_OVER", nws=news, frm=frame_train.headline.microsoft,cost=10,gamma=0.001,ov=3),
                               Workflow("myWF.svm_SMOTE", nws=news, frm=frame_train.headline.obama,cost=300,gamma=0.01,un=0.6,ov=2),
                               Workflow("myWF.svm_IS", nws=news, frm=frame_train.headline.microsoft,cost=10,gamma=0.001,un=0.05,ov=5)),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             c(Workflow("myWF.mars", nws=news, frm=frame_train.headline.obama,nk=10,degree=2,thresh=0.01),
                               Workflow("myWF.mars_UNDER", nws=news, frm=frame_train.headline.obama,nk=10,degree=1,thresh=0.01,un=0.05),
                               Workflow("myWF.mars_OVER", nws=news, frm=frame_train.headline.obama,nk=10,degree=1,thresh=0.01,ov=10),
                               Workflow("myWF.mars_SMOTE", nws=news, frm=frame_train.headline.obama,nk=10,degree=1,thresh=0.01,un=0.05,ov=2),
                               Workflow("myWF.mars_IS", nws=news, frm=frame_train.headline.obama,nk=10,degree=1,thresh=0.01,un=0.1,ov=3)),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             c(Workflow("myWF.rf", nws=news, frm=frame_train.headline.obama,mtry=5,ntree=750),
                               Workflow("myWF.rf_UNDER", nws=news, frm=frame_train.headline.obama,mtry=5,ntree=750,un=0.05),
                               Workflow("myWF.rf_OVER", nws=news, frm=frame_train.headline.obama,mtry=5,ntree=500,ov=10),
                               Workflow("myWF.rf_SMOTE", nws=news, frm=frame_train.headline.obama,mtry=5,ntree=1500,un=0.05,ov=10),
                               Workflow("myWF.rf_IS", nws=news, frm=frame_train.headline.obama,mtry=5,ntree=1500,un=0.05,ov=10)),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)
