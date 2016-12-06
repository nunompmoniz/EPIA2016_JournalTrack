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
frame_train.headline.microsoft <- read.csv("HeadlineMicrosoft.csv")
frame_train.headline.obama <- read.csv("HeadlineObama.csv")
frame_train.headline.palestine <- read.csv("HeadlinePalestine.csv")

frame_train.headline.economy <- frame_train.headline.economy[frame_train.headline.economy$TimesPublishedTwitter>0,]
frame_train.headline.microsoft <- frame_train.headline.microsoft[frame_train.headline.microsoft$TimesPublishedTwitter>0,]
frame_train.headline.obama <- frame_train.headline.obama[frame_train.headline.obama$TimesPublishedTwitter>0,]
frame_train.headline.palestine <- frame_train.headline.palestine[frame_train.headline.palestine$TimesPublishedTwitter>0,]


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

source("R/WfsExp2.R")

source("R/EvalProcedures.R")

# [EXAMPLES] PERFORMANCE ESTIMATION EXPERIMENTS (USE OPTIMAL PARAMETRIZATION FROM Exps1.R)
exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.palestine),
                             c(Workflow("myWF.time", nws=news, frm=frame_train.headline.palestine),
                               Workflow("myWF.source", nws=news, frm=frame_train.headline.palestine)),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)

exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.obama),
                             Workflow("myWF.bandari", nws=news),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)

exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ ., frame_train.headline.obama),
                             Workflow("myWF.svm_SMOTE", nws=news,cost=300,gamma=0.01,un=0.6,ov=2),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)

exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ ., frame_train.headline.economy),
                             Workflow("myWF.mars", nws=news, nk=10,degree=1,thresh=0.01),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)

exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ ., frame_train.headline.obama),
                             Workflow("myWF.rf_SMOTE", nws=news,mtry=5,ntree=1500,un=0.05,ov=10),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)
