#WFsExp2

myWF.time <- function(form,train,test,nws,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  test["Day"] <- substr(test$PublishDate,1,10)
  days <- names(table(test$Day)>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% test[test$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,PublishDate=news.new$PublishDate)
    pred.df <- pred.df[with(pred.df,order(pred.df$PublishDate,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  res <- list(eval_2=stats)
  res
  
}

myWF.source <- function(form,train,test,nws,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  sources <- getSources(train,nws)
  
  test["SourceRes"] <- sources[match(nws[match(test$IDLink,nws$IDLink),]$Source,sources$Source),]$Mean
  test[is.na(test$SourceRes),]$SourceRes <- 0
  
  test["Day"] <- substr(test$PublishDate,1,10)
  days <- names(table(test$Day)>10)
  
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% test[test$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["SourceRes"] <- test[match(news.new$IDLink,test$IDLink),]$SourceRes
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,PublishDate=news.new$PublishDate,SourceRes=news.new$SourceRes)
    pred.df <- pred.df[with(pred.df,order(pred.df$SourceRes,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  res <- list(eval_2=stats)
  res
  
  
}

myWF.bandari <- function(form,train,test,nws,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.bandari")
  
  #PREDICTIONS
  model <- runWorkflow(wf, form, train, test)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

#SVM

myWF.svm <- function(form,train,test,nws,cost,gamma,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.svm")
  
  #PREDICTIONS
  model <- runWorkflow(wf, form, train, test, cost=cost, gamma=gamma)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.svm_UNDER <- function(form,train,test,nws,cost,gamma,un,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.svm_UNDER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, cost=cost, gamma=gamma, un=un)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.svm_OVER <- function(form,train,test,nws,cost,gamma,ov,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.svm_OVER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, cost=cost, gamma=gamma, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.svm_SMOTE <- function(form,train,test,nws,cost,gamma,un,ov,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.svm_SMOTE")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, cost=cost, gamma=gamma, un=un, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.svm_IS <- function(form,train,test,nws,cost,gamma,un,ov,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.svm_IS")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, cost=cost, gamma=gamma, un=un, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

#MARS

myWF.mars <- function(form,train,test,nws,nk,degree,thresh,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.mars")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, nk=nk, degree=degree, thresh=thresh)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.mars_UNDER <- function(form,train,test,nws,nk,degree,thresh,un,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.mars_UNDER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, nk=nk, degree=degree, thresh=thresh, un=un)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.mars_OVER <- function(form,train,test,nws,nk,degree,thresh,ov,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.mars_OVER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, nk=nk, degree=degree, thresh=thresh, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.mars_SMOTE <- function(form,train,test,nws,nk,degree,thresh,un,ov,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.mars_SMOTE")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, nk=nk, degree=degree, thresh=thresh, un=un, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.mars_IS <- function(form,train,test,nws,nk,degree,thresh,un,ov,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.mars_IS")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, nk=nk, degree=degree, thresh=thresh, un=un, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

#RF

myWF.rf <- function(form,train,test,nws,mtry,ntree,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.rf")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, mtry=mtry, ntree=ntree)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.rf_UNDER <- function(form,train,test,nws,mtry,ntree,un,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.rf_UNDER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, mtry=mtry, ntree=ntree, un=un)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.rf_OVER <- function(form,train,test,nws,mtry,ntree,ov,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.rf_OVER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, mtry=mtry, ntree=ntree, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.rf_SMOTE <- function(form,train,test,nws,mtry,ntree,un,ov,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.rf_SMOTE")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, mtry=mtry, ntree=ntree, un=un, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

myWF.rf_IS <- function(form,train,test,nws,mtry,ntree,un,ov,...) {
  
  ###DATA
  
  #TRAIN
  train <- train[as.POSIXct(train$PublishDate)<=(as.POSIXct(test[1,]$PublishDate)-(2*24*60*60)),]
  
  ###PREDICTION MODELS
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  
  wf <- Workflow("mc.rf_IS")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., train, test, mtry=mtry, ntree=ntree, un=un, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- test$IDLink
  
  #MARS SNIPPET
  if(ncol(predictions)==4) {
    predictions$predicted <- predictions$TimesPublishedTwitter
    predictions$TimesPublishedTwitter <- NULL
  }
  
  predictions["PublishDate"] <- nws[match(predictions$IDLink,nws$IDLink),]$PublishDate
  
  predictions["Day"] <- substr(predictions$PublishDate,1,10)
  days <- names(table(substr(predictions$PublishDate,1,10))>10)
  
  stats <- data.frame(data.frame(Day=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0)))
  
  #NEWS POOL
  news.pool <- nws[nws$IDLink %in% predictions[predictions$Day %in% days,]$IDLink,]
  news.pool.df <- data.frame(IDLink=news.pool$IDLink,NTweets=news.pool$TimesPublishedTwitter,PublishDate=news.pool$PublishDate,Day=substr(news.pool$PublishDate,1,10))
  
  #PREDICTION RANK
  for(i in 1:(length(days)-1)) {
    
    news.new <- news.pool.df[news.pool.df$Day %in% c(days[i],days[i+1]),]
    news.new["preds"] <- predictions[match(news.new$IDLink,predictions$IDLink),]$preds
    
    pred.df <- data.frame(IDLink=news.new$IDLink,NTweets=news.new$NTweets,Preds=news.new$preds)
    pred.df <- pred.df[with(pred.df,order(pred.df$Preds,decreasing=TRUE)),]
    
    size <- nrow(pred.df)
    
    if(size>0) {
      
      pred.df["PredictionRank"] <- seq(1,size,by=1)
      pred.df <- pred.df[with(pred.df,order(pred.df$NTweets,decreasing=TRUE)),]
      pred.df["Rank"] <- seq(1,size,by=1)
      
      if(any(is.na(pred.df$PredictionRank))) { ranktest[is.na(pred.df$PredictionRank),]$PredictionRank <- size }
      
      prediction_ap10 <- AP(pred.df$PredictionRank,10)
      prediction_rr10 <- RR(pred.df$PredictionRank,10)
      prediction_ndcg5 <- NDCGatK(pred.df$PredictionRank,5)
      prediction_ndcg10 <- NDCGatK(pred.df$PredictionRank,10)
      
      
      row <- data.frame(Day=days[i],AP10=prediction_ap10,RR=prediction_rr10,NDCG5=prediction_ndcg5,NDCG10=prediction_ndcg10)
      stats <- rbind(stats,row)
    } else {}
    
    
  }
  
  #INFORMATION RETURN
  res <- list(eval_2=stats)
  res
  
  
}

