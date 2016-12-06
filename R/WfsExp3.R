#WFsExp3

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

myWF.bandari <- function(form,train,test,nws,frm,...) {
  
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
  
  wf <- Workflow("mc.bandari")
  
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

myWF.svm <- function(form,train,test,nws,frm,cost,gamma,...) {
  
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
  
  wf <- Workflow("mc.svm")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, cost=cost, gamma=gamma)
  
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

myWF.svm_UNDER <- function(form,train,test,nws,frm,cost,gamma,un,...) {
  
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
  
  wf <- Workflow("mc.svm_UNDER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, cost=cost, gamma=gamma, un=un)
  
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

myWF.svm_OVER <- function(form,train,test,nws,frm,cost,gamma,ov,...) {
  
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
  
  wf <- Workflow("mc.svm_OVER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, cost=cost, gamma=gamma, ov=ov)
  
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

myWF.svm_SMOTE <- function(form,train,test,nws,frm,cost,gamma,un,ov,...) {
  
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
  
  wf <- Workflow("mc.svm_SMOTE")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, cost=cost, gamma=gamma, un=un, ov=ov)
  
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

myWF.svm_IS <- function(form,train,test,nws,frm,cost,gamma,un,ov,...) {
  
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
  
  wf <- Workflow("mc.svm_IS")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, cost=cost, gamma=gamma, un=un, ov=ov)
  
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

myWF.mars <- function(form,train,test,nws,frm,nk,degree,thresh,...) {
  
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
  
  wf <- Workflow("mc.mars")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, nk=nk, degree=degree, thresh=thresh)
  
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

myWF.mars_UNDER <- function(form,train,test,nws,frm,nk,degree,thresh,un,...) {
  
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
  
  wf <- Workflow("mc.mars_UNDER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, nk=nk, degree=degree, thresh=thresh, un=un)
  
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

myWF.mars_OVER <- function(form,train,test,nws,frm,nk,degree,thresh,ov,...) {
  
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
  
  wf <- Workflow("mc.mars_OVER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, nk=nk, degree=degree, thresh=thresh, ov=ov)
  
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

myWF.mars_SMOTE <- function(form,train,test,nws,frm,nk,degree,thresh,un,ov,...) {
  
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
  
  wf <- Workflow("mc.mars_SMOTE")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, nk=nk, degree=degree, thresh=thresh, un=un, ov=ov)
  
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

myWF.mars_IS <- function(form,train,test,nws,frm,nk,degree,thresh,un,ov,...) {
  
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
  
  wf <- Workflow("mc.mars_IS")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, nk=nk, degree=degree, thresh=thresh, un=un, ov=ov)
  
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

myWF.rf <- function(form,train,test,nws,frm,mtry,ntree,...) {
  
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
  
  wf <- Workflow("mc.rf")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, mtry=mtry, ntree=ntree)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- ts.ids
  
  
  #rf SNIPPET
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

myWF.rf_UNDER <- function(form,train,test,nws,frm,mtry,ntree,un,...) {
  
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
  
  wf <- Workflow("mc.rf_UNDER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, mtry=mtry, ntree=ntree, un=un)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- ts.ids
  
  
  #rf SNIPPET
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

myWF.rf_OVER <- function(form,train,test,nws,frm,mtry,ntree,ov,...) {
  
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
  
  wf <- Workflow("mc.rf_OVER")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, mtry=mtry, ntree=ntree, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- ts.ids
  
  
  #rf SNIPPET
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

myWF.rf_SMOTE <- function(form,train,test,nws,frm,mtry,ntree,un,ov,...) {
  
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
  
  wf <- Workflow("mc.rf_SMOTE")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, mtry=mtry, ntree=ntree, un=un, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- ts.ids
  
  
  #rf SNIPPET
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

myWF.rf_IS <- function(form,train,test,nws,frm,mtry,ntree,un,ov,...) {
  
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
  
  wf <- Workflow("mc.rf_IS")
  
  #PREDICTIONS
  model <- runWorkflow(wf,TimesPublishedTwitter ~ ., tr, ts, mtry=mtry, ntree=ntree, un=un, ov=ov)
  
  predictions <- as.data.frame(model$preds)
  colnames(predictions) <- "preds"
  predictions["IDLink"] <- ts.ids
  
  
  #rf SNIPPET
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





