#Working Directory
#setwd("/Users/admin/Desktop/Artigos/PhD/2015 - Machine Learning/TIST/Data/")
setwd("/home/nmoniz/TIST")
###LIBRARIES
#library(performanceEstimation,lib.loc="~/R/dev.libs/")
library(performanceEstimation)
library(lubridate)
library(e1071)
library(DMwR)
library(uba)
#library(uba,lib.loc="~/R/dev.libs/")
library(randomForest)
library(earth)
library(UBL)
library(qdap)
#library(openNLP)
#library(NLP)

#### RUN ONCE

# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# word_token_annotator <- Maxent_Word_Token_Annotator()
# 
# person_annotator <- Maxent_Entity_Annotator(kind="person",language="en",model='//Users/admin/Downloads/en-ner-person.bin')
# org_annotator <- Maxent_Entity_Annotator(kind="organization",language="en",model='//Users/admin/Downloads/en-ner-organization.bin')
# loc_annotator <- Maxent_Entity_Annotator(kind="location",language="en",model='//Users/admin/Downloads/en-ner-location.bin')

#person_annotator <- Maxent_Entity_Annotator(kind="person",language="en",model='/home/nmoniz/TIST/en-ner-person.bin')
#org_annotator <- Maxent_Entity_Annotator(kind="organization",language="en",model='/home/nmoniz/TIST/en-ner-organization.bin')
#loc_annotator <- Maxent_Entity_Annotator(kind="location",language="en",model='/home/nmoniz/TIST/en-ner-location.bin')

#LOAD DATA
#DATA LOADING
# news <- read.csv("Data/Links_clean.csv")
news <- read.csv("Links_clean.csv")
news$PublishDate <- as.POSIXct(news$PublishDate)

#LOAD NEWS
# frame_train.headline.economy <- read.csv("HeadlineEconomy.csv")
# frame_train.headline.economy$PublishDate <- NULL
# frame_train.headline.microsoft <- read.csv("HeadlineMicrosoft.csv")
# frame_train.headline.microsoft$PublishDate <- NULL
# frame_train.headline.obama <- read.csv("HeadlineObama.csv")
# frame_train.headline.obama$PublishDate <- NULL
frame_train.headline.palestine <- read.csv("HeadlinePalestine.csv")
frame_train.headline.palestine$PublishDate <- NULL

# frame_train.headline.economy <- frame_train.headline.economy[frame_train.headline.economy$TimesPublishedTwitter>0,]
# frame_train.headline.microsoft <- frame_train.headline.microsoft[frame_train.headline.microsoft$TimesPublishedTwitter>0,]
# frame_train.headline.obama <- frame_train.headline.obama[frame_train.headline.obama$TimesPublishedTwitter>0,]
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
    mse=mse, mse_phi=mse_phi, prec=prec,rec=rec,F1=F1
  )
  
}

#UTIL FOR ONE BUMP
utilOneBump <- function(form,train,test,preds,PHIs,cf,thr,makePlot) {
  
  require(akima)
  
  ph <- phi(resp(form,train),PHIs)
  ph.test <- phi(resp(form,test),PHIs)
  
  phi1 <- unique(resp(form,train)[which(ph==min(ph[ph>=thr]))])
  phi2 <- PHIs$control.pts[4]
  phi3 <- PHIs$control.pts[7]
  max.x <- max(preds)
  max.y <- max(resp(form,test))
  
  values <- sort(unique(resp(form,test)))
  values.df <- data.frame(trues=as.numeric(resp(form,test)),preds=as.numeric(preds))
  
  #############
  #FALSE POSITIVE
  x <- c(phi1,
         phi1,
         (phi1+phi1),
         max.y,
         max.y,
         phi1,
         (max.y/2),
         seq(max.y/2,max.y,by=1),
         seq(max.y/2,max.y,by=1))
  y <- c(phi1,
         0,
         phi1,
         phi1,
         0,
         (phi1/2),
         0,
         rep(phi1,length(seq(max.y/2,max.y,by=1))),
         rep(0,length(seq(max.y/2,max.y,by=1))))
  u <- c(thr,
         0, #or 0
         0,
         0,
         -1,
         0,
         -1,
         rep(0,length(seq(max.y/2,max.y,by=1))),
         rep(-1,length(seq(max.y/2,max.y,by=1))))
  
  if(max.x>max.y) {
    x <- c(x,max.x,max.x,(max.x/2),seq(max.y,max.x,by=1),seq(max.y,max.x,by=1))
    y <- c(y,phi1,0,0,rep(phi1,length(seq(max.y,max.x,by=1))),rep(0,length(seq(max.y,max.x,by=1))))
    u <- c(u,0,-1,-1,rep(0,length(seq(max.y,max.x,by=1))),rep(-1,length(seq(max.y,max.x,by=1))))
  }
  
  dat <- data.frame(x,y,u)
  
  xo.values <- unique(sort(values.df[values.df$preds>=phi1,]$preds))
  yo.values <- unique(sort(values.df[values.df$trues<=phi1,]$trues))
  if(length(xo.values)==0) { if(max.x>max.y) { xo.values <- c(seq(phi1,max.x,by=1)) } else { xo.values <- c(seq(phi1,max.y,by=1)) } }
  if(length(yo.values)==0) { yo.values <- c(seq(0,phi1,by=1)) }
  
  fp <- interp(x,y,u,duplicate="mean",xo=xo.values,yo=yo.values)
  
  #FALSE NEGATIVE
  x <- c(0,
         0,
         phi1,
         phi1,
         phi1,
         (phi1/2))
  y <- c(phi1,
         max.y,
         phi1,
         max.y,
         (phi1+phi1),
         phi1)
  u <- c(0, #or ZERO
         -1,
         thr,
         0,
         0,
         0)
  
  if(max.x>max.y) {
    x <- c(x,0,phi1)
    y <- c(y,max.x,max.x)
    u <- c(u,-1,0)
  }
  
  dat <- data.frame(x,y,u)
  
  xo.values <- unique(sort(values.df[values.df$preds<=phi1,]$preds))
  yo.values <- unique(sort(values.df[values.df$trues>=phi1,]$trues))
  if(length(xo.values)==0) { xo.values <- seq(0,phi1,by=1) }
  if(length(yo.values)==0) { if(max.x>max.y) { yo.values <- seq(phi1,max.x,by=1) } else { yo.values <- seq(phi1,max.y,by=1) } }
  
  fn <- interp(x,y,u,duplicate="mean",xo=xo.values,yo=yo.values)
  
  #TRUE NEGATIVE
  phi_aux <- data.frame(points=resp(form,test)[which(ph.test<=thr)], p=ph.test[ph.test<=thr])
  phi_aux <- phi_aux[with(phi_aux,order(phi_aux$points)),]
  phi_aux <- unique(phi_aux)
  
  x <- c(rep(phi1,length(fp$y)),
         0,
         phi_aux$points)
  
  y <- c(fp$y,
         phi1,
         phi_aux$points)
  
  u <- c(fp$z[1,],
         0,
         phi_aux$p)
  
  dat <- data.frame(x,y,u)
  
  xo.values <- unique(sort(values.df[values.df$preds<=phi1,]$preds))
  yo.values <- unique(sort(values.df[values.df$trues<=phi1,]$trues))
  if(length(xo.values)==0) { xo.values <- seq(0,phi1,by=1) }
  if(length(yo.values)==0) { yo.values <- seq(0,phi1,by=1) }
  
  tn <- interp(x,y,u,duplicate="mean",xo=xo.values,yo=yo.values)
  #tn$z[tn$z<0] <- 0
  
  #TRUE POSITIVE
  phi_aux <- data.frame(points=resp(form,test)[which(ph.test>=thr)], p=ph.test[ph.test>=thr])
  phi_aux <- phi_aux[with(phi_aux,order(phi_aux$points)),]
  phi_aux <- unique(phi_aux)
  
  x <- c(rep(phi1,length(fn$y)),
         phi_aux$points,
         seq(phi3,max.y,by=1),
         max.y,
         phi1,
         (max.y/2),
         seq(max.y/2,max.y,by=1),
         (phi1+phi1))
  
  y <- c(fn$y,
         phi_aux$points,
         seq(phi3,max.y,by=1),
         phi1,
         max.y,
         phi1,
         rep(phi1,length(seq(max.y/2,max.y,by=1))),
         phi1)
  
  u <- c(fn$z[nrow(fn$z),],
         phi_aux$p,
         rep(1,length(seq(phi3,max.y,by=1))),
         0,
         0,
         0,
         rep(0,length(seq(max.y/2,max.y,by=1))),
         0)
  
  if(max.x>max.y) {
    x <- c(x,max.x,max.x,phi1,seq(max.y,max.x,by=1),seq(max.y,max.x,by=1))
    y <- c(y,max.x,phi1,max.x,seq(max.y,max.x,by=1),rep(phi1,length(seq(max.y,max.x,by=1))))
    u <- c(u,1,0,0,rep(1,length(seq(max.y,max.x,by=1))),rep(0,length(seq(max.y,max.x,by=1))))
  }
  
  dat <- data.frame(x,y,u)
  
  xo.values <- unique(sort(values.df[values.df$preds>=phi1,]$preds))
  yo.values <- unique(sort(values.df[values.df$trues>=phi1,]$trues))
  if(length(xo.values)==0) { if(max.x>max.y) { xo.values <- seq(phi1,max.x,by=1) } else { xo.values <- seq(phi1,max.y,by=1) } }
  if(length(yo.values)==0) { if(max.x>max.y) { yo.values <- seq(phi1,max.x,by=1) } else { yo.values <- seq(phi1,max.y,by=1) } }
  
  tp <- interp(x,y,u,duplicate="mean",xo=xo.values,yo=yo.values)
  #tp$z[tp$z<0] <- 0
  
  if(any(is.na(tp$z[,1]))) { tp$z[,1] <- fp$z[,ncol(fp$z)] }
  
  ####
  
  tests <- fp
  tests$x <- c(tn$x,tp$x)
  tests$y <- c(tn$y,tp$y)
  tst.m <- cbind(tn$z,fn$z)
  tst.m2 <- cbind(fp$z,tp$z)
  tst <- rbind(tst.m,tst.m2)
  tests$z <- tst
  
  if(any(table(tests$x)>1)) { 
    #tests$x <- tests$x[-min(which(tests$x==phi1))]
    #tests$z <- tests$z[-min(which(tests$x==phi1)),]
    tests$z <- tests$z[!duplicated(tests$x),]
    tests$x <- unique(tests$x)
  }
  
  if(any(table(tests$y)>1)) { 
    #tests$y <- tests$y[-min(which(tests$y==phi1))]
    #tests$z <- tests$z[,-min(which(tests$y==phi1))]
    tests$z <- tests$z[,!duplicated(tests$y)]
    tests$y <- unique(tests$y)
  }
  
  if(makePlot) {
    image.plot(tests, xlab=expression("hat{y}"), ylab="y")
    contour(tests,add=TRUE,labcex=1)
    abline(h=PHIs$control.pts[7],lty=2)
    abline(v=PHIs$control.pts[7],lty=2)
  }
  
  #####
  #values.df$Utility <- NA
  values.df$Utility <- 0
  for(i in 1:nrow(values.df)) {
    values.df[i,]$Utility <- tests$z[which(tests$x==values.df[i,]$preds),which(tests$y==values.df[i,]$trues)]
  }
  
  if(any(is.na(values.df$Utility))) {
    print(values.df[is.na(values.df$Utility),])
  }
  
  values.df
  
}

getEntities <- function(data) {
  
  ent <- list()
  
  for(i in 1:nrow(data)) {
    
    #print(i)
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
    #print(entities)
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
    #print(i)
    e <- entities[i]
    items <- grep(e,ent,fixed=TRUE)
    tweets <- news[items,]$TimesPublishedTwitter
    tweets[tweets<0] <- 0
    stats_entities <- rbind(stats_entities,data.frame(Entity=e,Average=mean(tweets)))
  }
  
  stats_entities
  
}


#########

# bandari_train <- function(data,sources,n_entities,ent,stats_entities) {
#   
#   #CREATE DATA SET
#   dataset <- data.frame(IDLink=numeric(0),Source=numeric(0),SubjectivityTitle=numeric(0),SubjectivityHeadline=numeric(0),
#                         NEntities=numeric(0),Highest=numeric(0),Average=numeric(0),TimesPublishedTwitter=numeric(0))
#   
#   for(i in 1:nrow(data)) {
#     
#     print(paste0(i,"-",nrow(data)))
#     
#     id <- data[i,]$IDLink
#     
#     src <- sources[sources$Source %in% news[news$IDLink==id,]$Source,]$Mean
#     if(is.null(src)) { src <- 0 }
#     
#     poltitle <- polarity(news[news$IDLink==id,]$Title)
#     polheadline <- polarity(news[news$IDLink==id,]$Headline)
#     words_title <- c(poltitle$all$pos.words,poltitle$all$neg.words)
#     words_title <- words_title[words_title!="-"]
#     words_headline <- c(polheadline$all$pos.words,polheadline$all$neg.words)
#     words_headline <- words_headline[words_headline!="-"]
#     
#     subj_title <- length(words_title)/poltitle$all$wc
#     subj_headline <- length(words_headline)/polheadline$all$wc
#     
#     n_ent <- n_entities[i,]$Number
#     
#     entities_obj <- ent[[i]]
#     high <- numeric(0)
#     avg <- numeric(0)
#     
#     if(length(entities_obj)==0) {
#       high <- 0
#       avg <- 0
#     } else {
#       high <- max(stats_entities[stats_entities$Entity %in% entities_obj,]$Average)
#       avg <- mean(stats_entities[stats_entities$Entity %in% entities_obj,]$Average)
#     }
#     
#     dataset <- rbind(dataset,data.frame(IDLink=id,Source=src,SubjectivityTitle=subj_title,SubjectivityHeadline=subj_headline,
#                                         NEntities=n_ent,Highest=high,Average=avg,TimesPublishedTwitter=news[news$IDLink==id,]$TimesPublishedTwitter))
#     
#   }
#   
#   dataset
#   
# }
# 
# bandari_test <- function(data,sources,stats_entities) {
#   
#   ent <- getEntities(data)
#   entities <- unlist(ent)
#   entities <- unique(entities)
#   
#   #NUMBER OF NAMED ENTITIES
#   n_entities <- data.frame(IDLink=news[match(data$IDLink,news$IDLink),]$IDLink,Number=sapply(ent, length))
#   
#   #CREATE DATA SET
#   dataset <- data.frame(IDLink=numeric(0),Source=numeric(0),SubjectivityTitle=numeric(0),SubjectivityHeadline=numeric(0),
#                         NEntities=numeric(0),Highest=numeric(0),Average=numeric(0),TimesPublishedTwitter=numeric(0))
#   
#   for(i in 1:nrow(data)) {
#     
#     print(paste0(i,"-",nrow(data)))
#     
#     id <- data[i,]$IDLink
#     
#     src <- sources[sources$Source %in% news[news$IDLink==id,]$Source,]$Mean
#     if(is.null(src)) { src <- 0 }
#     if(length(src)==0) { src <- 0 }
#     
#     poltitle <- polarity(news[news$IDLink==id,]$Title)
#     polheadline <- polarity(news[news$IDLink==id,]$Headline)
#     words_title <- c(poltitle$all$pos.words,poltitle$all$neg.words)
#     words_title <- words_title[words_title!="-"]
#     words_headline <- c(polheadline$all$pos.words,polheadline$all$neg.words)
#     words_headline <- words_headline[words_headline!="-"]
#     
#     subj_title <- length(words_title)/poltitle$all$wc
#     subj_headline <- length(words_headline)/polheadline$all$wc
#     
#     n_ent <- n_entities[i,]$Number
#     
#     entities_obj <- ent[[i]]
#     high <- numeric(0)
#     avg <- numeric(0)
#     
#     if(length(entities_obj)==0) {
#       high <- 0
#       avg <- 0
#     } else {
#       if(nrow(stats_entities[stats_entities$Entity %in% entities_obj,])==0) {
#         high <- 0
#         avg <- 0
#       } else {
#         high <- max(stats_entities[stats_entities$Entity %in% entities_obj,]$Average)
#         avg <- mean(stats_entities[stats_entities$Entity %in% entities_obj,]$Average)
#       }
#     }
#     
#     dataset <- rbind(dataset,data.frame(IDLink=id,Source=src,SubjectivityTitle=subj_title,SubjectivityHeadline=subj_headline,
#                                         NEntities=n_ent,Highest=high,Average=avg,TimesPublishedTwitter=news[news$IDLink==id,]$TimesPublishedTwitter))
#     
#   }
#   
#   dataset
#   
# }


bandari_train <- function(data,sources,n_entities,ent,stats_entities) {
  
  #CREATE DATA SET
  dataset <- data.frame(IDLink=numeric(0),Source=numeric(0),SentimentTitle=numeric(0),SentimentHeadline=numeric(0),
                        NEntities=numeric(0),Highest=numeric(0),Average=numeric(0),TimesPublishedTwitter=numeric(0))
  
  for(i in 1:nrow(data)) {
    
    #print(i)
    
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
    
    #print(i)
    
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
  eval <- eval.stats("TimesPublishedTwitter ~ .",new_train,new_test,p,ph,ls)
  res <- list(trues=responseValues(form,new_test),preds=p,evaluation=eval)
  res
  
}

#PREDICTION MODEL WORKFLOWS
mc.lm <- function(form,train,test,...) {
  
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
  
  m <- lm(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.lm_SMOTE <- function(form,train,test,...) {
  
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
  
  train <- SmoteRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- lm(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.lm_UNDER <- function(form,train,test,...) {
  
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
  
  train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- lm(form,train)
  p <- predict(m,test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.lm_OVER <- function(form,train,test,...) {
  
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
  
  train <- RandOverRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- lm(form,train)
  p <- predict(m,test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.lm_GN <- function(form,train,test,...) {
  
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
  
  train <- GaussNoiseRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- lm(form,train)
  p <- predict(m,test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.lm_IS <- function(form,train,test,...) {
  
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
  
  train <- ImpSampRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- lm(form,train)
  p <- predict(m,test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.svm <- function(form,train,test,...) {
  
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
  
  m <- svm(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.svm_SMOTE <- function(form,train,test,...) {
  
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
  
  train <- SmoteRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- svm(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.svm_UNDER <- function(form,train,test,...) {
  
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
  
  train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- svm(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.svm_OVER <- function(form,train,test,...) {
  
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
  
  train <- RandOverRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- svm(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.svm_GN <- function(form,train,test,...) {
  
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
  
  train <- GaussNoiseRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- svm(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.svm_IS <- function(form,train,test,...) {
  
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
  
  train <- ImpSampRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- svm(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.mars <- function(form,train,test,...) {
  
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
  
  m <- earth(form,train)
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.mars_SMOTE <- function(form,train,test,...) {
  
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
  
  train <- SmoteRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- earth(form,train,...)
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.mars_UNDER <- function(form,train,test,...) {
  
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
  
  train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- earth(form,train,...)
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.mars_OVER <- function(form,train,test,...) {
  
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
  
  train <- RandOverRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- earth(form,train,...)
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.mars_GN <- function(form,train,test,...) {
  
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
  
  train <- GaussNoiseRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- earth(form,train)
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.mars_IS <- function(form,train,test,...) {
  
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
  
  train <- ImpSampRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- earth(form,train,...)
  p <- predict(m,test)
  p <- as.vector(p)
  names(p) <- rownames(test)
  
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.rf <- function(form,train,test,...) {
  
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
  
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.rf_SMOTE <- function(form,train,test,...) {
  
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
  
  train <- SmoteRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.rf_UNDER <- function(form,train,test,...) {
  
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
  
  train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.rf_OVER <- function(form,train,test,...) {
  
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
  
  train <- RandOverRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.rf_GN <- function(form,train,test,...) {
  
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
  
  train <- GaussNoiseRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.rf_IS <- function(form,train,test,...) {
  
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
  
  train <- ImpSampRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05,1.1))
  train <- train[,colSums(train)>0]
  test <- test[,colnames(train)]
  train$IDLink <- NULL
  test$IDLink <- NULL
  
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  if(length(p[p<1])>0) { p[p<1] <- min(p[p>=1]) }
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

# exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.economy),
#                              Workflow("mc.bandari"),
#                              EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
# )
# 
# exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.microsoft),
#                              Workflow("mc.bandari"),
#                              EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
# )
# 
# exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.obama),
#                              Workflow("mc.bandari"),
#                              EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
# )
# 
# exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.palestine),
#                              Workflow("mc.bandari"),
#                              EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
# )
# 
# 
# exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.economy),
#                              c(Workflow("mc.mars"),
#                                Workflow("mc.mars_SMOTE"),
#                                Workflow("mc.mars_UNDER"),
#                                Workflow("mc.mars_OVER"),
#                                #Workflow("mc.mars_GN"),
#                                Workflow("mc.mars_IS")),
#                              EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
# )
# 
# exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.microsoft),
#                              c(Workflow("mc.mars"),
#                                Workflow("mc.mars_SMOTE"),
#                                Workflow("mc.mars_UNDER"),
#                                Workflow("mc.mars_OVER"),
#                                #Workflow("mc.mars_GN"),
#                                Workflow("mc.mars_IS")),
#                              EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
# )
# 
# exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.obama),
#                              c(Workflow("mc.mars"),
#                                Workflow("mc.mars_SMOTE"),
#                                Workflow("mc.mars_UNDER"),
#                                Workflow("mc.mars_OVER"),
#                                #Workflow("mc.mars_GN"),
#                                Workflow("mc.mars_IS")),
#                              EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
# )
# 

exp <- performanceEstimation(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.palestine),
                             Workflow("mc.mars_GN"),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)
