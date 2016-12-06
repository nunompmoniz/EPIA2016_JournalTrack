###############
#EXP1
###############

#BANDARI
results <- data.frame(model=character(0),rmse=numeric(0),rrmse_phi=numeric(0),prec=numeric(0),rec=numeric(0),F1=numeric(0),Time=numeric(0))
models <- c("mc.bandari")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$evaluation
  traintime <- as.numeric(getIterationsInfo(exp,workflow=i,task=i,it=1)$traintime[1])
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$evaluation)
    traintime <- rbind(traintime,as.numeric(getIterationsInfo(exp,workflow=i,task=1,it=a)$traintime[1]))
  }
  results_row <- data.frame(model=models[i],rmse=r[,1],rrmse_phi=r[,2],prec=r[,3],rec=r[,4],F1=r[,5],Time=traintime)
  results <- rbind(results,results_row)
}

results

#SVM
results <- data.frame(model=character(0),rmse=numeric(0),rrmse_phi=numeric(0),prec=numeric(0),rec=numeric(0),F1=numeric(0),Time=numeric(0))
#models <- c("mc.svm","mc.svm_SMOTE","mc.svm_UNDER","mc.svm_OVER","mc.svm_GN","mc.svm_IS")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$evaluation
  traintime <- as.numeric(getIterationsInfo(exp,workflow=i,task=i,it=1)$traintime[1])
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$evaluation)
    traintime <- rbind(traintime,as.numeric(getIterationsInfo(exp,workflow=i,task=1,it=a)$traintime[1]))
  }
  results_row <- data.frame(model=models[i],rmse=r[,1],rrmse_phi=r[,2],prec=r[,3],rec=r[,4],F1=r[,5],Time=traintime)
  results <- rbind(results,results_row)
}

results

#MARS
results <- data.frame(model=character(0),rmse=numeric(0),rrmse_phi=numeric(0),prec=numeric(0),rec=numeric(0),F1=numeric(0),Time=numeric(0))
#models <- c("mc.mars","mc.mars_SMOTE","mc.mars_UNDER","mc.mars_OVER","mc.mars_IS")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$evaluation
  traintime <- as.numeric(getIterationsInfo(exp,workflow=i,task=i,it=1)$traintime[1])
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$evaluation)
    traintime <- rbind(traintime,as.numeric(getIterationsInfo(exp,workflow=i,task=1,it=a)$traintime[1]))
  }
  results_row <- data.frame(model=models[i],rmse=r[,1],rrmse_phi=r[,2],prec=r[,3],rec=r[,4],F1=r[,5],Time=traintime)
  results <- rbind(results,results_row)
}

results

#RF
results <- data.frame(model=character(0),rmse=numeric(0),rrmse_phi=numeric(0),prec=numeric(0),rec=numeric(0),F1=numeric(0),Time=numeric(0))
#models <- c("mc.rf","mc.rf_SMOTE","mc.rf_UNDER","mc.rf_OVER","mc.rf_GN","mc.rf_IS")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$evaluation
  traintime <- as.numeric(getIterationsInfo(exp,workflow=i,task=i,it=1)$traintime[1])
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$evaluation)
    traintime <- rbind(traintime,as.numeric(getIterationsInfo(exp,workflow=i,task=1,it=a)$traintime[1]))
  }
  results_row <- data.frame(model=models[i],rmse=r[,1],rrmse_phi=r[,2],prec=r[,3],rec=r[,4],F1=r[,5],Time=traintime)
  results <- rbind(results,results_row)
}

results

##################
#EXP2
##################

aggregate(stats,by=list(stats$model),FUN=mean,na.rm=TRUE)

#BANDARI
stats <- data.frame(model=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0))
models <- c("mc.bandari")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$eval_2
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$eval_2)
  }
  
  stats_aux <- data.frame(model=models[i],AP10=r[,2],RR=r[,3],NDCG5=r[,4],NDCG10=r[,5])
  
  stats <- rbind(stats,stats_aux)
  
}

stats

#BASELINES
stats <- data.frame(model=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0))
models <- c("mc.Time","mc.Source")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$eval_2
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$eval_2)
  }
  
  stats_aux <- data.frame(model=models[i],AP10=r[,2],RR=r[,3],NDCG5=r[,4],NDCG10=r[,5])
  
  stats <- rbind(stats,stats_aux)
  
}

stats

#SVM
stats <- data.frame(model=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0))
#models <- c("mc.svm_SMOTE","mc.svm_UNDER","mc.svm_OVER","mc.svm_GN","mc.svm_IS")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$eval_2
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$eval_2)
  }

  stats_aux <- data.frame(model=models[i],AP10=r[,2],RR=r[,3],NDCG5=r[,4],NDCG10=r[,5])

  stats <- rbind(stats,stats_aux)

}

stats

#MARS
stats <- data.frame(model=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0))
#models <- c("mc.mars_SMOTE","mc.mars_UNDER","mc.mars_OVER","mc.mars_IS")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$eval_2
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$eval_2)
  }
  
  stats_aux <- data.frame(model=models[i],AP10=r[,2],RR=r[,3],NDCG5=r[,4],NDCG10=r[,5])
  
  stats <- rbind(stats,stats_aux)
  
}

stats

#RF
stats <- data.frame(model=character(0),AP10=numeric(0),RR=numeric(0),NDCG5=numeric(0),NDCG10=numeric(0))
#models <- c("mc.rf_SMOTE","mc.rf_UNDER","mc.rf_OVER","mc.rf_GN","mc.rf_IS")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$eval_2
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$eval_2)
  }
  
  stats_aux <- data.frame(model=models[i],AP10=r[,2],RR=r[,3],NDCG5=r[,4],NDCG10=r[,5])
  
  stats <- rbind(stats,stats_aux)
  
}

stats



###############
#EXP3
###############

aggregate(stats,by=list(stats$model),FUN=mean,na.rm=TRUE)

#BANDARI
stats <- data.frame(model=character(0),PRED_AP=numeric(0),GOOGLE_AP=numeric(0),PRED_RP=numeric(0),GOOGLE_RP=numeric(0),PRED_RR=numeric(0),GOOGLE_RR=numeric(0),PRED_NDCG=numeric(0),GOOGLE_NDCG=numeric(0))
models <- c("mc.bandari")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$eval_3
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$eval_3)
  }
  
  stats_aux <- data.frame(model=models[i],PRED_AP=r[,1],GOOGLE_AP=r[,2],PRED_RP=r[,3],GOOGLE_RP=r[,4],PRED_RR=r[,5],GOOGLE_RR=r[,6],PRED_NDCG=r[,7],GOOGLE_NDCG=r[,8])
  
  
  stats <- rbind(stats,stats_aux)
  
}

stats

#BASELINE
stats <- data.frame(model=character(0),PRED_AP=numeric(0),GOOGLE_AP=numeric(0),PRED_RP=numeric(0),GOOGLE_RP=numeric(0),PRED_RR=numeric(0),GOOGLE_RR=numeric(0),PRED_NDCG=numeric(0),GOOGLE_NDCG=numeric(0))
models <- c("mc.Time","mc.Source")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$eval_3
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$eval_3)
  }
  
  stats_aux <- data.frame(model=models[i],PRED_AP=r[,1],GOOGLE_AP=r[,2],PRED_RP=r[,3],GOOGLE_RP=r[,4],PRED_RR=r[,5],GOOGLE_RR=r[,6],PRED_NDCG=r[,7],GOOGLE_NDCG=r[,8])
  
  
  stats <- rbind(stats,stats_aux)
  
}

stats

#SVM
stats <- data.frame(model=character(0),PRED_AP=numeric(0),GOOGLE_AP=numeric(0),PRED_RP=numeric(0),GOOGLE_RP=numeric(0),PRED_RR=numeric(0),GOOGLE_RR=numeric(0),PRED_NDCG=numeric(0),GOOGLE_NDCG=numeric(0))
#models <- c("mc.svm_SMOTE","mc.svm_UNDER","mc.svm_OVER","mc.svm_GN","mc.svm_IS")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$eval_3
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$eval_3)
  }

  stats_aux <- data.frame(model=models[i],PRED_AP=r[,1],GOOGLE_AP=r[,2],PRED_RP=r[,3],GOOGLE_RP=r[,4],PRED_RR=r[,5],GOOGLE_RR=r[,6],PRED_NDCG=r[,7],GOOGLE_NDCG=r[,8])
  

  stats <- rbind(stats,stats_aux)

}

stats

#MARS
stats <- data.frame(model=character(0),PRED_AP=numeric(0),GOOGLE_AP=numeric(0),PRED_RP=numeric(0),GOOGLE_RP=numeric(0),PRED_RR=numeric(0),GOOGLE_RR=numeric(0),PRED_NDCG=numeric(0),GOOGLE_NDCG=numeric(0))
#models <- c("mc.mars_SMOTE","mc.mars_UNDER","mc.mars_OVER","mc.mars_IS")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$eval_3
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$eval_3)
  }
  
  stats_aux <- data.frame(model=models[i],PRED_AP=r[,1],GOOGLE_AP=r[,2],PRED_RP=r[,3],GOOGLE_RP=r[,4],PRED_RR=r[,5],GOOGLE_RR=r[,6],PRED_NDCG=r[,7],GOOGLE_NDCG=r[,8])
  
  
  stats <- rbind(stats,stats_aux)
  
}

stats

#RF
stats <- data.frame(model=character(0),PRED_AP=numeric(0),GOOGLE_AP=numeric(0),PRED_RP=numeric(0),GOOGLE_RP=numeric(0),PRED_RR=numeric(0),GOOGLE_RR=numeric(0),PRED_NDCG=numeric(0),GOOGLE_NDCG=numeric(0))
#models <- c("mc.rf_SMOTE","mc.rf_UNDER","mc.rf_OVER","mc.rf_GN","mc.rf_IS")

for(i in 1:length(models)) {
  r <- getIterationsInfo(exp,workflow=i,task=1,it=1)$eval_3
  for(a in 2:50) {
    r <- rbind(r,getIterationsInfo(exp,workflow=i,task=1,it=a)$eval_3)
  }
  
  stats_aux <- data.frame(model=models[i],PRED_AP=r[,1],GOOGLE_AP=r[,2],PRED_RP=r[,3],GOOGLE_RP=r[,4],PRED_RR=r[,5],GOOGLE_RR=r[,6],PRED_NDCG=r[,7],GOOGLE_NDCG=r[,8])
  
  
  stats <- rbind(stats,stats_aux)
  
}

stats