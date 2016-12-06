#EXPERIMENT 1

setwd("")

economy_bandari <- read.csv("economy_bandari.csv")
economy_svm <- read.csv("economy_svm_none.csv")
economy_svm_UNDER <- read.csv("economy_svm_UNDER.csv")
economy_svm_OVER <- read.csv("economy_svm_OVER.csv")
economy_svm_SMOTE <- read.csv("economy_svm_SMOTE.csv")
economy_svm_IS <- read.csv("economy_svm_IS.csv")
economy_mars <- read.csv("economy_mars_none.csv")
economy_mars_UNDER <- read.csv("economy_mars_UNDER.csv")
economy_mars_OVER <- read.csv("economy_mars_OVER.csv")
economy_mars_SMOTE <- read.csv("economy_mars_SMOTE.csv")
economy_mars_IS <- read.csv("economy_mars_IS.csv")
economy_rf <- read.csv("economy_rf_none.csv")
economy_rf_UNDER <- read.csv("economy_rf_UNDER.csv")
economy_rf_OVER <- read.csv("economy_rf_OVER.csv")
economy_rf_SMOTE <- read.csv("economy_rf_SMOTE.csv")
economy_rf_IS <- read.csv("economy_rf_IS.csv")

economy <- rbind(economy_bandari,
                 economy_svm, economy_svm_UNDER, economy_svm_OVER, economy_svm_SMOTE, economy_svm_IS,
                 economy_mars, economy_mars_UNDER, economy_mars_OVER, economy_mars_SMOTE, economy_mars_IS,
                 economy_rf, economy_rf_UNDER, economy_rf_OVER, economy_rf_SMOTE, economy_rf_IS)

economy.tb <- aggregate(economy,by=list(economy$model),FUN=mean,na.rm=TRUE)

microsoft_bandari <- read.csv("microsoft_bandari.csv")
microsoft_svm <- read.csv("microsoft_svm_none.csv")
microsoft_svm_UNDER <- read.csv("microsoft_svm_UNDER.csv")
microsoft_svm_OVER <- read.csv("microsoft_svm_OVER.csv")
microsoft_svm_SMOTE <- read.csv("microsoft_svm_SMOTE.csv")
microsoft_svm_IS <- read.csv("microsoft_svm_IS.csv")
microsoft_mars <- read.csv("microsoft_mars_none.csv")
microsoft_mars_UNDER <- read.csv("microsoft_mars_UNDER.csv")
microsoft_mars_OVER <- read.csv("microsoft_mars_OVER.csv")
microsoft_mars_SMOTE <- read.csv("microsoft_mars_SMOTE.csv")
microsoft_mars_IS <- read.csv("microsoft_mars_IS.csv")
microsoft_rf <- read.csv("microsoft_rf_none.csv")
microsoft_rf_UNDER <- read.csv("microsoft_rf_UNDER.csv")
microsoft_rf_OVER <- read.csv("microsoft_rf_OVER.csv")
microsoft_rf_SMOTE <- read.csv("microsoft_rf_SMOTE.csv")
microsoft_rf_IS <- read.csv("microsoft_rf_IS.csv")

microsoft <- rbind(microsoft_bandari,
                 microsoft_svm, microsoft_svm_UNDER, microsoft_svm_OVER, microsoft_svm_SMOTE, microsoft_svm_IS,
                 microsoft_mars, microsoft_mars_UNDER, microsoft_mars_OVER, microsoft_mars_SMOTE, microsoft_mars_IS,
                 microsoft_rf, microsoft_rf_UNDER, microsoft_rf_OVER, microsoft_rf_SMOTE, microsoft_rf_IS)

microsoft.tb <- aggregate(microsoft,by=list(microsoft$model),FUN=mean,na.rm=TRUE)

obama_bandari <- read.csv("obama_bandari.csv")
obama_svm <- read.csv("obama_svm_none.csv")
obama_svm_UNDER <- read.csv("obama_svm_UNDER.csv")
obama_svm_OVER <- read.csv("obama_svm_OVER.csv")
obama_svm_SMOTE <- read.csv("obama_svm_SMOTE.csv")
obama_svm_IS <- read.csv("obama_svm_IS.csv")
obama_mars <- read.csv("obama_mars_none.csv")
obama_mars_UNDER <- read.csv("obama_mars_UNDER.csv")
obama_mars_OVER <- read.csv("obama_mars_OVER.csv")
obama_mars_SMOTE <- read.csv("obama_mars_SMOTE.csv")
obama_mars_IS <- read.csv("obama_mars_IS.csv")
obama_rf <- read.csv("obama_rf_none.csv")
obama_rf_UNDER <- read.csv("obama_rf_UNDER.csv")
obama_rf_OVER <- read.csv("obama_rf_OVER.csv")
obama_rf_SMOTE <- read.csv("obama_rf_SMOTE.csv")
obama_rf_IS <- read.csv("obama_rf_IS.csv")

obama <- rbind(obama_bandari,
                 obama_svm, obama_svm_UNDER, obama_svm_OVER, obama_svm_SMOTE, obama_svm_IS,
                 obama_mars, obama_mars_UNDER, obama_mars_OVER, obama_mars_SMOTE, obama_mars_IS,
                 obama_rf, obama_rf_UNDER, obama_rf_OVER, obama_rf_SMOTE, obama_rf_IS)

obama.tb <- aggregate(obama,by=list(obama$model),FUN=mean,na.rm=TRUE)

palestine_bandari <- read.csv("palestine_bandari.csv")
palestine_svm <- read.csv("palestine_svm_none.csv")
palestine_svm_UNDER <- read.csv("palestine_svm_UNDER.csv")
palestine_svm_OVER <- read.csv("palestine_svm_OVER.csv")
palestine_svm_SMOTE <- read.csv("palestine_svm_SMOTE.csv")
palestine_svm_IS <- read.csv("palestine_svm_IS.csv")
palestine_mars <- read.csv("palestine_mars_none.csv")
palestine_mars_UNDER <- read.csv("palestine_mars_UNDER.csv")
palestine_mars_OVER <- read.csv("palestine_mars_OVER.csv")
palestine_mars_SMOTE <- read.csv("palestine_mars_SMOTE.csv")
palestine_mars_IS <- read.csv("palestine_mars_IS.csv")
palestine_rf <- read.csv("palestine_rf_none.csv")
palestine_rf_UNDER <- read.csv("palestine_rf_UNDER.csv")
palestine_rf_OVER <- read.csv("palestine_rf_OVER.csv")
palestine_rf_SMOTE <- read.csv("palestine_rf_SMOTE.csv")
palestine_rf_IS <- read.csv("palestine_rf_IS.csv")

palestine <- rbind(palestine_bandari,
                 palestine_svm, palestine_svm_UNDER, palestine_svm_OVER, palestine_svm_SMOTE, palestine_svm_IS,
                 palestine_mars, palestine_mars_UNDER, palestine_mars_OVER, palestine_mars_SMOTE, palestine_mars_IS,
                 palestine_rf, palestine_rf_UNDER, palestine_rf_OVER, palestine_rf_SMOTE, palestine_rf_IS)

palestine.tb <- aggregate(palestine,by=list(palestine$model),FUN=mean,na.rm=TRUE)

#EXPERIMENT 2

setwd("")

economy_baseline <- read.csv("exp2_economy_baseline.csv")
economy_bandari <- read.csv("exp2_economy_bandari.csv")
economy_lm <- read.csv("exp2_economy_lm.csv")
economy_mars <- read.csv("exp2_economy_mars.csv")
economy_svm <- read.csv("exp2_economy_svm.csv")
economy_rf <- read.csv("exp2_economy_rf.csv")

economy <- rbind(economy_baseline,economy_bandari,economy_lm,economy_svm,economy_mars,economy_rf)
economy.tb <- aggregate(economy,by=list(economy$model),FUN=mean,na.rm=TRUE)

microsoft_baseline <- read.csv("exp2_microsoft_baseline.csv")
microsoft_bandari <- read.csv("exp2_microsoft_bandari.csv")
microsoft_lm <- read.csv("exp2_microsoft_lm.csv")
microsoft_mars <- read.csv("exp2_microsoft_mars.csv")
microsoft_svm <- read.csv("exp2_microsoft_svm.csv")
microsoft_rf <- read.csv("exp2_microsoft_rf.csv")

microsoft <- rbind(microsoft_baseline,microsoft_bandari,microsoft_lm,microsoft_svm,microsoft_mars,microsoft_rf)
microsoft.tb <- aggregate(microsoft,by=list(microsoft$model),FUN=mean,na.rm=TRUE)

obama_baseline <- read.csv("exp2_obama_baseline.csv")
obama_bandari <- read.csv("exp2_obama_bandari.csv")
obama_lm <- read.csv("exp2_obama_lm.csv")
obama_mars <- read.csv("exp2_obama_mars.csv")
obama_svm <- read.csv("exp2_obama_svm.csv")
obama_rf <- read.csv("exp2_obama_rf.csv")

obama <- rbind(obama_baseline,obama_bandari,obama_lm,obama_svm,obama_mars,obama_rf)
obama.tb <- aggregate(obama,by=list(obama$model),FUN=mean,na.rm=TRUE)

palestine_baseline <- read.csv("exp2_palestine_baseline.csv")
palestine_bandari <- read.csv("exp2_palestine_bandari.csv")
palestine_lm <- read.csv("exp2_palestine_lm.csv")
palestine_mars <- read.csv("exp2_palestine_mars.csv")
palestine_svm <- read.csv("exp2_palestine_svm.csv")
palestine_rf <- read.csv("exp2_palestine_rf.csv")

palestine <- rbind(palestine_baseline,palestine_bandari,palestine_lm,palestine_svm,palestine_mars,palestine_rf)
palestine.tb <- aggregate(palestine,by=list(palestine$model),FUN=mean,na.rm=TRUE)

#EXPERIMENT 3

setwd("")

economy_baseline <- read.csv("exp3_economy_baseline.csv")
economy_bandari <- read.csv("exp3_economy_bandari.csv")
economy_lm <- read.csv("exp3_economy_lm.csv")
economy_mars <- read.csv("exp3_economy_mars.csv")
economy_svm <- read.csv("exp3_economy_svm.csv")
economy_rf <- read.csv("exp3_economy_rf.csv")

economy <- rbind(economy_baseline,economy_bandari,economy_lm,economy_svm,economy_mars,economy_rf)
economy.tb <- aggregate(economy,by=list(economy$model),FUN=mean,na.rm=TRUE)

microsoft_baseline <- read.csv("exp3_microsoft_baseline.csv")
microsoft_bandari <- read.csv("exp3_microsoft_bandari.csv")
microsoft_lm <- read.csv("exp3_microsoft_lm.csv")
microsoft_mars <- read.csv("exp3_microsoft_mars.csv")
microsoft_svm <- read.csv("exp3_microsoft_svm.csv")
microsoft_rf <- read.csv("exp3_microsoft_rf.csv")

microsoft <- rbind(microsoft_baseline,microsoft_bandari,microsoft_lm,microsoft_svm,microsoft_mars,microsoft_rf)
microsoft.tb <- aggregate(microsoft,by=list(microsoft$model),FUN=mean,na.rm=TRUE)

obama_baseline <- read.csv("exp3_obama_baseline.csv")
obama_bandari <- read.csv("exp3_obama_bandari.csv")
obama_lm <- read.csv("exp3_obama_lm.csv")
obama_mars <- read.csv("exp3_obama_mars.csv")
obama_svm <- read.csv("exp3_obama_svm.csv")
obama_rf <- read.csv("exp3_obama_rf.csv")

obama <- rbind(obama_baseline,obama_bandari,obama_lm,obama_svm,obama_mars,obama_rf)
obama.tb <- aggregate(obama,by=list(obama$model),FUN=mean,na.rm=TRUE)

palestine_baseline <- read.csv("exp3_palestine_baseline.csv")
palestine_bandari <- read.csv("exp3_palestine_bandari.csv")
palestine_lm <- read.csv("exp3_palestine_lm.csv")
palestine_mars <- read.csv("exp3_palestine_mars.csv")
palestine_svm <- read.csv("exp3_palestine_svm.csv")
palestine_rf <- read.csv("exp3_palestine_rf.csv")

palestine <- rbind(palestine_baseline,palestine_bandari,palestine_lm,palestine_svm,palestine_mars,palestine_rf)
palestine.tb <- aggregate(palestine,by=list(palestine$model),FUN=mean,na.rm=TRUE)