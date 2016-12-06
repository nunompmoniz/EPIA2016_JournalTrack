svm_under <- rbind(economy_svm_UNDER,microsoft_svm_UNDER,obama_svm_UNDER,palestine_svm_UNDER)
svm_over <- rbind(economy_svm_OVER,microsoft_svm_OVER,obama_svm_OVER,palestine_svm_OVER)
svm_smote <- rbind(economy_svm_SMOTE,microsoft_svm_SMOTE,obama_svm_SMOTE,palestine_svm_SMOTE)
svm_is <- rbind(economy_svm_IS,microsoft_svm_IS,obama_svm_IS,palestine_svm_IS)

mars_under <- rbind(economy_mars_UNDER,microsoft_mars_UNDER,obama_mars_UNDER,palestine_mars_UNDER)
mars_over <- rbind(economy_mars_OVER,microsoft_mars_OVER,obama_mars_OVER,palestine_mars_OVER)
mars_smote <- rbind(economy_mars_SMOTE,microsoft_mars_SMOTE,obama_mars_SMOTE,palestine_mars_SMOTE)
mars_is <- rbind(economy_mars_IS,microsoft_mars_IS,obama_mars_IS,palestine_mars_IS)

rf_under <- rbind(economy_rf_UNDER,microsoft_rf_UNDER,obama_rf_UNDER,palestine_rf_UNDER)
rf_over <- rbind(economy_rf_OVER,microsoft_rf_OVER,obama_rf_OVER,palestine_rf_OVER)
rf_smote <- rbind(economy_rf_SMOTE,microsoft_rf_SMOTE,obama_rf_SMOTE,palestine_rf_SMOTE)
rf_is <- rbind(economy_rf_IS,microsoft_rf_IS,obama_rf_IS,palestine_rf_IS)

wilcox.test(svm_under$F1,svm_over$F1,paired=T,alternative="greater")$p.value
wilcox.test(svm_under$F1,svm_smote$F1,paired=T,alternative="greater")$p.value
wilcox.test(svm_under$F1,svm_is$F1,paired=T,alternative="greater")$p.value
wilcox.test(svm_over$F1,svm_smote$F1,paired=T,alternative="greater")$p.value
wilcox.test(svm_over$F1,svm_is$F1,paired=T,alternative="greater")$p.value
wilcox.test(svm_smote$F1,svm_is$F1,paired=T,alternative="greater")$p.value

wilcox.test(mars_under$F1,mars_over$F1,paired=T,alternative="greater")$p.value
wilcox.test(mars_under$F1,mars_smote$F1,paired=T,alternative="greater")$p.value
wilcox.test(mars_under$F1,mars_is$F1,paired=T,alternative="greater")$p.value
wilcox.test(mars_over$F1,mars_smote$F1,paired=T,alternative="greater")$p.value
wilcox.test(mars_over$F1,mars_is$F1,paired=T,alternative="greater")$p.value
wilcox.test(mars_smote$F1,mars_is$F1,paired=T,alternative="greater")$p.value

wilcox.test(rf_under$F1,rf_over$F1,paired=T,alternative="greater")$p.value
wilcox.test(rf_under$F1,rf_smote$F1,paired=T,alternative="greater")$p.value
wilcox.test(rf_under$F1,rf_is$F1,paired=T,alternative="greater")$p.value
wilcox.test(rf_over$F1,rf_smote$F1,paired=T,alternative="greater")$p.value
wilcox.test(rf_over$F1,rf_is$F1,paired=T,alternative="greater")$p.value
wilcox.test(rf_smote$F1,rf_is$F1,paired=T,alternative="greater")$p.value



