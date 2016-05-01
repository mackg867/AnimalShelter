setwd("C:/Users/Computer/Desktop/Kaggle/Animal Shelter Competition")
source("Models.R")
load("test_clean.RData")

#Build Random Forest Model
forest = build_RF() #Accuracy: .6307

#Build Naive Bayes
bayes = build_Bayes() #Accuracy: 0.6034

#Build Neural Net
net = build_Net() #Accuracy: 0.6325
#Build GBM 
g_boost = build_GBM() #Accuracy: 0.635
preds = predict(g_boost,newdata=test,type="response")
submission = as.data.frame(preds[,,1])
submission = cbind(test$AnimalID,submission)
names(submission)[1] = "ID"
write.csv(submission,file="gbm_submission.csv",sep=",",row.names=FALSE)



