setwd("C:/Users/mginithan/Desktop/Kaggle/Animal Shelter")
source("Models.R")

######
#Build Random Forest Model
######
  forest = build_rf() #Accuracy: .6307

######
#Build Naive Bayes
######
  bayes = build_bayes() #Accuracy: 0.6034

######
#Build Neural Net
######
  net = build_net() #Accuracy: 0.6325

######
#Build GBM Model and Predict the Test Data
######
  g_boost = build_gbm() #Accuracy: 0.635
  preds = predict(g_boost,newdata=test,type="response")
  submission = as.data.frame(preds[,,1])
  submission = cbind(test$AnimalID,submission)
  names(submission)[1] = "ID"
  write.csv(submission,file="gbm_submission.csv",sep=",",row.names=FALSE)

######
#Build Logistic Model and Predict the Test Data
######
  logistic = build_logistic() #Accuracy: 0.6154


