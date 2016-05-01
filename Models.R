require(dplyr)
require(randomForest)
require(e1071)
require(caret)
require(h2o)
require(nnet)
require(class)
require(gbm)


######
#Split data into Train and Validation
######
split_data = function(data){
  idx = createDataPartition(data$OutcomeType,p=.95,list=FALSE)
  ls = list(data[idx, ],data[-idx, ])
  names(ls) = c("Train","Validation")
  return(ls)
}

######
#Evaluate Model Performance
######
eval_model = function(model,data){
  #Display Confusion Matrix
  preds = predict(model,newdata=data)
  if(class(data) == "H2OFrame"){
    #The Neural Net Case
    target = as.data.frame(data$OutcomeType)[,1]
    preds = as.data.frame(preds$predict)[,1]
    conf_mat = confusionMatrix(preds,target)
  }else if(class(preds) == "array"){
    #The GBM Case
    outcomes = c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
    preds = apply(preds,1,which.max)
    preds = as.factor(outcomes[preds])
    conf_mat = confusionMatrix(preds,data$OutcomeType)
  }else{
    conf_mat = confusionMatrix(preds,data$OutcomeType)
  }
  print(conf_mat)
}

######
#Build Random Forest Model
######
build_RF = function(){
  set.seed(624)
  load("train_clean.RData")
  #Combine the lowest categories to get to 53 categories for MainBreed
  s = sort(table(train$MainBreed))
  n = names(s[1:(length(s)-52)])
  train$MainBreed = as.character(train$MainBreed)
  train = train %>%
            mutate(MainBreed = ifelse(MainBreed %in% n,"Other",MainBreed))
  train$MainBreed = as.factor(train$MainBreed)
  seg_data = split_data(train)
  
  #Build Model
  forest = randomForest(OutcomeType ~ .,
                        data = seg_data[[1]][,c(-1,-3)],
                        na.action=na.omit)
  eval_model(forest,seg_data[[2]])
  return(forest)
}

#####
#Build Naive Bayes Model
#####
build_Bayes = function(){
  set.seed(624)
  load("train_clean.RData")
  seg_data = split_data(train)
  bayes = naiveBayes(OutcomeType ~ .,
                     data = seg_data[[1]][,c(-1,-3)])  
  eval_model(bayes,seg_data[[2]])
  return(bayes)
}

#####
#Build Neural Net Model
#####
build_Net = function(){
  set.seed(624)
  load("train_clean.RData")
  #Load data and start h2o instance
  localh2o = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = "6g" )
  seg_data = split_data(train)
  trainh2o = as.h2o(seg_data[[1]])
  valh2o = as.h2o(seg_data[[2]])
  
  #Build and evaluate model
  net = h2o.deeplearning(x = c(2,5:12),
                         y = 4,
                         trainh2o,
                         hidden = c(20,20))
  eval_model(net,valh2o)

  #Close h2o instance
  h2o.shutdown()
  return(net)
}
#####
#Build Logistic Regression Model
#####
build_Logist = function(){
  load("train_clean.RData")
  #Combine the bottom enumerations for the Color and Breed variables
  #such that the bottom types make up 20% of the total observations
  breeds = names(sort(table(train$MainBreed),decreasing=TRUE))[1:13]
  colors = names(sort(table(train$MainColor),decreasing=TRUE))[1:8]
  train$MainColor = as.character(train$MainColor)
  train$MainBreed = as.character(train$MainBreed)
  l_train = train %>%
              mutate(MainBreed = ifelse(MainBreed %in% breeds,MainBreed,"Other")) %>%
              mutate(MainColor = ifelse(MainColor %in% colors,MainColor,"Other"))
  l_train$MainColor = as.factor(l_train$MainColor)
  l_train$MainBreed = as.factor(l_train$MainBreed)
  
  #Build model
  set.seed(624)
  logistic = multinom(OutcomeType ~ .,data=l_train[,c(-1,-3)])
  
  #Calculate p-values
  z = summary(logistic)$coefficients/summary(logistic)$standard.errors
  p_values = (1 - pnorm(z,0,1))*2
  return(logistic)
}
  
  
#####
#Build K-NN Model
#####
build_NN = function(){
  load("train_clean.RData")
  #Transform variables into numbers
  k_train = train
  for(i in 1:ncol(train)){
    k_train[,i] = as.numeric(train[,i])
  }
  
  #Create test and training set
  num = floor(nrow(train)*.2)
  idx = sample(1:nrow(train),num)
  k_test = k_train[idx,]
  k_train = k_train[-idx,]
  targets = as.factor(train$OutcomeType)
  
  #Build 
  nearest = knn(train=k_train[,-1],test=k_test[,c(-1,-3)],cl=targets,k=5)
  return(nearest)
}
  
#####
#Build GBM Model
#####  
build_GBM = function(){
  set.seed(624)
  load("train_clean.RData")
  seg_data = split_data(train)
  
  #Build and Evaluate Model
  g_boost = gbm(OutcomeType ~ .,
                distribution = "multinomial",
                data = seg_data[[1]][,c(-1,-3)],
                n.trees = 1000,
                cv.folds = 5,
                shrinkage = .01,
                verbose = TRUE)
  eval_model(g_boost,seg_data[[2]])
  return(g_boost)
}
  