library(dplyr)

#####
#Load Data
#####
  setwd(".../Animal Shelter Competition")
  train <- read.csv("train.csv",stringsAsFactors=FALSE)
  test <- read.csv("test.csv",stringsAsFactors=FALSE)

#####
#Prep Data and Combine Test and Train Sets
#####
  names(test)[1] = "AnimalID"
  test$AnimalID = as.character(test$AnimalID)
  full = bind_rows(train,test)
  full = as.data.frame(full)

#####
#Transform AgeUponOutcome
#####
  full[,8] = gsub("s","",full[,8])
  ageNum = substr(full[,8],1,1)
  timeFrame = substr(full[,8],3,nchar(full[,8]))
  timeFrame = gsub(" ","",timeFrame)
  
  multiplier = ifelse(timeFrame=="",0,
               ifelse(timeFrame=="month",30,
               ifelse(timeFrame=="year",365,
               ifelse(timeFrame=="day",1,
               ifelse(timeFrame=="week",7,-1
               )))))
  
  full = full %>%
        mutate(Age = as.numeric(ageNum)*multiplier) %>%
        mutate(Age = ifelse(is.na(Age)==TRUE,0,Age)) %>%
        select(-AgeuponOutcome)

#####
#Seperate Sex from Neutered/Spayed
#####  
  full = full %>%
        mutate(Sex = ifelse(regexpr("Male",SexuponOutcome)>0,"Male",SexuponOutcome)) %>%
        mutate(Sex = ifelse(regexpr("Female",SexuponOutcome)>0,"Female",Sex)) %>%
        mutate(Sex = ifelse(regexpr("Unknown",SexuponOutcome)>0,"Unknown",Sex))
  
  full = full %>%
        mutate(IsFixed = ifelse(regexpr("Intact",SexuponOutcome)>0,"No",SexuponOutcome)) %>%
        mutate(IsFixed = ifelse(regexpr("Neutered",SexuponOutcome)>0,"Yes",IsFixed)) %>%
        mutate(IsFixed = ifelse(regexpr("Spayed",SexuponOutcome)>0,"Yes",IsFixed)) %>%
        mutate(IsFixed = ifelse(regexpr("Unknown",SexuponOutcome)>0,"Unkown",IsFixed))
  
  full = full %>%
        select(-SexuponOutcome)

#####
#Transform Name variable
#####
  full = full %>%
        mutate(Name = ifelse(nchar(Name)>0,"Yes","No"))

#####  
#Reduce Color variable and add MxColor variable
#####
  full = full %>%
        mutate(MixColor = ifelse(regexpr("/",Color)>0,"Yes","No"))
  
  full = full %>%
        mutate(MainColor = ifelse(regexpr("/",Color)>0, 
                                  substr(Color,1,regexpr("/",Color)-1),
                                  Color))
  
  full = full %>%
        mutate(MainColor = ifelse(regexpr("Black",MainColor)>0,"Black",MainColor)) %>%
        mutate(MainColor = ifelse(regexpr("Blue",MainColor)>0,"Blue",MainColor)) %>%
        mutate(MainColor = ifelse(regexpr("Brown",MainColor)>0,"Brown",MainColor)) %>%
        mutate(MainColor = ifelse(regexpr("Orange",MainColor)>0,"Orange",MainColor)) %>%
        mutate(MainColor = ifelse(regexpr("Yellow",MainColor)>0,"Yellow",MainColor)) %>%
        mutate(MainColor = ifelse(regexpr("Red",MainColor)>0,"Red",MainColor)) %>%
        mutate(MainColor = ifelse(regexpr("Chocolate",MainColor)>0,"Chocolate",MainColor)) %>%
        mutate(MainColor = ifelse(regexpr("Silver",MainColor)>0,"Silver",MainColor)) %>%
        mutate(MainColor = ifelse(regexpr("Calico",MainColor)>0,"Calico",MainColor)) %>%
        mutate(MainColor = ifelse(regexpr("Cream",MainColor)>0,"Cream",MainColor)) %>%
        mutate(MainColor = ifelse(regexpr("Tortie",MainColor)>0,"Tortie",MainColor))
                  
  full = full %>%
        select(-Color)
  
  idx = as.vector(table(full$MainColor) <= 51)
  names = names(table(full$MainColor))[idx]
  
  full = full %>%
        mutate(MainColor = ifelse(MainColor %in% names,"Other",MainColor))

#####  
#Reduce Breed variable
#####  
  full = full %>%
    mutate(MixBreed = ifelse(regexpr("/",Breed)>0,"Yes","No")) %>%
    mutate(MixBreed = ifelse(regexpr("Mix",Breed)>0,"Yes",MixBreed))
  
  full = full %>%
    mutate(MainBreed = ifelse(regexpr("/",Breed)>0,
                              substr(Breed,1,regexpr("/",Breed)-1),
                              Breed)) %>%
    mutate(MainBreed = ifelse(regexpr("Mix",Breed)>0,
                              gsub(" Mix","",Breed),
                              MainBreed))
  
  idx = as.vector(table(full$MainBreed) <= 50)
  names = names(table(full$MainBreed))[idx]
  
  full = full %>%
    mutate(MainBreed = ifelse(MainBreed %in% names,"Other",MainBreed)) %>%
    select(-Breed)

#####  
#Drop OutcomeSubType (Not sure if this is a good idea)
#####  
  full = select(full,-OutcomeSubtype)

#####
#Set Character Variables to Factors
#####
  for(i in 2:ncol(full)){
    if(is.numeric(full[,i])==0){
      full[,i] = as.factor(full[,i])
    }
  }
  
#####
#Split Training and Test Data and Save
#####
  train = full[1:nrow(train),]
  test = full[(nrow(train)+1):nrow(full),]
  
  save(train,file="train_clean.RData")
  save(test,file="test_clean.RData")
  write.table(train,file="train_clean.csv",sep=",",row.names=FALSE)
  write.table(test,file="test_clean.csv",sep=",",row.names=FALSE)
  
