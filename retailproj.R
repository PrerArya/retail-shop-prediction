#import the dataset
train <- read.csv("store_train.csv")
test <-  read.csv("store_test.csv")

#importing lbraries:
library(dplyr)
#check for nul values in the data
sum(is.na(train))   # only 1 null value
sum(is.na(test))   # only 2 null value
# since there are neglibile or too less null value in the data that is we can drop the rows
#  with null values


           #######               Data Preparation             ##########    

#drop the rows with null value from the data
train_new <- na.omit(train)
test_new <- na.omit(test)
#merging the data set:
test_new$store=NA
train_new$data="train"
test_new$data="test"#so that both the datasets have the same no.of obs
net = rbind(train_new,test_new)

#converting the response variable here it is sales to factor:
net$store = as.factor(net$store)

#converting the categorical variables to some dummy variables and constructing sep 
#col for them using function:
Dummy_variables = function(data,var,freq_count=0){
  t = table(data[,var])#selecting the col
  t = t[t > freq_count]
  t = sort(t)
  categories = names(t)[-1]#extract the category names
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)#replaces all occurrences of spaces in the name string with empty string
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)#create a new col with name of cat
  }
  #finally droping the original var col
  data[,var]=NULL
  return(data)
  }
  
#getting our categorical variables:
names(net)[sapply(net,function(x) is.character(x))]
#"countyname" "storecode"  "Areaname"   "countytownname" "state_alpha" "store_Type" "data"          
# data is the col containing train and test 

#checking cardinality of data variables:
length(unique(net$countyname))#1962
length(unique(net$storecode))#2572
length(unique(net$Areaname))#2572
length(unique(net$countytownname))#3173
length(unique(net$state_alpha))#54
length(unique(net$store_Type))#4

#removing var with high cardinality:
net = net %>% select(-countyname,-storecode,-Areaname,-countytownname)
#function call for the rest of variables statealpha,storeType:
rest_col= c("state_alpha","store_Type")

for(cat in rest_col){
  net=Dummy_variables(net,cat,100) 
}

#lets split the data to train and test again :
train_new = net %>% filter(data=="train") %>% select(-data)
test_new =  net %>% filter(data=="test") %>% select(-data,-store)

#split the train data into two so to build model and to test with other
set.seed(2)
s=sample(1:nrow(train_new),0.8*nrow(train_new))
s_train1=train_new[s,]
s_train2=train_new[-s,]

                #########     Model Building       ##########
library(randomForest)
#constructing model with random var and say tree 100
model_rf=randomForest(store~.-Id,data=s_train1,mtry=5,ntree=100)
#validate the model in s_train2:
val.score=predict(model_rf,newdata=s_train2,type='response')
#accuracy check :
library(caret)
confusionMatrix(val.score,s_train2$store)
library(pROC)
auc_score=auc(roc(s_train2$store,val.prob_score[,1]))#area under curve
#Area under the curve: 0.8087 , decent enough
plot(roc(s_train2$store,val.prob_score[,1]))

#building the final model:
model_rf_final=randomForest(store~.-Id,data=train_new,mtry=5,ntree=100)
model_rf_final
#predict final values
test.score=predict(model_rf_final,newdata = test_new,type='response')
test.score
test_new$store = test.score


#exporting the final file to csv
write.csv(test_new, file = "test_final.csv", row.names = FALSE)


