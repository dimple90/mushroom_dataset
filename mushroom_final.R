##package installation
library(caret)
library(randomForest)
library(rpart)
library(partykit)
library(e1071)
library(gmodels)

##loading data
mushroom <- read.csv('agaricus-lepiota.csv',header=F,sep=',')

##explore data
names(mushroom) <- c('type','cap_shape','cap_surface','cap_color','bruises','odor','gill_attach','gill_spacing','gill_size','gill_color','stalk_shape','stalk_root','stalk_surface_abo','stalk_surface_bel','stalk_color_abo','stalk_color_bel','veil_type','veil_color','ring_number','ring_type','spore_color','pop','habitat')
#8124 observations, 23 variables
dim(mushroom)
str(mushroom)
summary(mushroom)
#It is likely that this variable was
#somehow coded incorrectly. In any case, since veil_type does not vary across
#samples, it does not provide any useful information for prediction.
mushroom$veil_type <- NULL
#eliminates the feature (partial) from the mushrooms data frame
#class	edible = e , poisonous = p
table(mushroom$type)
round(prop.table(table(mushroom$type))*100)
#About 52 percent of the mushroom samples (N = 4,208) are edible, while 48 percent
#(N = 3,916) are poisonous. As the class levels are split into about 50/50, we do not
#need to worry about imbalanced data.


##EDA
nb_value_counts = table(as.numeric(df$type))
nb_value_counts

barplot(nb_value_counts, names.arg=c('Edible', 'Poisonous'), col=c('Green', 'Red'))
boxplot(as.numeric(df$cap_color), col="blue", main="Boxplot of cap color")
summary(as.numeric(df$cap_color))
##train and test model
df <- mushroom

smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproducible
set.seed(1)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

##MODEL EVALUATION - RANDOM FOREST

rf = randomForest(type ~ .,  
                  ntree = 100,
                  data=train)
plot(rf)
print(rf)

# using TEST dataset
# Predicting response variable
test$predicted.response = predict(rf , test)
test$predicted.response <- as.factor(test$predicted.response)
# Create Confusion Matrix
coMa <- confusionMatrix(test$predicted.response,as.factor(test$predicted.response))
coMa
acc_rt <- coMa$overall["Accuracy"]
acc_rt

##EDA
##important variable in predicting edibility
varImpPlot(rf,  
           sort = T,
           main = "Variable Importance")
##The importance of each attribute according to Mean Decrease Gini is listed below.
var.imp = data.frame(importance(rf, type=2))

# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])







##MODEL EVALUATION - NAIVE BAYES
mushroom_classifer<-naiveBayes(type ~ ., train, test,laplace = 20)
mushroom_prediction<-predict(mushroom_classifer,test)
mushroom_table<-CrossTable(mushroom_prediction,test$predicted.response,prop.chisq = FALSE, prop.t = FALSE,dnn = c('predicted', 'actual'))
coMa <- confusionMatrix(mushroom_prediction,test$predicted.response)
acc_nb <- coMa$overall["Accuracy"]
acc_nb









##MODEL EVALUATION - DECISION TREE
##Train the model
mod1 <- rpart(type~.,data=train,control=rpart.control(cp=0.005,xval=10))
print(mod1)
mod1.p <- as.party(mod1)
plot(mod1.p)
##model evaluation
pred <- predict(mod1,test,type = "class")
table(pred,test$type)
coMa <- confusionMatrix(pred, reference = test$type)
acc_dt <- coMa$overall["Accuracy"]
acc_dt
confusionMatrix(pred, test$type)

##MODEL ACCURACY
# make a dataframe with each of the accuracy
df_acc <- data.frame(model=c("Random Forest", "Naive Bayes", "Decision Tree"), Accuracy=c(acc_rt, acc_nb, acc_dt))
df_acc
barplot(df_acc$Accuracy, names.arg=df_acc$model, col= c("red", "green", "blue"))

prop.table



