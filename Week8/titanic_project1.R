library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('rpart')
library('randomForest') # classification algorithm
train <-read.csv('/Users/kailan/Documents/GitHub/DataScienceR/week_8/task_8/titanicTrain.csv', stringsAsFactors = F)
test  <-read.csv('/Users/kailan/Documents/GitHub/DataScienceR/week_8/task_8/titanicQuestion.csv', stringsAsFactors = F)
test$Survived <- NA
train_test <-rbind(train,test)
sapply(train_test,function(x) sum(is.na(x)))
sapply(train_test,function(x) sum(x == ""))
faremiss <- which(is.na(train_test$Fare))
train_test[faremiss,]
Fareage <- ggplot(train_test[train_test$Pclass=='3' & train_test$Embarked=='S' & train_test$Age>=50 ,],
                  aes( x=Fare )) +
  geom_density( fill = '#99d6ff',alpha=0.4 ) +
  geom_vline(aes(xintercept=median(Fare,na.rm=T)),colour='red',linetype='dashed') +
  ggtitle("Fare1:Age considered") +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
Fareothers <- ggplot(train_test[train_test$Pclass=='3' & train_test$Embarked=='S',], 
                     aes( x=Fare )) +
  geom_density(fill = '#99d6ff',alpha=0.4 ) +
  geom_vline(aes(xintercept=median(Fare,na.rm=T)),colour='red',linetype='dashed' ) +
  ggtitle("Fare2:Regardless of age") + 
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
library(gridExtra)
grid.arrange(Fareage, Fareothers, ncol=2, nrow=1)
Fareage <- median(train_test[train_test$Pclass=='3' & train_test$Embarked=='S' & train_test$Age>=50 ,]$Fare,
                  na.rm = TRUE)
Fareage
Fareothers <- median(train_test[train_test$Pclass=='3' & train_test$Embarked=='S' ,]$Fare,
                     na.rm = TRUE)
Fareothers
train_test$Fare[faremiss] <- 8.00
ggplot(train_test[1:309,], aes(x = Fare, color = factor(Survived))) +
  geom_line(stat='count', position='dodge') +
  theme_few()
embarkedmiss <- which(train_test$Embarked=="")
train_test[embarkedmiss,]

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=1) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
train_test$Embarked <- factor(train_test$Embarked)
train_test$familysize <- train_test$SibSp + train_test$Parch +1
ggplot(train_test[1:309,], aes(x = familysize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few() + label_value()+label_context()
train_test$familysize <- factor(train_test$familysize)

