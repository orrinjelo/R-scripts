# Assign the training set
train <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"))

# Assign the testing set
test <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"))

library(randomForest)

set.seed(111)

#Build the train dataframe
sex <- train$Sex
pclass <- train$Pclass
fare <- train$Fare
age <- train$Age
age[is.na(age)] <- 28  # Average age
child <- survived
child[age < 18] <- 1
child[age >= 18] <- 0
sibsp <- train$SibSp
parch <- train$Parch
family <- train$SibSp + train$Parch + 1
title <- factor(word(word(train$Name, 2, sep=fixed(',')), 2))
child[title == "Master."] <- 1
child[is.na(child)] <- 0
cabin <- train$Cabin
level <- factor(test.cabin,  c("A","B","C","D","E","F","G","T"))
level[word(train$Cabin) != ""] <- substr(word(train$Cabin)[word(train$Cabin) != ""],1,1)
level <- factor(level,  c("A","B","C","D","E","F","G","T"))
embarked <- train$Embarked
survived <- train$Survived

choochootrain <- data.frame(Pclass=pclass[word(train$Cabin) != ""], 
                            Fare=fare[word(train$Cabin) != ""], 
                            Level=level[word(train$Cabin) != ""])

vacanttrain <- data.frame(Pclass=pclass[word(train$Cabin) == ""], 
                          Fare=fare[word(train$Cabin) == ""]) 


levelpredict <- rpart(Level ~ Pclass + Fare, data=choochootrain, method="class")

level[word(train$Cabin) == ""] <- predict(levelpredict, vacanttrain, type="class")

train.clean <- data.frame(Sex=sex, 
                          Pclass=pclass, 
                          Fare=fare, 
                          Age=age, 
                          Child=child, 
                          SibSp=sibsp,
                          Parch=parch, 
                          Family=family, 
                          Title=as.numeric(factor(title)), 
                          Embarked=embarked,
                          Level=as.numeric(level), 
                          Survived=survived)

# Build the test dataframe
test.sex <- test$Sex
test.pclass <- test$Pclass
test.fare <- test$Fare
test.age <- test$Age
test.age[is.na(test.age)] <- 28  # Average age
test.child <- test$Age
test.child[test$Age < 18] <- 1
test.child[test$Age >= 18] <- 0
test.sibsp <- test$SibSp
test.parch <- test$Parch
test.family <- test$SibSp + test$Parch + 1
test.title <- word(word(test$Name, 2, sep=fixed(',')), 2)
test.title[test.title == "Dona."] <- "Jonkheer."
test.title <- factor(test.title)
child[title == "Master."] <- 1
test.child[is.na(test.child)] <- 0
test.cabin <- test$Cabin
test.level <- factor(test.cabin, c("A","B","C","D","E","F","G","T"))
test.level[word(test$Cabin) != ""] <- substr(word(test$Cabin)[word(test$Cabin) != ""],1,1)
test.level <- factor(test.level,  c("A","B","C","D","E","F","G","T"))
test.embarked <- test$Embarked
test.fare[is.na(test.fare)] <- mean(test.fare[test.level=="F"][-110])

test.choochootrain <- data.frame(Pclass=test.pclass[word(test$Cabin) != ""], 
                                 Fare=test.fare[word(test$Cabin) != ""], 
                                 #Title=title[word(train$Cabin) != ""], 
                                 Level=test.level[word(test$Cabin) != ""])

test.vacanttrain <- data.frame(Pclass=test.pclass[word(test$Cabin) == ""], 
                               Fare=test.fare[word(test$Cabin) == ""]) 
#Title=title[word(train$Cabin) == ""])


test.levelpredict <- rpart(Level ~ Pclass + Fare, data=test.choochootrain, method="class")

test.level[word(test$Cabin) == ""] <- predict(test.levelpredict, test.vacanttrain, type="class")


test.clean <- data.frame(Sex=test.sex, 
                         Pclass=test.pclass, 
                         Fare=test.fare, 
                         Age=test.age, 
                         Child=test.child, 
                         SibSp=test.sibsp, 
                         Parch=test.parch, 
                         Family=test.family,
                         Title=as.numeric(test.title), 
                         Embarked=test.embarked,
                         Level=as.numeric(test.level)
                         )

levels(test.clean$Embarked) <- levels(train.clean$Embarked)

#plot(my_forest)

#test.imputed <- rfImpute(as.factor(Title) ~ Sex + Pclass + Fare + Age + Child + SibSp 
#                         + Parch + Family + Level,
#                         data = test.clean)

for (t in c(3,4,5,6))
{
    my_forest <- randomForest(as.factor(Survived) ~ Sex + Pclass + Fare + Age +
                              Child + SibSp + Parch + 
                              Family + Title, + Embarked + Level,
                          data = train.clean, importance=TRUE, ntree=5000, mtry=t)
    my_forest$importance
    my_prediction <- predict(my_forest, test.clean)
    
    my_prediction[is.na(my_prediction)] <- 0
    
    my_solution <- data.frame(PassengerId=test$PassengerId, Survived=as.numeric(my_prediction)-1)
    
    my_forest$importance
    
    write.csv(my_solution, paste("titanic",t,".csv",sep=''), row.names=FALSE)
}
