# Assign the training set
train <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"))

# Assign the testing set
test <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"))

library(neuralnet)
library(nnet)
library(rpart)

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
survived <- train$Survived

choochootrain <- data.frame(Pclass=pclass[word(train$Cabin) != ""], 
                            Fare=fare[word(train$Cabin) != ""], 
                            Level=level[word(train$Cabin) != ""])

vacanttrain <- data.frame(Pclass=pclass[word(train$Cabin) == ""], 
                          Fare=fare[word(train$Cabin) == ""]) 


levelpredict <- rpart(Level ~ Pclass + Fare, data=choochootrain, method="class")

level[word(train$Cabin) == ""] <- predict(levelpredict, vacanttrain, type="class")

#for (i in 1:length(level[word(train$Cabin) == ""]))
#    level[word(train$Cabin) == ""][i] <- substr("ABCDEFGT",strtoi(level[word(train$Cabin) == ""][i]),strtoi(level[word(train$Cabin) == ""][i]))

train.clean <- data.frame(Sex=as.numeric(factor(sex,c("male","female"))), 
#                          Pclass=pclass, 
#                          Fare=fare, 
#                          Age=age, 
                          Child=child, 
#                          SibSp=sibsp,
#                          Parch=parch, 
#                          Family=family, 
                          Title=as.numeric(factor(title)), 
#                          Level=as.numeric(level), 
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

#for (i in 1:length(test.level[word(test$Cabin) == ""]))
#    test.level[word(test$Cabin) == ""][i] <- substr("ABCDEFGT",
#                                                    strtoi(test.level[word(test$Cabin) == ""][i]),
#                                                    strtoi(test.level[word(test$Cabin) == ""][i]))


test.clean <- data.frame(Sex=as.numeric(factor(test.sex, c("male","female"))), 
#                         Pclass=test.pclass, 
#                         Fare=test.fare, 
#                         Age=test.age, 
                         Child=test.child, 
#                         SibSp=test.sibsp, 
#                         Parch=test.parch, 
#                         Family=test.family,
                         Title=as.numeric(test.title))#, 
#                         Level=as.numeric(test.level))


sizes <- c(10)

df <- data.frame(Size=integer(), Stat=integer())

set.seed(42)

for (nnsize in sizes)
{
    train.clean.train <- sample(1:891,600)
    train.clean.test <- setdiff(1:891,train.clean.train)

    ideal <- train.clean$Survived


    train.nn <- nnet(train.clean[train.clean.train,-dim(train.clean)[2]], 
                 as.numeric(ideal[train.clean.train]), 
                 size=nnsize, maxit=1000)

    train.clean.predict <- round(predict(train.nn, train.clean[train.clean.test,-dim(train.clean)[2]]))

    stat = prop.table(table(train.clean.predict, train.clean[train.clean.test,]$Survived),1)[1,2] #minimize
    df <- rbind(df, data.frame(Size=nnsize,Stat=stat))
}
df
plot(df)
# prop.table(table(train.clean.predict, train.clean[train.clean.test,]$Survived),1)
# prop.table(table(train.clean.predict, train.clean[train.clean.test,]$Survived),2)

test.nn <- nnet(train.clean[,-dim(train.clean)[2]],
                as.numeric(ideal),
                size=10, maxit=1000)

test.clean.predict <- round(predict(test.nn, test.clean))

table(test.clean.predict)

result <- data.frame(PassengerId=test$PassengerId, Survived=test.clean.predict)

write.csv(result, "titanic4.csv", row.names=FALSE)
