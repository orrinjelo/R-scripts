# Assign the training set
train <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"))

# Assign the testing set
test <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"))

library(nnet)
library(caret)
library(rpart)

sex <- train$Sex
pclass <- train$Pclass
fare <- train$Fare
age <- train$Age
child <- survived
child[train$Age < 18] <- 1
child[train$Age >= 18] <- 0
sibsp <- train$SibSp
parch <- train$Parch
family <- train$SibSp + train$Parch + 1
title <- word(word(train$Name, 2, sep=fixed(',')), 2)
cabin <- train$Cabin
level[word(train$Cabin) != ""] <- substr(word(train$Cabin)[word(train$Cabin) != ""],1,1)
survived <- train$Survived

choochootrain <- data.frame(Pclass=pclass[word(train$Cabin) != ""], 
                            Fare=fare[word(train$Cabin) != ""], 
                            #Title=title[word(train$Cabin) != ""], 
                            Level=level[word(train$Cabin) != ""])

vacanttrain <- data.frame(Pclass=pclass[word(train$Cabin) == ""], 
                          Fare=fare[word(train$Cabin) == ""]) 
                          #Title=title[word(train$Cabin) == ""])


levelpredict <- rpart(Level ~ Pclass + Fare, data=choochootrain, method="class")

level[word(train$Cabin) == ""] <- predict(levelpredict, vacanttrain, type="class")

for (i in 1:length(level[word(train$Cabin) == ""]))
    level[word(train$Cabin) == ""][i] <- substr("ABCDEFGT",strtoi(level[word(train$Cabin) == ""][i]),strtoi(level[word(train$Cabin) == ""][i]))

train.clean <- data.frame(Sex=sex, Pclass=pclass, Fare=fare, Age=age, Child=child, 
                          SibSp=sibsp, Parch=parch, Family=family, Title=title, 
                          Cabin=cabin, Level=level, Survived=survived);

# Build the test dataframe
test.sex <- test$Sex
test.pclass <- test$Pclass
test.fare <- test$Fare
test.age <- test$Age
test.child <- test$Age
test.child[test$Age < 18] <- 1
test.child[test$Age >= 18] <- 0
test.sibsp <- test$SibSp
test.parch <- test$Parch
test.family <- test$SibSp + test$Parch + 1
test.title <- word(word(test$Name, 2, sep=fixed(',')), 2)
test.title[test.title == "Dona."] <- "Jonkheer."
test.cabin <- test$Cabin
test.level <- test.cabin
test.level[word(test$Cabin) != ""] <- substr(word(test$Cabin)[word(test$Cabin) != ""],1,1)


test.choochootrain <- data.frame(Pclass=test.pclass[word(test$Cabin) != ""], 
                            Fare=test.fare[word(test$Cabin) != ""], 
                            #Title=title[word(train$Cabin) != ""], 
                            Level=test.level[word(test$Cabin) != ""])

test.vacanttrain <- data.frame(Pclass=test.pclass[word(test$Cabin) == ""], 
                          Fare=test.fare[word(test$Cabin) == ""]) 
#Title=title[word(train$Cabin) == ""])


test.levelpredict <- rpart(Level ~ Pclass + Fare, data=test.choochootrain, method="class")

test.level[word(test$Cabin) == ""] <- predict(test.levelpredict, test.vacanttrain, type="class")

for (i in 1:length(test.level[word(test$Cabin) == ""]))
    test.level[word(test$Cabin) == ""][i] <- substr("ABCDEFGT",
                                                    strtoi(test.level[word(test$Cabin) == ""][i]),
                                                    strtoi(test.level[word(test$Cabin) == ""][i]))


test.clean <- data.frame(Sex=test.sex, Pclass=test.pclass, Fare=test.fare, Age=test.age, 
                         Child=test.child, SibSp=test.sibsp, Parch=test.parch, Family=test.family,
                         Title=test.title, Level=test.level);

survivetree <- rpart(Survived ~ Sex + Pclass + Fare + Age + Child + SibSp + Parch + Family +
                            Title + Level, 
                     data=train.clean, method="class")

survived.prediction <- predict(survivetree, test.clean, type="class")

plot(survivetree)

report <- data.frame(PassengerId=test$PassengerId, Survived=survived.prediction)

write.csv(report, "~/titanic5.csv", row.names=FALSE)

library(randomForest)
# Fandom Rorests
set.seed(42)
#train.imputed <- rfImpute(as.factor(Survived) ~ Sex + Pclass + Fare  + Child + SibSp 
#                          + Parch + Family + Title + Level,
#                          data = train.clean)
my_forest <- randomForest(as.factor(Survived) ~ Sex + #Pclass + Fare + 
                              Child + #SibSp + Parch + Family + 
                              Title, #+ Level,
                          data = train.clean, importance=TRUE, ntree=1000)

#test.imputed <- rfImpute(as.factor(Title) ~ Sex + Pclass + Fare + Age + Child + SibSp 
#                         + Parch + Family + Level,
#                         data = test.clean)
my_prediction <- predict(my_forest, test.clean)

my_solution <- data.frame(test.clean$PassengerId, my_prediction)

my_forest$importance

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file="rftitanic3.csv", row.names=FALSE)