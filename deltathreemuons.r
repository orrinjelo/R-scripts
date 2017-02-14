# Tau - 3 Mu Dataset

library(tree)

traindata = read.csv("~/Data/DeltaThreeMuons/training.csv")
head(traindata)

strippeddata = traindata[,-1] # Remove first column

sig = strippeddata$signal # Separate signal out
strippeddata$signal <- NULL # Remove the signal
prod = strippeddata$production
strippeddata$production <- NULL # Remove the production
mass = strippeddata$mass
strippeddata$mass <- NULL # Remove the mass
minANNmu = strippeddata$min_ANNmuon
strippeddata$min_ANNmuon <- NULL

set.seed(13)
train = sample(1:nrow(strippeddata),nrow(strippeddata)/2)
test = -train
training_data = strippeddata[train,]
test_data = strippeddata[test,]

training_sig = sig[train]
testing_sig = sig[test]

# fit tree model using training data
tree_model = ctree(training_sig~., training_data, 
                   controls = ctree_control(maxsurrogate = 3)))

plot(tree_model) # This is what nightmares are made of
#text(tree_model, pretty=0)

tree_pred = predict(tree_model, test_data)
rms = sqrt(mean((tree_pred - testing_sig)^2))
rsq = summary(lm(tree_pred ~ testing_sig))  # I got %50.56%.  Yikes!
rms
rsq

# Let's look at a random forest approach

# forest_model = cforest(training_sig~., training_data)
# This is dangerous. :|
