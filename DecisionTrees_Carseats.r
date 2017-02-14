# Fitting/Classification Tree Models
library(ISLR)
library(tree)

attach(Carseats)

head(Carseats)

### Start Data Manipulation
range(Sales)  # Sales range from 0 to 16
# create a categorical variables bases on Sales
High = ifelse(Sales >= 8, "Yes", "No")
# appends High to Carseat dataset, and now our dataset is ready!
Carseats = data.frame(Carseats, High)
Carseats = Carseats[,-1] # Remove first column
### Split data in to testing and training using
set.seed(2)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
test = -train
training_data = Carseats[train,]
testing_data = Carseats[test, ]
testing_High = High[test]

# fit tree model using training data
tree_model = tree(High~., training_data)

plot(tree_model)
text(tree_model, pretty=0)

# check how the model is doing using the test data
tree_pred = predict(tree_model, testing_data, type="class")
mean(tree_pred != testing_High) # 28.5%

## Pruning the tree
## Cross validation to check where to stop pruning

set.seed(3)

cv_tree = cv.tree(tree_model, FUN = prune.misclass)

plot(cv_tree$size, cv_tree$dev, type="b") # Optimized at 9

### Prune the tree--for reals

pruned_model = prune.misclass(tree_model, best = 9)
plot(pruned_model)
text(pruned_model, pretty = 0)

### Check how it is doing

tree_pred = predict(pruned_model, testing_data, type = "class")
mean(tree_pred != testing_High)
