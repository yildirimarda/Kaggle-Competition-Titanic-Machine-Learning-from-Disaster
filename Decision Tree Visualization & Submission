train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
test$Survived <- 0

combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
# We code all embarkment codes as factors.
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

library("rpart")
library("rattle")
# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.
combi$family_size <- combi$SibSp + combi$Parch + 1

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(predicted_age, combi[is.na(combi$Age),])

train_new <- combi[1:891,]
test_new <- combi[892:1309,]
test_new$Survived <- NULL

# Find Cabin Class 
train_new$Cabin <- substr(train_new$Cabin,1,1)
test_new$Cabin <- substr(test_new$Cabin,1,1)

train_new$Cabin <- factor(train_new$Cabin)
test_new$Cabin <- factor(test_new$Cabin)



# train_new and test_new are available in the workspace
str(train_new)
str(test_new)

# Create a new model `my_tree_five`
# Age - Cabin ???
my_tree_five <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data = train_new, method = "class", control=rpart.control(cp=0.0001))

# Visualize your new decision tree
fancyRpartPlot(my_tree_five)

# Make your prediction using `my_tree_five` and `test_new`
my_prediction <- predict(my_tree_five, test_new, type = "class")
head(my_prediction)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
vector_passengerid <- test_new$PassengerId

my_solution <- data.frame(PassengerId = vector_passengerid, Survived = my_prediction)

head(my_solution)

# Write your solution to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv",row.names=FALSE)
