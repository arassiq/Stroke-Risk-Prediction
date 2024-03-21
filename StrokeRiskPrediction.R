# loads all the correct libraries:
library(tree) # has classification model within library 
library(pastecs) # has statistical functions 
library(randomForest) # ensembles classification algorithm to improve accuracy of model prediction

class(stroke_data) # use to make sure that the data type is data frame 
str(stroke_data) # allows me to see the names of attributes, data type, and some data held within the attributes allowing me to see whether there are factors or not  
summary(stroke_data) # use summary to summarize the different attributes in my data set (statistically)

# Shows the distribution of age in our data 
hist(stroke_data$age) # we see that the age of most applicants are 30-80 years old





#Pre-Processing 

# due to the fact that ID is a nominal attribute, and therefore does not add any explanation power to our data set, i will go ahead and omit it from the data set 
stroke_data <- subset(stroke_data, select = -id)

#we can also see that many attributes are in the chr data type which is not allowed in a dtree, we will now convert thsse to factors
stroke_data$gender <- as.factor(stroke_data$ever_married)
stroke_data$ever_married <- as.factor(stroke_data$ever_married)
stroke_data$work_type <- as.factor(stroke_data$work_type)
stroke_data$Residence_type <- as.factor(stroke_data$Residence_type)
stroke_data$smoking_status <- as.factor(stroke_data$smoking_status)

#will then check for missing data in the data set 
sum(is.na(stroke_data)) #due to 45 missing values in the data set, i will have to remove the missing values 
stroke_data <- na.omit(stroke_data) # this function omits any missing data from the data set 
sum(is.na(stroke_data)) # use this to double check that there are no more missing values in the data set 

# we now want to make whether someone had a stroke be "Yes" or "No" instead of 1 or 0
strokeFactor <- ifelse(stroke_data$stroke == 1, "Yes", "No") # lets us use Yes or No instead of 1 or 0 for strokes
heart_diseaseFactor <- ifelse(stroke_data$heart_disease == 1, "Yes", "No") # lets us use Yes or No instead of 1 or 0 for heart disease
hypertensionFactor <- ifelse(stroke_data$hypertension == 1, "Yes", "No") # lets us use Yes or No instead of 1 or 0 for hypertension

# Now we want to turn these "Yes" or "No" into factors 
strokeFactor <- as.factor(strokeFactor)
heart_diseaseFactor <- as.factor(heart_diseaseFactor)
hypertensionFactor <- as.factor(hypertensionFactor)

# We need to now add these factors into our data set and remove the old ones 
stroke_data <- data.frame(stroke_data, strokeFactor)
stroke_data <- data.frame(stroke_data, heart_diseaseFactor)
stroke_data <- data.frame(stroke_data, hypertensionFactor)

# We will then remove these factors from our data set
stroke_data <- subset(stroke_data, select = -stroke)
stroke_data <- subset(stroke_data, select = -hypertension)
stroke_data <- subset(stroke_data, select = -heart_disease)





#Create training and testing data sets and attributes

# need to see what number to break training_index into 
training_index_length <- nrow(stroke_data) * .8
test_index_length <- nrow(stroke_data) - training_index_length

#now going to create our test set and training set, yet sets the sampling first  

# Sets the random sampling to be uniform every time code is ran 
set.seed(123)

training_index <- sample(1: nrow(stroke_data), training_index_length) # randomly samples 364 observations from our 455 observations and stores those values into our training index 

training_set <- stroke_data[training_index,] #uses the random sampling of training index to store the 364 random data samples into a variable training_set
testing_set <- stroke_data[-training_index,] # uses the left over 91 random samples of stroke_data to create a testing set 

#we will now create a variable holding the correct class labels of the testing_set to compare against after our predictions
Stroke_test <- strokeFactor[-training_index]





#Build Classification Model: train, test, cross validate, prune, use rain forest 

#first we will use tree() to create a prediction model for Stroke (class label) based on all other attributes
dtree <- tree(strokeFactor~., training_set) # tells the tree function we want to predict stroke Factor based on all other attributes in training_set
plot(dtree) # plots our dtree for us to see
text(dtree, pretty = 0) # adds on labels to show us exactly what we are looking at 

#we will then look at the summary of our dtree to see how it is performing 
summary(dtree)
#we are able to see that hyper tension, smoking status, age, glucose level and bmi are actually used, and that the error rate is around 18% 
dtree




#Testing

# we will now use our d tree model to predict  the class labels of our test set
dtree.test <- predict(dtree, testing_set, type = "class")
dtree.test # we can see that we have successfully predicted our class labels, now we must compare them to the right answers

table(dtree.test, Stroke_test) # based on our table we can see that 66 class labels were accurately predicted, we will store this accuracy into a variable
accuracy.test <- (30 + 37)/91 # shows that our accuracy is 72%





#Cross Validation 

# will use prune to see if we can simplify our model whilst improving or maintaining accuracy
dtree.validation <- cv.tree(dtree, FUN = prune.misclass) #cross validates different possibilities of decision trees while discarding different nodes and attributes
dtree.validation # due to the fact that from 9 nodes to 13 nodes standard deviation didn't change much, we can simplify our nodes to 9

dtree.pruned <- prune.misclass(dtree, best = 7) # prunes d tree to 9 nodes and stores in dtree.pruned
plot(dtree.pruned)
text(dtree.pruned, pretty = 0)
dtree.pruned #summarize 


#we will now look at the accuracy of our pruned model
summary(dtree.pruned) # we can see that we had a misclassification error of around 18% again, which means we simplified our model whilst maintaining accuracy 

dtree.pruned.test <- predict(dtree.pruned, testing_set, type = "class") # tests for predictive accuracy of pruned model, predicting class labels in which we will now compare to our test
table(dtree.pruned.test, Stroke_test) # we can see that 66 of our class labels were correct out of 91
pruned.accuracy.test <- (67/91) # our accuracy was 72%


dtree.rf


#Random Forest

#will now create a forest of dtrees to make predictions with and find the average of all of them to improve accuracy 
dtree.rf <- randomForest::randomForest(strokeFactor~., training_set) #regresses all the attributes on class label StrokeFactor using training set 
dtree.rf.test <- predict(dtree.rf,testing_set, type = "class") # uses random forest dtree model to predict our class labels
table(dtree.rf.test, Stroke_test) # we can see that 66 of our class labels were correct out of 91
randomForest.accuracy.test <- (69/91) # our accuracy was 72%

#dtree validation

