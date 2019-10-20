data("iris")
str(iris)
table(iris$Species)
head(iris)
set.seed(9850)
gp <- runif(nrow(iris))
iris <- iris[order(gp),]
str(iris) #Check structure of iris
head(iris)
summary(iris[, c(1, 2, 3, 4)])

#Rescale and normalize numerical features
normalize <- function(x) (x - min(x))/(max(x)-min(x))
normalize(c(1, 2, 3, 4, 5))
normalize(iris[, 1:5])
# Apply the previously created Normalize function to all rows and only columns 1 - 4 
iris_norm <- as.data.frame(apply(iris[, 1:4], 2, function(x) (x - min(x))/(max(x)-min(x))))
iris_norm$Species <- iris$Species
str(iris_norm)
iris_n <- as.data.frame(lapply(iris[,c(1,2,3,4)], normalize))
str(iris_n) #Check structure of iris_n
summary(iris_n) #Check the structure of the newly created and or normalized numerical features (variables) Min(s) = 0 and Max(s). This preprocessing in necessary for the KNN algorithm

#Next step will use this pre-processng and create a trainig data set and a separate test data set to assess how well our model performs

# Create a train data set that the KNN algorithm will use to learn a patern
iris_train <- iris_n[1:129, ] #Training data set uses rows 1 - 129 from the normalized dataset and all the columns for the train dataset
iris_test <- iris_n[130:150, ] #Test data set Uses rows 130 - 150 from the normalized dataset and all the columns for the test dtaset
iris_train_target <- iris[1:129, 5] #Target feature (Species) training data set uses rows 1 - 129 and column 5 from theoriginal dataset
iris_test_target <- iris[130:150, 5] #Target feature (Species) testing data set uses rows 130 - 150 and column 5 from the original dataset

#Load the class package
require(class)
#The knn algorithm receives the training, testing and training target data frmes. In addition to the k - a plce holder for how many neighbors we want the algorithm to use
# A rule of thum is to set k = to the sqrt(total number of obs.) and preferably use an odd number to help knn break a potential tie
sqrt(150) # A rule of thum is to set k = to the sqrt(total number of obs.) and preferably use an odd number to help knn break a potential tie

# Generate a prediction fro the classification of all the values in the test dataframe
m1 <- knn(train = iris_train, test = iris_test, cl=iris_train_target, k=13)

#Use a confusion matrix to evaluate how well knn did at classifying those 21 flowers in the test data set
table(iris_test_target, m1) #Use the target test data set in the X axis, and put m1 in the Y axis

m1 #Values in the diagonal of the confusion matrix are the correctly predicted, off the diagonal are values incorrectly predicted
# Target variables that categorical must be coded as factor in R for the KNN algorithm to be sucessful
