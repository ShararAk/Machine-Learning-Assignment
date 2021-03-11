install.packages("rmarkdown")
install.packages("knitr")
 remotes::install_github('rstudio/rmarkdown')


library(lattice)
library(ggplot2)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

#Loading Data
trainingset <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
testingset <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))


set.seed(1234)
# Perform exploratory analysis - 
dim(trainingset)
dim(testingset)
summary(trainingset)
summary(testingset)
str(trainingset)
str(testingset)
head(trainingset)
head(testingset)


# Delete columns with all missing values
trainingset<-trainingset[,colSums(is.na(trainingset)) == 0]
testingset <-testingset[,colSums(is.na(testingset)) == 0]

# Delete variables are irrelevant to our current project: user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7). 
trainingset   <-trainingset[,-c(1:7)]
testingset <-testingset[,-c(1:7)]

# partition the data so that 75% of the training dataset into training and the remaining 25% to testing
traintrainset <- createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
TrainTrainingSet <- trainingset[traintrainset, ] 
TestTrainingSet <- trainingset[-traintrainset, ]


# The variable "classe" contains 5 levels: A, B, C, D and E. A plot of the 
#outcome variable will allow us to see the frequency of each levels in the 
#TrainTrainingSet data set and # compare one another.

ggplot(data.frame(TrainTrainingSet$classe), aes(x=TrainTrainingSet$classe)) +
  geom_bar()+
  geom_bar(fill="lightgreen")+
  geom_bar(fill="lightblue",aes(y=..count../sum(..count..)))+
  labs(x = "classe", y = "Frequency")


#Decision Tree

model1 <- rpart(classe ~ ., data=TrainTrainingSet, method="class")

prediction1 <- predict(model1, TestTrainingSet, type = "class")

# Plot the Decision Tree
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)


# Test results on our TestTrainingSet data set:
#str(prediction1)
#str(TestTrainingSet$classe)

#Both input in the below matrix should be as a smae type
confusionMatrix(prediction1, as.factor(TestTrainingSet$classe))


#Random Forest

TrainTrainingSet$classe = factor (TrainTrainingSet$classe)
model2 <- randomForest(classe ~. , data=TrainTrainingSet, method="class")

# Predicting:
prediction2 <- predict(model2, TestTrainingSet, type = "class")

# Test results on TestTrainingSet data set:
confusionMatrix(prediction2, as.factor(TestTrainingSet$classe))



# predict outcome levels on the original Testing data set using Random Forest algorithm
predictfinal <- predict(model2, testingset, type="class")
predictfinal

