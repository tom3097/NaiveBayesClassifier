#Getting started with Naive Bayes
#Install the package
#install.packages("e1071")
#Loading the library
library(e1071)
?naiveBayes #The documentation also contains an example implementation of Titanic dataset
#Next load the Titanic dataset
data("Titanic")
#Save into a data frame and view it
Titanic_df=as.data.frame(Titanic)

#Creating data from table
repeating_sequence=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq) #This will repeat each combination equal to the frequency of each combination
#Create the dataset by row repetition created
Titanic_dataset=Titanic_df[repeating_sequence,]
#We no longer need the frequency, drop the feature
Titanic_dataset$Freq=NULL
levels(Titanic_dataset$Age)
names(Titanic_dataset)
