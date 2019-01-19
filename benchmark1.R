
library(e1071)
load("dataC50500.Rda")

dataC50500 = bagOfWords
print(nrow(dataC50500))

names(dataC50500)[501] = as.factor("target")

for (n in names(dataC50500)) {
  dataC50500[[n]] <- as.factor(dataC50500[[n]])
}

new_one = data.frame(dataC50500)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dataC50500))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dataC50500)), size = smp_size)

train <- dataC50500[train_ind, ]
test <- dataC50500[-train_ind, ]

print(is.data.frame(train))

Naive_Bayes_Model=naiveBayes(as.factor(X1) ~., data=new_one, laplace = 1)
print(Naive_Bayes_Model)

NB_Predictions=predict(Naive_Bayes_Model,new_one)
print(NB_Predictions)
asdf = table(NB_Predictions,new_one$X1)
print(asdf)

fg = diag(nrow(asdf)) * asdf
print(fg)

acc = sum(fg) / sum(asdf)
print("Accuracy")
print(acc)


bnb <- bernoulliNaiveBayess(X1 ~., new_one)
print(bnb)
bnb_predictions = predict(bnb, new_one)
print(bnb_predictions)
asdf = table(bnb_predictions,new_one$X1)

fg = diag(nrow(asdf)) * asdf
print(fg)

acc = sum(fg) / sum(asdf)
print("Accuracy")
print(acc)


