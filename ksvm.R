model <- ksvm(class~., data=trainSet)
pred <- predict(model, trainSet[1])
table(pred, trainSet[,2])