```{r}
#How to run k-fold cross validation (alternatively, you can use it in the pipe too!):
#Data: Portuguese Bank

set.seed(2020)

#Let's save the same logistic regression as a model (without making predictions)
fitLogCV <- train(openedAccount ~ ., family='binomial', data=dfTrain, method='glm', trControl=trainControl(method='cv', number=10))

#See the accuracy of each of the 10 folds:
fitLogCV$resample

#See the final (average) accuracy:
fitLogCV$results
```

```{r}
#How to run LOOCV cross validation (alternatively, you can use it in the pipe too!):
#Data: Portuguese Bank

set.seed(2020)

#Let's save the same logistic regression as a model (without making predictions)
fitLogCV <- train(openedAccount ~ ., family='binomial', data=dfTrain, method='glm', trControl=trainControl(method='loocv'))

#See the accuracy of each of the 10 folds:
fitLogCV$resample

#See the final (average) accuracy:
fitLogCV$results
```
