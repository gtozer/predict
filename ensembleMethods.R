```{r}
#Decision tree

set.seed(2020)

fitTree <- train(openedAccount ~ ., data=dfTrain, method='rpart', trControl=trainControl(method='cv', number=10))
#For a larger tree, you can add control = rpart.control(minsplit = XX) to the train function (the default is 20)
#Add tuneGrid=expand.grid(cp=-1) if you want to see the full tree

#See the CV output (accuracy per pruning parameter etc.)
fitTree
#See the final decision tree
fitTree$finalModel

#Plot the tree in its basic form
plot(fitTree$finalModel, uniform=TRUE, margin=0.2)
text(fitTree$finalModel)

#Plot the same tree fancier using the rpart.plot library
library(rpart.plot)
prp(fitTree$finalModel, type=5, extra=102, uniform=TRUE, varlen=0,
box.palette="GnBu", branch.lty=3, shadow.col="gray", fallen.leaves=TRUE)

#Make predictions:
resultsTree <-
  fitTree %>% 
  predict(dfTest, type='raw') %>% 
  bind_cols(dfTest, predictedClass=.)

resultsTree %>% 
  xtabs(~predictedClass+openedAccount, .) %>% 
  confusionMatrix(positive = '1')

```


```{r}
#Bagged decision tree

set.seed(2020)

fitBaggedTree <- train(openedAccount ~ ., data=dfTrain, method='treebag', trControl=trainControl(method='cv', number=10))
#The default bootstrap replications is 25. You can change it using the argument nbagg = XX

#See the CV output (accuracy per pruning parameter etc.)
fitBaggedTree$finalModel

#See the variables plotted by importance (according to the bagged tree):
plot(varImp(fitBaggedTree), top=20)

#See the variables listed by importance (according to the bagged tree)
varImp(fitBaggedTree)$importance %>%    # Add scale=FALSE inside VarImp if you don't want to scale
  rownames_to_column(var = "Variable") %>%
  mutate(Importance = scales::percent(Overall/100)) %>% 
  arrange(desc(Overall)) %>% 
  as_tibble()

#Make predictions:
resultsBaggedTree <-
  fitBaggedTree %>% 
  predict(dfTest, type='raw') %>% 
  bind_cols(dfTest, predictedClass=.)

resultsBaggedTree %>% 
  xtabs(~predictedClass+openedAccount, .) %>% 
  confusionMatrix(positive = '1') #%>% 
  # tidy()

```


```{r}
#Let's fit a random forest using ranger and 10-fold CV, and make predictions

resultsRandomForest <- train(openedAccount ~ ., data=dfTrain, method='ranger', trControl=trainControl(method='cv', number=10)) %>%
  predict(dfTest, type='raw') %>%
  bind_cols(dfTest, predictedClass=.)

resultsRandomForest %>%
  xtabs(~predictedClass+openedAccount, .) %>%
  confusionMatrix(positive = '1')

```


```{r}
#Let's fit a adaptive boosting model using 10-fold CV, and make predictions

resultsAdaptive <- train(openedAccount ~ . -job, data=dfTrain, method='ada', trControl=trainControl(method='cv', number=10)) %>%
  predict(dfTest, type='raw') %>%
  bind_cols(dfTest, predictedClass=.)

resultsAdaptive %>%
  xtabs(~predictedClass+openedAccount, .) %>%
  confusionMatrix(positive = '1')

```


```{r}
#Let's fit a gradient boosting model using 10-fold CV, and make predictions

resultsGradient <- train(openedAccount ~ ., data=dfTrain, method='gbm', trControl=trainControl(method='cv', number=10), verbose=FALSE) %>%
  predict(dfTest, type='raw') %>%
  bind_cols(dfTest, predictedClass=.)

resultsGradient %>%
  xtabs(~predictedClass+openedAccount, .) %>%
  confusionMatrix(positive = '1')

```


```{r}
#(Supposedly fast) adaptive boosting

set.seed(2020)

fitAdaBoost <- train(openedAccount ~ ., data=dfTrain, method='adaboost', trControl=trainControl(method='cv', number=10))

#See the CV output (accuracy per pruning parameter etc.)
fitAdaBoost

#See the variables plotted by importance (according to the bagged tree):
plot(varImp(fitAdaBoost), top=20)

#See the variables listed by importance (according to the bagged tree)
varImp(fitAdaBoost)$importance %>%    # Add scale=FALSE inside VarImp if you don't want to scale
  rownames_to_column(var = "Variable") %>%
  mutate(Importance = scales::percent(Overall/100)) %>% 
  arrange(desc(Overall)) %>% 
  as_tibble()

#Make predictions:
resultsAdaBoost <-
  fitAdaBoost %>% 
  predict(dfTest, type='raw') %>% 
  bind_cols(dfTest, predictedClass=.)

resultsAdaBoost %>% 
  xtabs(~predictedClass+openedAccount, .) %>% 
  confusionMatrix(positive = '1')

```


```{r}
#Random forest using rf (instead of ranger)

set.seed(2020)

fitRandomForest <- train(openedAccount ~ ., data=dfTrain, method='rf', trControl=trainControl(method='cv', number=10))
#You can use mtry to set a subset size manually if you like, and you can use ntree set the number of trees
#You can also use the ranger function to change the split rule from Gini. FYI, ntree becomes num.trees in ranger

#See the CV output (accuracy per pruning parameter etc.)
fitRandomForest

#See the variables plotted by importance (according to the bagged tree):
plot(varImp(fitRandomForest), top=20)

#See the variables listed by importance (according to the bagged tree)
varImp(fitRandomForest)$importance %>%    # Add scale=FALSE inside VarImp if you don't want to scale
  rownames_to_column(var = "Variable") %>%
  mutate(Importance = scales::percent(Overall/100)) %>% 
  arrange(desc(Overall)) %>% 
  as_tibble()

#Make predictions:
resultsRandomForest <-
  fitRandomForest %>% 
  predict(dfTest, type='raw') %>% 
  bind_cols(dfTest, predictedClass=.)

resultsRandomForest %>% 
  xtabs(~predictedClass+openedAccount, .) %>% 
  confusionMatrix(positive = '1')

```


```{r}
#XGBoost

set.seed(2020)

fitXGBoost <- train(openedAccount ~ ., data=dfTrain, method='xgbTree', trControl=trainControl(method='cv', number=10))

#See the CV output (accuracy per pruning parameter etc.)
fitXGBoost$results %>% 
  arrange(-Accuracy)

#See the variables plotted by importance (according to the bagged tree):
plot(varImp(fitXGBoost), top=20)

#See the variables listed by importance (according to the bagged tree)
varImp(fitXGBoost)$importance %>%    # Add scale=FALSE inside VarImp if you don't want to scale
  rownames_to_column(var = "Variable") %>%
  mutate(Importance = scales::percent(Overall/100)) %>% 
  arrange(desc(Overall)) %>% 
  as_tibble()

#Make predictions:
resultsXGBoost <-
  fitXGBoost %>% 
  predict(dfTest, type='raw') %>% 
  bind_cols(dfTest, predictedClass=.)

resultsXGBoost %>% 
  xtabs(~predictedClass+openedAccount, .) %>% 
  confusionMatrix(positive = '1')
          
```

