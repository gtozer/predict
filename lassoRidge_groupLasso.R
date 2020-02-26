```{r}
#Lasso regression (change alpha to 0 for Ridge regression)

#Set the grid for the lambda values
lambdaValues <- 10^seq(-3, 3, length = 100)

set.seed(2020)

fitLasso <- train(openedAccount ~ ., family='binomial', data=dfTrain, method='glmnet', trControl=trainControl(method='cv', number=10), tuneGrid = expand.grid(alpha=1, lambda=lambdaValues))

#Variable importance complete table
varImp(fitLasso)$importance %>%    # Add scale=FALSE inside VarImp if you don't want to scale
  rownames_to_column(var = "Variable") %>%
  mutate(Importance = scales::percent(Overall/100)) %>% 
  arrange(desc(Overall)) %>% 
  as_tibble()

#Variable importance plot with the most important variables
plot(varImp(fitLasso))    # Add top = XX to change the number of visible variables

#Optimum lambda selected by the algorithm
fitLasso$bestTune$lambda   # You can also run fitLasso$finalModel$lambdaOpt

#Not so useful but helps with understanding -See how variables are dropped as lambda increases
#plot(fitLasso$finalModel, xvar="lambda", label = TRUE)

#Not so useful but helps with understanding -See the coefficients from the final lasso model
#coef(fitLasso$finalModel, fitLasso$bestTune$lambda)   # You can also use fitLasso$finalModel$lambdaOpt for optimum lambda

resultsLasso <- 
  fitLasso %>%
  predict(dfTest, type='raw') %>%
  bind_cols(dfTest, predictedClass=.)

resultsLasso %>% 
  xtabs(~predictedClass+openedAccount, .) %>% 
  confusionMatrix(positive = '1')

```

```{r warning=FALSE}
library(grplasso)

dfTrainGroup <-
  dfTrain %>%
  mutate(openedAccount = as.numeric(openedAccount)) %>% 
  mutate(openedAccount = ifelse(openedAccount == 2, 1, 0))

set.seed(123)

fitGroupedLasso <- grplasso(openedAccount ~ ., data=dfTrainGroup, model=LogReg(), lambda=100)

#Coefficients from the group lasso (If a coefficient is zero, the variable is dropped!)
fitGroupedLasso$coefficients

#For any R object, you can see what is inside by running names() such as names(fitGroupedLasso)

```

