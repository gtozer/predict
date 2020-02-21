```{r}
#How to calculate heteroskedasticity-robust standard errors:
#Data: Walmart Sales

library(lmtest)
library(sandwich)

coeftest(fitOrg, vcov = vcovHC(fitOrg, type="HC1"))

```

```{r}
#How to run individual regressions by group, for each group's data and extract coefficients and standard errors:
#Data: Walmart Sales

dfwTrain %>% 
  group_by(Store) %>%
  group_modify(~tidy(lm(Weekly_Sales ~ ., data=.x))) %>% 
  filter(term == 'Temperature')

```

```{r}
#How to draw ROC curves and calculate AUC:
#Data: Portuguese Bank

# set the second level of truth to positive in yardstick
options(yardstick.event_first = FALSE)

# load cowplot to change plot theme
library(cowplot)


# fit a logistic regression model to predict whether customers will open an account
glmOut1 <- glm(openedAccount ~ . -pdays -emp.var.rate, family='binomial', data = dfTrain) %>%
  augment() %>%
  mutate(model = "m1") # name the model

# fit a different logistic regression model to predict whether customers will open an account
glmOut2 <- glm(openedAccount ~ campaign + contact, family='binomial', data = dfTrain) %>%
  augment() %>%
  mutate(model = "m2") # name the model

# combine the two data frames by rows into a larger data frame
glmOutAll <- bind_rows(glmOut1, glmOut2)


glmOutAll %>%
  group_by(model) %>% # group to get individual ROC curve for each model
  roc_curve(truth = openedAccount, .fitted) %>% # get values to plot an ROC curve
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) + # plota ROC curve for each model
  geom_line(size = 1.1) +
  geom_abline(slope = 1, intercept = 0, size = 0.4) +
  scale_color_manual(values = c("#4F9EC4", "#C33C23")) +
  coord_fixed() +
  theme_cowplot()


# calculate the AUCs
glmOutAll %>%
  group_by(model) %>% # group to get individual AUC value for each model
  roc_auc(truth = openedAccount, .fitted)
```

```{r}
#How to run k-fold cross validation (alternatively, you can use in the pipe too!):
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
#How to run LOOCV cross validation (alternatively, you can use in the pipe too!):
#Data: Portuguese Bank

set.seed(2020)

#Let's save the same logistic regression as a model (without making predictions)
fitLogCV <- train(openedAccount ~ ., family='binomial', data=dfTrain, method='glm', trControl=trainControl(method='loocv'))

#See the accuracy of each of the 10 folds:
fitLogCV$resample

#See the final (average) accuracy:
fitLogCV$results
```
