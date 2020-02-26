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
