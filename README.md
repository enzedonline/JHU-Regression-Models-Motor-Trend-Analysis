# JHU-Regression-Models-Motor-Trend-Analysis

A simple linear regression analysis that forms part of the Johns Hopkins University Regression Modelling course assessment.

This report sets out to examine the relationship between fuel economy and transmission type in the `mtcars` data set.

With only 32 observations but 10 explanatory variables with high degrees of collinearity making 1,024 possible models, the best linear model suffers from a high degree of uncertainty.

Additionally, there is a high degree of bias in the data set with respect to sampled manual and automatic cars, with most manual cars having characteristics independent of transmission type that favour better fuel economy than the automatic cars included.

Using a model selected by backwards regression and VIF elimination, the model suggests that, ***for the cars in the `mtcar` data set, holding all other variables constant, changing from automatic to manual transmission will increase the fuel economy by 2.15 mpg***. However, with a residual standard error of 2.308, **the null hypothesis that transmission has no effect on fuel economy is failed to be rejected**.

Report can be [seen on RPubs](https://rpubs.com/enzedonline/jhu-regression-motor-trends-analysis).
