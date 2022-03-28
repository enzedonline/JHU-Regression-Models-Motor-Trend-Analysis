lm0 <- lm(formula = mpg ~ ., data = df_cars)
lm_table <- tibble(model = character(), AR2 = numeric(), RMSE = numeric())
df_model <- df_cars %>% select(-gear)

lm1 <- lm(formula = mpg ~ . -cyl, data = df_model)
lm2 <- lm(formula = mpg ~ . -log.disp, data = df_model)
lm3 <- lm(formula = mpg ~ . -drat, data = df_model)
lm4 <- lm(formula = mpg ~ . -wt, data = df_model)
lm5 <- lm(formula = mpg ~ . -qsec, data = df_model)
lm6 <- lm(formula = mpg ~ . -cyl_alignment, data = df_model)
lm7 <- lm(formula = mpg ~ . -log.hp, data = df_model)
lm8 <- lm(formula = mpg ~ . -log.carb, data = df_model)

lm_table <- lm_table %>% add_row(
    model = "lm1",
    AR2 = summary(lm1)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm1$fitted.values)
) %>% add_row(
    model = "lm2",
    AR2 = summary(lm2)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm2$fitted.values)
) %>% add_row(
    model = "lm3",
    AR2 = summary(lm3)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm3$fitted.values)
) %>% add_row(
    model = "lm4",
    AR2 = summary(lm4)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm4$fitted.values)
) %>% add_row(
    model = "lm5",
    AR2 = summary(lm5)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm5$fitted.values)
) %>% add_row(
    model = "lm6",
    AR2 = summary(lm6)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm6$fitted.values)
) %>% add_row(
    model = "lm7",
    AR2 = summary(lm7)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm7$fitted.values)
) %>% add_row(
    model = "lm8",
    AR2 = summary(lm8)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm8$fitted.values)
) 

df_model <- df_model %>% select(-cyl_alignment)

lm11 <- lm(formula = mpg ~ . -cyl, data = df_model)
lm12 <- lm(formula = mpg ~ . -log.disp, data = df_model)
lm13 <- lm(formula = mpg ~ . -drat, data = df_model)
lm14 <- lm(formula = mpg ~ . -wt, data = df_model)
lm15 <- lm(formula = mpg ~ . -qsec, data = df_model)
lm16 <- lm(formula = mpg ~ . -log.hp, data = df_model)
lm17 <- lm(formula = mpg ~ . -log.carb, data = df_model)


lm_table <- lm_table %>% add_row(
    model = "lm11",
    AR2 = summary(lm11)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm11$fitted.values)
) %>% add_row(
    model = "lm12",
    AR2 = summary(lm12)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm12$fitted.values)
) %>% add_row(
    model = "lm13",
    AR2 = summary(lm13)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm13$fitted.values)
) %>% add_row(
    model = "lm14",
    AR2 = summary(lm14)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm14$fitted.values)
) %>% add_row(
    model = "lm15",
    AR2 = summary(lm15)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm15$fitted.values)
) %>% add_row(
    model = "lm16",
    AR2 = summary(lm16)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm16$fitted.values)
) %>% add_row(
    model = "lm17",
    AR2 = summary(lm17)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm17$fitted.values)
) 

df_model <- df_model %>% select(-qsec)

lm21 <- lm(formula = mpg ~ . -cyl, data = df_model)
lm22 <- lm(formula = mpg ~ . -log.disp, data = df_model)
lm23 <- lm(formula = mpg ~ . -drat, data = df_model)
lm24 <- lm(formula = mpg ~ . -wt, data = df_model)
lm25 <- lm(formula = mpg ~ . -log.hp, data = df_model)
lm26 <- lm(formula = mpg ~ . -log.carb, data = df_model)


lm_table <- lm_table %>% add_row(
    model = "lm21",
    AR2 = summary(lm21)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm21$fitted.values)
) %>% add_row(
    model = "lm22",
    AR2 = summary(lm22)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm22$fitted.values)
) %>% add_row(
    model = "lm23",
    AR2 = summary(lm23)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm23$fitted.values)
) %>% add_row(
    model = "lm24",
    AR2 = summary(lm24)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm24$fitted.values)
) %>% add_row(
    model = "lm25",
    AR2 = summary(lm25)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm25$fitted.values)
) %>% add_row(
    model = "lm26",
    AR2 = summary(lm26)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm26$fitted.values)
) 


df_model <- df_model %>% select(-cyl)

lm31 <- lm(formula = mpg ~ . -log.disp, data = df_model)
lm32 <- lm(formula = mpg ~ . -drat, data = df_model)
lm33 <- lm(formula = mpg ~ . -wt, data = df_model)
lm34 <- lm(formula = mpg ~ . -log.hp, data = df_model)
lm35 <- lm(formula = mpg ~ . -log.carb, data = df_model)


lm_table <- lm_table %>% add_row(
    model = "lm31",
    AR2 = summary(lm31)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm31$fitted.values)
) %>% add_row(
    model = "lm32",
    AR2 = summary(lm32)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm32$fitted.values)
) %>% add_row(
    model = "lm33",
    AR2 = summary(lm33)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm33$fitted.values)
) %>% add_row(
    model = "lm34",
    AR2 = summary(lm34)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm34$fitted.values)
) %>% add_row(
    model = "lm35",
    AR2 = summary(lm35)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm35$fitted.values)
) 


df_model <- df_model %>% select(-drat)

lm41 <- lm(formula = mpg ~ . -log.disp, data = df_model)
lm42 <- lm(formula = mpg ~ . -wt, data = df_model)
lm43 <- lm(formula = mpg ~ . -log.hp, data = df_model)
lm44 <- lm(formula = mpg ~ . -log.carb, data = df_model)


lm_table <- lm_table %>% add_row(
    model = "lm41",
    AR2 = summary(lm41)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm41$fitted.values)
) %>% add_row(
    model = "lm42",
    AR2 = summary(lm42)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm42$fitted.values)
) %>% add_row(
    model = "lm43",
    AR2 = summary(lm43)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm43$fitted.values)
) %>% add_row(
    model = "lm44",
    AR2 = summary(lm44)$adj.r.squared, 
    RMSE = rmse(actual = mtcars$mpg, predicted = lm44$fitted.values)
) 







