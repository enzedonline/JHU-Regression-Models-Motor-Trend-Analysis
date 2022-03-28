library(tidyverse)
df_cars <- mtcars %>%
    mutate(transmission = factor(am, labels = c("Automatic", "Manual"))) %>%
    mutate(cyl_alignment = factor(vs, labels = c("V-Shaped", "Straight"))) %>%
    select(-am, -vs)

density_graph <- function(variable, description){
    t_colours <- c("Automatic" = "#33A02B", "Manual" = "#1F78B4")
    df_cars %>% 
        ggplot(aes(x=!!as.name(variable), fill=transmission, colour=transmission)) +
        geom_histogram(
            aes(y = stat(density, ..scaled..)),
            position="identity", 
            alpha=0.4, 
            bins = 30, 
        ) +
        geom_line(
            aes(y = ..density..),
            stat = 'density',
            size = 1.5,
            alpha = 0.8
        ) +
        theme_minimal()  +
        theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
        scale_fill_manual(name="Transmission", values=t_colours) +
        scale_colour_manual(name="Transmission", values=t_colours) +
        labs(x=description, y=NULL)
}

library(ggpubr)
eda1 <- density_graph("mpg", "Fuel Economy (mpg)")
eda2 <- density_graph("cyl", "Number of Cylinders")
eda3 <- density_graph("disp", "Displacement (cu in)")
eda4 <- density_graph("hp", "Gross Horsepower")
eda5 <- density_graph("drat", "Rear Axle Ratio")
eda6 <- density_graph("wt", "Weight (1000 lbs)")
eda7 <- density_graph("qsec", "Quarter Mile Time")
eda8 <- density_graph("gear", "Number of Forward Gears")
eda9 <- density_graph("carb", "	Number of Carburetors")
leg <- get_legend(eda9)
as_ggplot(leg)

df_cars %>% 
    ggplot(aes(x=cyl_alignment, fill=transmission)) +
    geom_bar(alpha=0.8)  +
    theme_minimal()  +
    theme(legend.position="none") +
    scale_fill_manual(name="Transmission", values=t_colours) +
    labs(x="Cylinder Alignment", y=NULL)


plot_mpg <- function(variable, description){
    df_cars %>% 
        ggplot(aes(x=!!as.name(variable), y=mpg)) +
        geom_point(colour = "blue", size=5, alpha=0.4) +
        theme_minimal() +
        geom_smooth()
}

plot_mpg("cyl", "Number of Cylinders")
plot_mpg("disp", "Displacement (cu in)")
plot_mpg("hp", "Gross Horsepower")
plot_mpg("drat", "Rear Axle Ratio")
plot_mpg("wt", "Weight (1000 lbs)")
plot_mpg("qsec", "Quarter Mile Time") # drop qsec
plot_mpg("gear", "Number of Forward Gears") # drop gear
plot_mpg("carb", "Number of Carburetors")
plot_mpg("cyl_alignment", "Alignment")

df_cars <- df_cars %>% 
    mutate(log.disp = log(disp))
plot_mpg("log.disp", "Log of Displacement (log(cu in))")

df_cars <- df_cars %>% 
    mutate(log.hp = log(hp))
plot_mpg("log.hp", "Log of Gross Horsepower")

df_cars <- df_cars %>% 
    mutate(log.carb = log(carb))
plot_mpg("log.carb", "Number of Carburetors")


df_cars <- df_cars %>%
    select(-disp, -hp, -carb)


library(GGally)
df_cars %>% 
    select(-mpg, -cyl_alignment, -transmission) %>%
    ggpairs(ggplot2::aes(fill="#1F78B4"))

df_cars %>% 
    select(-mpg, -cyl_alignment, -transmission, -cyl, -log.hp) %>%
    ggpairs(ggplot2::aes(fill="#1F78B4"))

df_model <- df_cars %>% 
    select(-cyl, -log.disp) 

library(car)
mtcars.lm <- lm(mpg ~ ., data = df_model)
as.data.frame(car::vif(mtcars.lm)) %>% arrange(desc(`car::vif(mtcars.lm)`))

mtcars.lm1 <- lm(mpg ~ . -cyl_alignment, data = df_model)
as.data.frame(car::vif(mtcars.lm1)) %>% arrange(desc(`car::vif(mtcars.lm1)`))

mtcars.lm2 <- lm(mpg ~ . -cyl_alignment -log.carb, data = df_model)
mtcars.lm3 <- lm(mpg ~ . -cyl_alignment -log.carb -drat, data = df_model)
summary(mtcars.lm)

fit1 <- lm(mpg ~ ., data =  df_model)
summary(fit1)
MASS::stepAIC(fit1, scope=list(upper = ~ ., lower = ~ transmission))

fit2 <- lm(formula = mpg ~ wt + transmission + log.disp, data = df_model)
summary(fit2)

library(Metrics)
rmse(actual = mtcars$mpg, predicted = fit1$fitted.values)
rmse(actual = mtcars$mpg, predicted = fit2$fitted.values)

anova(fit2, fit1)

#keep fit1

fit <- lm(mpg ~ (wt + transmission + log.disp), df_cars)
summary(fit)

# Histogram to check the distribution of errors
hist(fit1$residuals)

plot(fit1)

library("lmtest")
dwtest(fit1)

actual <- mtcars$mpg
preds <- mtcars.lm$fitted.values
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

fit <- lm(mpg ~ wt + transmission + log.disp + log.carb + log.hp, df_cars)
data.frame(predict(fit, interval="confidence")) %>%
    mutate(observed = df_cars$mpg) %>% 
    mutate(in_ci = ifelse(observed >= lwr & observed <= upr,TRUE,FALSE)) %>% 
    summarize(`% in CI` = round(mean(in_ci)*100,2))
dwtest(fit)


lm_table %>% slice_max(AR2, n=6)
