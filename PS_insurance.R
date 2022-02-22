library(ggplot2)
library(dplyr)
library(cowplot)
library(Hmisc)
library(WVPlots)
library(lessR)
library(RColorBrewer)
# install.packages("lessR")

data1 = read.csv("C:/Users/Pruthvi/Downloads/insurance.csv")
View(data1)

str(data1)
summary(data1)
sum(is.na(data1))



#### EDA

# Distribution of smoker, children and region

# smokerperc <- data1 %>% 
#   group_by(smoker) %>% 
#   count() %>% 
#   ungroup() %>%
#   mutate(perc = smoker / sum(smoker)) %>%
#   arrange(perc) %>%
#   mutate(labels = scales::percent(perc))

# ggplot(data1, aes(x = "", y = smoker, fill = smoker)) +
#   geom_bar(stat = "identity", width = 1) +
#   coord_polar("y", start = 0) +
#   theme_void() + 
#   theme() +

coul <- brewer.pal(5, "Paired")

par(mfrow = c(1, 3))
smoker_table <- table(data1$smoker)
# par(mar=c(1, 1, 1, 1))
pie(smoker_table, main = "smoker", radius = 3.5, col = coul, 
    labels = paste0(round(100 * smoker_table/sum(smoker_table), 2), "%"))
legend("bottomleft", legend = c("No", "Yes"),
       fill = coul)

children_tb <- table(data1$children)
# par(mar=c(1, 1, 1, 1))
pie(children_tb, main = "Children", radius = 3.5, col = coul, 
    labels = paste0(round(100 * children_tb/sum(children_tb), 2), "%"))
legend("bottomleft", legend = c("0", "1", "2", "3", "4", "5"),
       fill = coul)


ggplot(data1, aes(x = chilren)) +
  geom_histogram(bins = 30, binwidth = 1)

region_table <- table(data1$region)
pie(region_table, main = "Region", radius = 3.5, col = coul, 
    labels = paste0(round(100 * region_table/sum(region_table), 2), "%"))
legend("bottomleft", legend = c("northeast", "northwest", "southeast", "southwest"),
       fill = coul)
view(region_table)

x <- ggplot(data1, aes(age, charges)) +
  geom_jitter(color = "purple", alpha = 0.5) +
  theme_update()

x

y <- ggplot(data1, aes(bmi, charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_update()

y


z <- ggplot(data1, aes(smoker, charges)) +
  geom_jitter(color = "red", alpha = 0.5) +
  theme_update()

c <- ggplot(data1, aes(children, charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_update()


p <- plot_grid(x, y, z, c) 
title <- ggdraw() + draw_label("1. Correlation between Charges and Age / BMI / Smoker / children", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


x1 <- ggplot(data1, aes(sex, charges)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  theme_light()

x1


y1 <- ggplot(data1, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()
y1



p <- plot_grid(x1, y1) 
title <- ggdraw() + draw_label("2. Correlation between Charges and Sex / Children covered by insurance", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


x2 <- ggplot(data1, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  theme_light()
x2


y2 <- ggplot(data1, aes(region, charges)) + 
  geom_jitter(aes(color = region), alpha = 0.7) + 
  theme_light()
y2


p <- plot_grid(x2, y2)
title <- ggdraw() + draw_label("3. Correlation between Charges and Smoker / Region", fontface='bold')
plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))


## Linear Regression Model
### Preparation and splitting the data
# ```{r prep, message=FALSE, warning=FALSE, paged.print=TRUE}
n_train <- round(0.8 * nrow(data1))
train_indices <- sample(1:nrow(data1), n_train)
Data_train <- data1[train_indices, ]
Data_test <- data1[-train_indices, ]

formula_0 <- as.formula("charges ~ age + sex + bmi + children + smoker + region")



### Train and Test the Model
# ```{r model_0, message=FALSE, warning=FALSE, paged.print=TRUE}
model_0 <- lm(formula_0, data = Data_train)
summary(model_0)
#Saving R-squared
r_sq_0 <- summary(model_0)$r.squared


#predict data on test set
prediction_0 <- predict(model_0, newdata = Data_test)
#calculating the residuals
residuals_0 <- Data_test$charges - prediction_0
#calculating Root Mean Squared Error
rmse_0 <- sqrt(mean(residuals_0^2))

View(cbind(Data_test, prediction_0, (Data_test$charges - prediction_0)))

#Using new formula

formula_1 <- as.formula("charges ~ age + bmi + smoker + children")


model_1 <- lm(formula_1, data = Data_train)

summary(model_1)

# r_sq_1 <- summary(model_1)$r
r_sq_1 <- summary(model_1)$r.squared


prediction_1 <- predict(model_1, newdata = Data_test)
residuals_1 <- Data_test$charges - prediction_1
rmse_1 <- sqrt(mean(residuals_1^2))


print(paste0("R-squared for first model: ", round(r_sq_0, 4)))
print(paste0("R-squared for new model: ", round(r_sq_1, 4)))
print(paste0("RMSE for first model: ", round(rmse_0, 4)))
print(paste0("RMSE for new model: ", round(rmse_1, 4)))

### Model Performance

Data_test$prediction <- predict(model_1, newdata = Data_test)
View(Data_test)

ggplot(Data_test, aes(x = prediction, y = charges)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(color = "red") +
  ggtitle("Prediction vs Actual Values")

Data_test$residuals <- Data_test$charges - Data_test$prediction

ggplot(Data_test, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "orangered", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "blue") +
  ggtitle("Residuals vs. Linear model prediction") + 
  theme_linedraw()


ggplot(Data_test, aes(x = residuals)) +
  geom_histogram(bins = 15, fill = 'blue') +
  ggtitle("Histogram of residuals")


GainCurvePlot(Data_test, "prediction", "charges", "Model")


bob <- data.frame(age = 23.7, bmi = 27.7, children = 0, smoker = "no", region = "southwest")

print(paste0("Health care charges for Bob: ", round(predict(model_1, bob), 2)))

ash <- data.frame(age = 23.6, 
                  bmi = 25.4, 
                  children = 0, 
                  smoker = "yes", 
                  region = "southwest")
print(paste0("Health care charges for Ash: ", 
             round(predict(model_1, ash), 2)))


eren <- data.frame(age = 29.6, 
                  bmi = 35.4, 
                  children = 0, 
                  smoker = "no", 
                  region = "southwest")
print(paste0("Health care charges for Ash: ", 
             round(predict(model_1, eren), 2)))


lm1 = lm(data = data1, charges~.)
lm1
summary(lm1)

View(cbind(data1$charges, as.data.frame(lm1$fitted.values)))



cor(data1$bmi, data1$charges,  method = c("pearson", "kendall", "spearman"))

