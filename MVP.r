library(ggplot2)
library(dplyr)
library(MASS)

#Loading and summarizing original data
original_data <- read.csv('nba_2022_2023.csv')
summary(original_data)

#Removing players who played less than 60 games or less than 30mpg
mvp_data <- subset(original_data, G >= 60 & MP >= 30)
summary(mvp_data)

#Convert stats to per 36 minute values
mvp_data$PTS_36 <- (mvp_data$PTS / mvp_data$MP) * 36
mvp_data$AST_36 <- (mvp_data$AST / mvp_data$MP) * 36
mvp_data$TRB_36 <- (mvp_data$TRB / mvp_data$MP) * 36
mvp_data$STL_36 <- (mvp_data$STL / mvp_data$MP) * 36
mvp_data$BLK_36 <- (mvp_data$BLK / mvp_data$MP) * 36
mvp_data$TOV_36 <- (mvp_data$TOV / mvp_data$MP) * 36

#Remove unnecessary columns
mvp_data <- subset(mvp_data, select = -c(PTS, AST, TRB, ORB, DRB, STL, BLK, TOV, ORB, PF, Age, GS, Tm))
head(mvp_data)

#Creating custom weights by position
position_weights <- data.frame(
  position = c("PG", "SG", "SF", "PF", "C"),
  weight_PTS = c(50, 50, 50, 50, 50),
  weight_AST = c(15, 18, 20, 23, 25),
  weight_TRB = c(20, 22, 20, 17, 15),
  weight_STL = c(8, 9, 10, 11, 12),
  weight_BLK = c(7, 6, 5, 4, 3),
  weight_TOV = c(-10, -15, -15, -15, -15)
)

#Merge the dataset with the lookup table based on the "Pos" column
mvp_data <- merge(mvp_data, position_weights, by.x = "Pos", by.y = "position", all.x = TRUE)

#Create a new column for the weighted total
mvp_data$weighted_total <- ((mvp_data$PTS_36 * mvp_data$weight_PTS) +
  (mvp_data$AST_36 * mvp_data$weight_AST) +
  (mvp_data$TRB_36 * mvp_data$weight_TRB) +
  (mvp_data$STL_36 * mvp_data$weight_STL) +
  (mvp_data$BLK_36 * mvp_data$weight_BLK) +
  (mvp_data$TOV_36 * mvp_data$weight_TOV)) / 100

#Order the players by highest total
ordered_mvp_data <- mvp_data[order(mvp_data$weighted_total, decreasing = TRUE),]

#First linear model
linear_model <- lm(formula = weighted_total ~ PTS_36 + AST_36 + TRB_36 + STL_36
                   + BLK_36 + TOV_36, data = ordered_mvp_data)
summary(linear_model)

#Checking to see if all variables were necessary
First <- lm(weighted_total~1, data = ordered_mvp_data)
All <- lm(weighted_total ~ PTS_36 + AST_36 + TRB_36 + STL_36 + BLK_36 + TOV_36, data = ordered_mvp_data)
step(First, direction = "forward", scope = formula(All))
step(All, direction = "backward", scope = formula(First))
step(First, direction = "both", scope = formula(All))

AIC(linear_model)
BIC(linear_model)

#Extract residuals from the linear model
residuals <- residuals(linear_model)

#Create a Q-Q plot of the first model residuals
qqnorm(residuals)
qqline(residuals, col = "red")

#Plotting first model residuals
par(mfrow = c(1, 2))
plot(fitted(linear_model), resid(linear_model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted Vs Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
qqnorm(resid(linear_model), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(linear_model), col = "dodgerblue", lwd = 2)

#Scatter plot of each stat in the original model. I ran it for each by
#replacing the variables in the code below
ggplot(ordered_mvp_data, aes(x = TOV_36, y = weighted_total)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot with Regression Line",
       x = "TOV_36",
       y = "Weighted Total")

#Box Cox plot on entire model
linear_model = lm(weighted_total ~ PTS_36 + AST_36 + TRB_36 + STL_36 + BLK_36 +
                    TOV_36, data = ordered_mvp_data)
boxcox(Fit1.linear_model, plotit = TRUE)

#Box Cox plot on each predictor variable. I replaced the variable each time
#and reran the lines
linear_model = lm(weighted_total ~ AST_36, data = ordered_mvp_data)
boxcox(Fit1.linear_model, plotit = TRUE)

#Transformed values based on Box Cox results
ordered_mvp_data$PTS_36_transformed <- ((ordered_mvp_data$PTS_36^0.2) - 1) / 0.2
ordered_mvp_data$AST_36_transformed <- ((ordered_mvp_data$AST_36^0.5) - 1) / 0.5
ordered_mvp_data$TRB_36_transformed <- ((ordered_mvp_data$TRB_36^0.5) - 1) / 0.5
ordered_mvp_data$STL_36_transformed <- ((ordered_mvp_data$STL_36^0.4) - 1) / 0.4
ordered_mvp_data$BLK_36_transformed <- ((ordered_mvp_data$BLK_36^0.3) - 1) / 0.3
ordered_mvp_data$TOV_36_transformed <- ((ordered_mvp_data$TOV_36^0.6) - 1) / 0.6

#Second linear model
Fit2.linear_model = lm(weighted_total ~ PTS_36 + AST_36_transformed + TRB_36_transformed + STL_36_transformed + BLK_36_transformed + TOV_36_transformed, data = ordered_mvp_data)
boxcox(Fit2.linear_model, plotit = TRUE)

#Extract residuals from the second linear model
residuals2 <- residuals(Fit2.linear_model)

#Plotting second model residuals
plot(fitted(Fit2.linear_model), resid(Fit2.linear_model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted Vs Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
qqnorm(resid(Fit2.linear_model), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(Fit2.linear_model), col = "dodgerblue", lwd = 2)

#Scatter plot of each stat in the second model. I ran it for each by
#replacing the variables in the code below
ggplot(ordered_mvp_data, aes(x = BLK_36_transformed, y = weighted_total)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot with Regression Line",
       x = "BLK_36_transformed",
       y = "Weighted Total")

#Making sure each variable was still necessary after transformations
First <- lm(weighted_total~1, data = ordered_mvp_data)
All <- lm(weighted_total ~ PTS_36 + AST_36_transformed + TRB_36_transformed + STL_36_transformed + BLK_36_transformed + TOV_36_transformed, data = ordered_mvp_data)
step(First, direction = "forward", scope = formula(All))
step(All, direction = "backward", scope = formula(First))
step(First, direction = "both", scope = formula(All))

#Printing summary of second model
summary(Fit2.linear_model)

#Printing model results for MVP
head(ordered_mvp_data[c("Player", "weighted_total")])
