library(ggplot2)
View(diamonds)

??diamonds

set.seed(1)
ind <- sample(2, nrow(diamonds), replace=TRUE, prob=c(0.8,0.2))

# TRAIN AND TEST DATA
train.data <- diamonds[ind==1,]
test.data <- diamonds[ind==2,]

dim(train.data)
dim(test.data)

myFormula <- price ~ .


# Model
# Utilizando todos os dados.

model <- lm(myFormula, data = train.data)
print(model)
summary(model)

plot(model)

predicted <- predict(model, test.data)
final_data <- cbind(Actual = test.data$price,
                    predicted = predicted)
Final_Data <- as.data.frame(final_data)
View(Final_Data)

model$coefficients



# Finding Error

error <- (Final_Data$Actual - 
            Final_Data$predicted)
Final_Data <- cbind(Final_Data, error)
View(Final_Data)

rmse <- sqrt(mean(Final_Data$error^2))
rmse

# MODEL USING TWO VARIABLES

model2 <- lm(diamonds$price ~ diamonds$carat)
summary(model2)


# PLOT
plot(Final_Data$Actual, Final_Data$predicted)