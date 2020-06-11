##RandomForest
setwd("P:\\Spring 2020\\ML\\Mid-term")
phneprices <- read.csv("phoneprices.csv", header=T, sep=",")
head(phneprices)

# Inspect the number of observables, features and data types in the dataset
str(phneprices)

# Getting set of descriptive statistics, depending on the type of variable including mean/median, S.D./IQR
# In case of a Numerical Variable -> Gives Mean, Median, Mode, Range and Quartiles.
# In case of a Factor Variable -> Gives a table with the frequencies
summary(phneprices)

#The stat.desc function provides the total n, number of null values, number of na values, min, max, range, sum, median, mean, SE of the mean, 
#95% CI of the mean, var, standard deviation, and coeff of var
#install.packages("pastecs")
library(pastecs)
round(stat.desc(phneprices),3)


#Determining the correlation of the independent features with the dependent feature(phone price)
library(corrplot)
corr = round(cor(phneprices), 8)
corrplot(corr)
h = hist(phneprices$price_range)
library(ggplot2)
ggplot(phneprices, aes(price_range)) + geom_bar(aes(fill=price_range))
ggplot(phneprices, aes(price_range)) + geom_density(aes(fill=price_range)) + xlab("Phone Price Range)") +  ylab("Density")
ggplot(phneprices, aes(price_range)) + stat_function(fun = dnorm) + xlab("Phone Price Range") +  ylab("Density")


# Converting few features to factors
phneprices$blue <- as.factor(phneprices$blue)
phneprices$dual_sim <- as.factor(phneprices$dual_sim)
phneprices$four_g <- as.factor(phneprices$four_g)
phneprices$price_range <- as.factor(phneprices$price_range)



# Subplots using filtered dataset
#Boxplots of the 4 important features identified in the previous step i.e.Battery_power, PX_height, PX_Width, and RAM
library(ggplot2)
ggplot(phneprices, aes(x=price_range, y = battery_power, color=price_range)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  labs(title = "Battery Power vs Price Range")
ggplot(phneprices, aes(x=price_range, y = px_height, color=price_range)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  labs(title = "Pixel resolution height vs Price Range")
ggplot(phneprices, aes(x=price_range, y = px_width, color=price_range)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  labs(title = "Pixel resolution width vs Price Range")
ggplot(phneprices, aes(x=price_range, y = ram, color=price_range)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  labs(title = "RAM vs Price Range")

#Dividing the dataset into a Train and Test datset in the ratio 80:20 respectively
population <- sample(nrow(phneprices), 0.8*nrow(phneprices))
train <- phneprices[population, ]
test <- phneprices[-population, ]

#Running the Random Forest algorithm on the data
library(randomForest)
model <- randomForest(price_range~., data=train)
model
prediction <- predict(model, newdata=test)
prediction
accuracy <- mean(prediction == test$price_range)
accuracy

model1 <- randomForest(price_range~ram + battery_power + px_height + px_width, data=train)
model1
prediction1 <- predict(model1, newdata=test)
prediction1
accuracy1 <- mean(prediction1 == test$price_range)
accuracy1

num_iterations <- 100
acc_history <- list(num_iterations)
acc_history_1 <- list(num_iterations)
library(caret)
library(randomForest)

for (i in 1:num_iterations) {
  inTrain = createDataPartition(y=phneprices$price_range, p=0.8, list=FALSE)
  X_train = phneprices[inTrain, ]
  X_test = phneprices[-inTrain, ]
  model <- randomForest(price_range~ram + battery_power + px_height + px_width, data=X_train)
  model1 <- randomForest(price_range~ram, data=X_train)
  prediction <- predict(model, newdata=X_test)
  prediction1 <- predict(model1, newdata=X_test)
  accuracy <- mean(prediction == X_test$price_range)
  accuracy1 <- mean(prediction1 == X_test$price_range)
  acc_history[[i]] <- accuracy
  acc_history_1[[i]] <- accuracy1
}
for (i in 1:num_iterations) {
  print(acc_history[[i]])
}
sum_acc = 0
for (i in 1:num_iterations) {
  sum_acc = sum_acc + acc_history[[i]]
}
ave_acc = sum_acc/num_iterations
print(ave_acc)

df1 <- data.frame(matrix(unlist(acc_history), nrow=length(acc_history), byrow=T))
df1$group = 1
df1$i <- seq.int(nrow(df1))
names(df1)[1] <- "accuracy"
df2 <- data.frame(matrix(unlist(acc_history_1), nrow=length(acc_history_1), byrow=T))
df2$group = 0
df2$i <- seq.int(nrow(df2))
names(df2)[1] <- "accuracy"

#Testing which model is significant between the baseline model with RAM as the only feature and the overall model
df3 <- rbind(df1, df2)
ggplot(data = df3, aes(x=i, y=accuracy, color=factor(group))) + xlab("round of test") +
  ylab("accuracy score") + geom_point() + labs(color="group")
t.test(accuracy~group, data=df3)