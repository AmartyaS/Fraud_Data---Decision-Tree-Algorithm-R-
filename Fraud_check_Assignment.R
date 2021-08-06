# Installing the required packages 
install.packages("C50", dependencies = T)
library("C50")
install.packages("caret")
library("caret")
install.packages("psych")
library("psych")
install.packages("fastDummies")
library("fastDummies")

# Reading the dataset
data <- read.csv(file.choose())
View(data)
attach(data)

# Exploring the dataset
colnames(data)
str(data)
data$Undergrad <- ifelse(data$Undergrad=='YES',1,0)
data <- fastDummies::dummy_cols(data,select_columns = "Marital.Status",remove_selected_columns = TRUE)
data$Urban <- ifelse(data$Urban=='YES',1,0)
data$Taxable.Income <- ifelse(data$Taxable.Income<=30000,"Risky","Good")
data$Taxable.Income <- as.factor(data$Taxable.Income)
View(data)

# Making training and testing dataset
ind <- sample(2, nrow(data),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- data[ind==1,]
testing <- data[ind==2,]
View(training)

# Decision Tree Model Making
model <- C5.0(training$Taxable.Income~.,data = training,trails=1000)
summary(model)
pred <- predict.C5.0(model,training[,-2])
pred
pred1 <- predict.C5.0(model,testing[,-2])
a <- table(training$Taxable.Income,pred)
b <- table(testing$Taxable.Income,pred1)
a
b
sum(diag(a)/sum(a))      # Training Accuracy
sum(diag(b)/sum(b))      # Testing Accuracy

plot(model)
pairs.panels(data)
