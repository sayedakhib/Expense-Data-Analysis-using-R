#install the required packages
install.packages(c("ggplot2", "dplyr"))

# Load required libraries
library(dplyr)
library(ggplot2)

# Load historical stock price data
data <- read.csv("Inc_Exp_Data.csv")
print(data)
str(data)

# Descriptive statistics
IQR<- IQR(data$Mthly_HH_Expense,na.rm = TRUE)
var<- var(data$Mthly_HH_Expense,na.rm = TRUE)
sd<- sd(data$Mthly_HH_Expense,na.rm = TRUE)

print(paste("IQR of Monthly Expence:", IQR))
print(paste("Variance of Monthly Expence:", var))
print(paste("Standard Deviation of Monthly Expence:", sd))

# Covariance and correlation
cov_matrix <- cov(select(data, Mthly_HH_Income, Mthly_HH_Expense,Annual_HH_Income))
print(cov_matrix)

#summarise
Avg_Annual_Income<- data %>% summarise(AvgAnualIncome = mean(Annual_HH_Income, na.rm = TRUE))
print(Avg_Annual_Income)

#filter
Anual_values<-data%>%filter(Annual_HH_Income>625500)
print(Anual_values)

# Create a linear regression model
model<-lm(Annual_HH_Income~Mthly_HH_Expense+Mthly_HH_Income,data=data)

# Print a summary of the model
summary(model)

#make Prediction
new_data<-data.frame(Mthly_HH_Income=54500,Mthly_HH_Expense=36000)
Annual_HH_Income<-predict(model,new_data)
print(Annual_HH_Income)

# Plot the linear regression model
plot(data$Mthly_HH_Income,data$Mthly_HH_Expense, main = "Linear Regression Model",
     xlab = "Monthly Income", ylab = "Monthly Expence", col = "blue")
abline(lm(data$Mthly_HH_Expense ~ data$Mthly_HH_Income), col = "red")

# Line graph
ggplot(data, aes(x = seq_along(Mthly_HH_Income), y = Mthly_HH_Income)) +
  geom_line() +
  labs(title = "Monthly Household Income Over Time", x = "Record Number"
       , y = "Monthly Household Income")

# Assuming you want to visualize the distribution of Highest_Qualified_Member
qualification_distribution <-data %>% 
  group_by(Highest_Qualified_Member) %>%
  summarise(count = n())
#pie chart
ggplot(qualification_distribution, aes(x = "", y = count, fill = Highest_Qualified_Member)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Distribution of Highest Qualified Member")


# Bar graph
ggplot(data, aes(x = Highest_Qualified_Member)) +
  geom_bar() +
  labs(title = "Distribution of Highest Qualified Member", x = "Highest Qualified Member",
       y = "Count")

# Histogram
ggplot(data, aes(x = Annual_HH_Income)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Annual Household Income Distribution", x = "Annual Household Income",
       y = "Frequency")

# Scatter plots
#Scatter Plot of Monthly vs. Annual Household Income
ggplot(data, aes(x = Mthly_HH_Income, y = Annual_HH_Income, color = Highest_Qualified_Member)) +
  geom_point() +
  labs(title = "Scatter Plot of Monthly vs. Annual Household Income",
       x = "Monthly Household Income", y = "Annual Household Income")

#Scatter Plot of Monthly Household Income and Expense
ggplot(data, aes(x = Mthly_HH_Expense, y = Mthly_HH_Income, color = No_of_Fly_Members)) +
  geom_point() +
  labs(title = "Scatter Plot of Monthly Household Income and Expense",
       x = "Monthly Household Expense",
       y = "Monthly Household Income",
       color = "Number of Family Members")





