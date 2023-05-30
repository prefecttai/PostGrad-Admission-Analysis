# Removes the scientific notation on the labels and gives whole numbers
options(scipen = 999)
# Read Original Data Set
raw = read.csv("C:/Users/danie/Desktop/CS 555 Found of ML/Term Project/adm_data.csv")
# Clean Data
data = raw[-c(1)]
# Use cor() function to determine the correlation coefficient between each factor individually against the Chances of admittance
cor(data)
pairs(data)
#
# Calculate Least Squares Regression Equation 
# Build a Multiple Linear Regression(MLR) model then run summary() on the model
model1 = lm(data$Chance.of.Admit ~ data$GRE.Score + data$TOEFL.Score + data$University.Rating + data$SOP + data$LOR + data$CGPA + data$Research)
summary(model1)
#
# Calculate R^2
totalss = sum((data$Chance.of.Admit - mean(data$Chance.of.Admit))^2)
regss = sum((fitted(model1) - mean(data$Chance.of.Admit))^2)
resiss = sum((data$Chance.of.Admit - fitted(model1))^2)
R2 = regss / totalss
#
# Run Global F-Test, use qf() to determine decision rule at alpha = 0.01
# We do a minus 1 in the ncol(data) to remove the observed/dependent variable $Admission Chance
qf(0.99, df1 = ncol(data) - 1, df2 = nrow(data) - (ncol(data) - 1) - 1)
#
# Run inference t-test for each parameter using qt() to determine decision rule at alpha = 0.05
qt(0.975, df = nrow(data) - 1)
#
# Calculate the 95% confidence intervals for variables of which the null hypothesis we rejected
model2 = lm(data$Chance.of.Admit ~ data$GRE.Score + data$TOEFL.Score + data$University.Rating + data$SOP + data$LOR + data$CGPA + data$Research)
confint(model2, level = 0.95)
#
# Hist() model residual to confirm Normal Q-Q plot
hist(residuals(model2), main = "Model Residuals to confirm Normality", xlab = "Residuals")
#
# Check if MLR satisfies the assumptions
plot(model2)