# Load necessary libraries
install.packages("mice")
install.packages("dplyr")
install.packages("multiUS")
library(mice)
library(dplyr)
library(multiUS)

# Step 1: Import data
custdata5 <- read.csv(file.choose(), header = TRUE, sep = ";")

# Step 2: Explore missing data and dataset structure
summary(custdata5)            # Summary of the dataset
str(custdata5)                # Check the structure of the dataset
library(mice)
md.pattern(custdata5)         # Visualize missing data patterns

# Identify rows with missing data
missing_data_rows <- custdata5[!complete.cases(custdata5),]
print(missing_data_rows)

# Step 3: Visualize the distribution of numerical variables
par(mfrow = c(1, 3))
hist(custdata5$num.vehicles, main = "Distribution of num.vehicles")
hist(custdata5$age, main = "Distribution of age")
hist(custdata5$Yearly.Income, main = "Distribution of Yearly.Income")

# Step 4: Handle missing data
# Convert `num.vehicles` to a factor for imputing mode
library(dplyr)
custdata5 <- custdata5 %>%
mutate(num.vehicles = as.factor(num.vehicles))

# Find the mode of `num.vehicles`
num.vehicles.freq <- table(custdata5$num.vehicles)
mode.num.vehicles <- names(num.vehicles.freq)[which.max(num.vehicles.freq)]
print(mode.num.vehicles)


# Replace missing `num.vehicles` with mode
custdata5$num.vehicles.fix <- ifelse(is.na(custdata5$num.vehicles), mode.num.vehicles, custdata5$num.vehicles)

# Replace missing `age` with median
median.age <- median(custdata5$age, na.rm = TRUE)
custdata5$age.fix <- ifelse(is.na(custdata5$age), median.age, custdata5$age)

# Visualize original and imputed distributions for `age`
par(mfrow = c(1, 2))
hist(custdata5$age, main = "Original Age Distribution")
hist(custdata5$age.fix, main = "Imputed Age Distribution")

# Replace missing `Yearly.Income` with median
median.Yearly.Income <- median(custdata5$Yearly.Income, na.rm = TRUE)
custdata5$Yearly.Income.fix <- ifelse(is.na(custdata5$Yearly.Income), median.Yearly.Income, custdata5$Yearly.Income)

# Visualize original and imputed distributions for `Yearly.Income`
par(mfrow = c(1, 2))
hist(custdata5$Yearly.Income, main = "Original Yearly.Income Distribution")
hist(custdata5$Yearly.Income.fix, main = "Imputed Yearly.Income Distribution")
str(custdata5)
# Step 5: Prepare complete dataset
custdata5.lengkap <- custdata5[, -c(9, 10, 13)]  # Drop original columns with missing values

str(custdata5.lengkap)

# Check missing data in the updated dataset
md.pattern(custdata5.lengkap)

# Step 6: Format categorical data for imputation

custdata5.lengkapv2 <- custdata5.lengkap %>%
  mutate(is.employed.fix1 = na_if(is.employed.fix1, "missing"))

custdata5.lengkap_v2 <- custdata5.lengkap %>%
  mutate(is.employed.fix1 = na_if(is.employed.fix1, "missing"),
         is.employed = as.factor(is.employed),
         is.employed.fix1 = as.factor(is.employed.fix1))

# Step 7: Perform multiple imputations
init <- mice(custdata5.lengkap, maxit = 0)
meth <- init$method
predM <- init$predictorMatrix

# Set imputation methods
meth[c("is.employed")] <- "logreg"
meth[c("is.employed.fix1")] <- "logreg"

# Run imputations
imputed_custdata5 <- mice(custdata5.lengkap, method = meth, predictorMatrix = predM)

# Extract the completed dataset
complete_custdata5 <- complete(imputed_custdata5)

# Final checks
md.pattern(complete_custdata5)
str(complete_custdata5)
