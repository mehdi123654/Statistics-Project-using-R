# Load necessary libraries
library(tidyverse)
library(scales) 
install.packages("reshape2")
library(reshape2)
install.packages("caret")
library(caret)
install.packages("RANN")
library(RANN)

#---------------------------- Import the data -----------------------------
dataOriginal <- read.csv(file=file.choose(),header=T,sep=';')
str(dataOriginal)
summary(dataOriginal)
data = dataOriginal
str(data)
data <- subset(data, select = -c(ID))  # Remove the "ID" column
str(data)

attach(data)

#=========== Part 1: Preprocessing and data cleaning ================

####################### 1. Study of outliers #########################

# Use boxplots for each numeric variable
# Select numeric variables for the boxplot
numeric_vars <- c("Customer_care_calls", "Customer_rating", "Cost_of_the_Product",
                  "Prior_purchases", "Discount_offered", "Weight_in_gms")

# Set layout for multiple plots
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

# Create a boxplot for each numeric variable
for (var in numeric_vars) {
  boxplot(data[[var]],
          main = paste("Boxplot of", var),
          col = "lightblue",
          border = "black",
          notch = TRUE,
          notchwidth = 0.6,
          ylab = "Values")
}

# Reset the default graph layout
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

#---- We obtained three quantitative variables with outliers ---
#---- We will now handle these outliers ---
#---- By replacing the values with NA ----

#### Cost_of_the_Product 
summary(data$Cost_of_the_Product)
Q1 = 170
Q3 = 251
Vmax = Q3 + 1.5 * (Q3 - Q1)
Vmin = Q1 - 1.5 * (Q3 - Q1)
outliers <- which(data$Cost_of_the_Product < Vmin | data$Cost_of_the_Product > Vmax)
outlier_percentage = length(outliers) * 100 / 10999
# 0.02% => Replace with NA
data$Cost_of_the_Product[outliers] = NA 
boxplot(data$Cost_of_the_Product)

#### Prior_purchases 
summary(data$Prior_purchases)
Q1 = 3
Q3 = 4
Vmax = Q3 + 1.5 * (Q3 - Q1)
Vmin = Q1 - 1.5 * (Q3 - Q1)
outliers <- which(data$Prior_purchases < Vmin | data$Prior_purchases > Vmax)
outlier_percentage = length(outliers) * 100 / 10999
# 9% => Replace with NA
data$Prior_purchases[outliers] = NA
boxplot(data$Prior_purchases)

#### Discount_offered 
summary(data$Discount_offered)
dataSupp = data
Q1 = 4
Q3 = 8
Vmax = Q3 + 1.5 * (Q3 - Q1)
Vmin = Q1 - 1.5 * (Q3 - Q1)
outliers <- which(dataSupp$Discount_offered < Vmin | data$Discount_offered > Vmax)
outlier_percentage = length(outliers) * 100 / 10999
# 22% => Replace with NA
dataSupp$Discount_offered[outliers] = NA
boxplot(dataSupp$Discount_offered)

####################### 2. Study of missing values #######################
missing_values <- sapply(dataSupp, function(x) sum(is.na(x)))
#--------- Visualizing the missing value rate per variable ---------------
# Calculate the percentage of missing values per variable
missing_percentage <- colMeans(is.na(data)) * 100

# Create a bar plot with clearer colors and labels
barplot(missing_percentage,
        col = "skyblue",            # Bar color
        border = "black",           # Border color
        ylim = c(0, 20),            # Y-axis range
        main = "Percentage of missing values per variable",
        ylab = "Percentage",
        xlab = "Variables",
        cex.names = 0.7,            # Label size
        las = 2)                    # Orientation of x-axis labels

# Add explanatory text above each bar
text(1:length(missing_percentage), missing_percentage + 2, 
              paste0(round(missing_percentage, 2), "%"),
              col = "black", pos = 3, cex = 0.7)

# Legend
legend("topright", legend = "Percentage of missing values", fill = "skyblue", border = "black")

#------------ Number of rows with missing values --------------------------
rows_with_null <- sum(is.na(data))
# Number of rows with missing values: 

# Rate of rows with missing values (in %)
percent_empty = rows_with_null * 100 / dim(data) 
# Percentage of rows affected by missing values: 
# 10% >>>> 5%, so we cannot delete the rows

#------------ Imputation of missing values ------------------------------------
# data_without_null <- na.omit(data) -- if percentage is << 5%

# We have several imputation options
#--------a. KNN Imputation
# Apply the imputation
library(VIM)
dataImp = kNN(data)
str(dataImp)
dataImp <- subset(dataImp, select = -c(Reached.on.Time_Y.N_imp, Weight_in_gms_imp, Discount_offered_imp, Gender_imp, Product_importance_imp, Prior_purchases_imp, Cost_of_the_Product_imp, Customer_rating_imp, Customer_care_calls_imp, Mode_of_Shipment_imp, Warehouse_block_imp, ID_imp))
str(dataImp)

#--------b. Mean Imputation for the variable 'Cost_of_the_Product'
# data$Cost_of_the_Product[is.na(data$Cost_of_the_Product)] <- mean(data$Cost_of_the_Product, na.rm = TRUE)

#--------c. Median Imputation for the variable 'Cost_of_the_Product'
# data$Cost_of_the_Product[is.na(data$Cost_of_the_Product)] <- median(data$Cost_of_the_Product, na.rm = TRUE)

#--------d. Mode Imputation for the variable 'Cost_of_the_Product'
# library(modeest)
# data$Cost_of_the_Product[is.na(data$Cost_of_the_Product)] <- mfv(data$Cost_of_the_Product)

#--------e. Multiple Imputation
# library(mice)
# imputed_data <- mice(ecommerce_data, m=5, maxit=50, method='pmm', seed=500)
# completed_data <- complete(imputed_data)

# In our case, we will use KNN imputation
# It uses the closest observed values to estimate missing values
# We try to maintain the local structures in the data
rows_with_null <- sum(is.na(dataImp))
percent_empty = rows_with_null * 100 / dim(dataImp)
missing_values <- sapply(dataImp, function(x) sum(is.na(x)))

# 4. Handling Missing Values by Removing Rows
dataImp <- na.omit(dataImp)

####################### 3. Encoding ###########################################################

########## Ordinal Customer_rating (already numeric)
########## Ordinal Product_importance (high, medium, low) 
dataEncod = dataImp
View(dataEncod)
# Ordinal ranking for 'Product_importance'
levels_order <- c("low", "medium", "high")  
# Convert the variable to an ordered factor
dataEncod$Product_importance <- factor(dataEncod$Product_importance, levels = levels_order, ordered = TRUE)
# Convert the ordered factor to numeric
dataEncod$Product_importance <- as.numeric(dataEncod$Product_importance)
# Display the first rows to verify
head(dataEncod$Product_importance)
str(dataEncod)
missing_values <- sapply(dataEncod, function(x) sum(is.na(x)))

########## Encoding of nominal variables
########################### Variable Mode_of_Shipment (FLIGHT, SHIP, ROAD)
dataEncod$Mode_of_Shipment <- factor(dataEncod$Mode_of_Shipment, levels = c("Flight", "Ship", "Road"))
dataEncod$Mode_of_Shipment <- as.numeric(dataEncod$Mode_of_Shipment)
str(dataEncod)

########################### Gender (M, F)
dataEncod$Gender <- factor(dataEncod$Gender, levels = c("M", "F"))
dataEncod$Gender <- as.numeric(dataEncod$Gender)
str(dataEncod)

########################### Warehouse_block (A, B, C, D, F)
dataEncod$Warehouse_block <- factor(dataEncod$Warehouse_block, levels = c("A", "B", "C", "D", "F"))
dataEncod$Warehouse_block <- as.numeric(dataEncod$Warehouse_block)
str(dataEncod)
head(dataEncod$Warehouse_block, 10)
View(dataEncod)

####################### 4. Data normalization ############################
dataStand = dataEncod
mins = apply(dataStand, MARGIN = 2, FUN = min)
maxs = apply(dataStand, MARGIN = 2, FUN = max)
dataStand = scale(dataStand, center = mins, scale = maxs - mins)
summary(dataStand)
View(dataStand)
str(dataStand)


##########################################################################################################

ecommerce_data <-dataImp

# ------------------------- Part 2: Exploratory Data Analysis -----------------------------------

# Load necessary libraries
library(ggplot2)      # For creating plots
library(tseries)      # For statistical tests on time series
library(gridExtra)    # For organizing plot layouts

# 1. Analysis of Quantitative Variables

# a) Descriptive Statistics
# Descriptive statistics for quantitative variables
# Identify the quantitative variables
impdata=subset(com, select=c("Warehouse_block", "Mode_of_Shipment", "Customer_care_calls", "Customer_rating", "Cost_of_the_Product", "Gender", "Discount_offered", "Weight_in_gms", "Reached.on.Time_Y.N"))

# Remove the "Reached.on.Time_Y.N" variable from quantitative variables
quantitative_vars <- sapply(impdata, is.numeric)
quantitative_vars["Reached.on.Time_Y.N"] = FALSE

# Display descriptive statistics for each variable
for (var in names(ecommerce_data[quantitative_vars])) {
  print(paste("Descriptive statistics for", var, ":"))
  print(summary(ecommerce_data[[var]]))
}

# Display frequency tables for the categorical variable "Reached.on.Time_Y.N"
print("Frequency table for Reached.on.Time_Y.N:")
print(data.frame(table(ecommerce_data$Reached.on.Time_Y.N)))
# For each quantitative variable, the descriptive statistics (min, 1st quartile, median, mean, 3rd quartile, max) are displayed.

# b) Histograms and Bar Charts
# Histograms for continuous variables and bar charts for discrete variables

# Specify the number of rows and columns for the grid layout
num_rows <- 2
num_cols <- ceiling(length(names(ecommerce_data[quantitative_vars])) / num_rows)

# Create a new graphics window with multiple sections
par(mfrow = c(num_rows, num_cols))

# Initialize a list to store the plots
plots_list <- list()

# Loop through the quantitative variables
for (var in names(ecommerce_data[quantitative_vars])) {
  if (length(unique(ecommerce_data[[var]])) > 10) {
    # Histogram for continuous variables
    plot <- ggplot(ecommerce_data, aes_string(x = var)) + 
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      labs(title = paste("Histogram of", var))
  } else {
    # Bar chart for discrete variables
    plot <- ggplot(ecommerce_data, aes_string(x = var)) + 
      geom_bar(fill = "orange", color = "black") +
      labs(title = paste("Bar chart of", var))
  }
  
  # Add the plot to the list
  plots_list[[length(plots_list) + 1]] <- plot
}

# Display the plots in a single window with a grid layout
grid.arrange(grobs = plots_list, ncol = num_cols)

# Interpretation:
# - Histograms are generated for continuous variables, and bar charts for discrete variables.

# c) Normality Tests (Shapiro-Wilk and Jarque-Bera)
# Shapiro-Wilk tests for normality
for (var in names(ecommerce_data[, quantitative_vars, drop = FALSE])) {
  sampled_data <- sample(ecommerce_data[[var]], 5000)
  print(paste("Shapiro-Wilk test for", var, ":"))
  print(shapiro.test(sampled_data))
}
# Calculate skewness and kurtosis for each quantitative variable
for (var in names(ecommerce_data[quantitative_vars])) {
  skew <- skewness(ecommerce_data[[var]])
  kurt <- kurtosis(ecommerce_data[[var]])
  
  print(paste("Skewness for", var, ":", skew))
  print(paste("Kurtosis for", var, ":", kurt))
}

# Jarque-Bera Test (requires the 'tseries' package)
sampled_data <- sample(ecommerce_data[[var]], min(5000, length(ecommerce_data[[Discount_offered]])))
print(paste("Jarque-Bera test for", var, ":"))
print(jarque.bera.test(sampled_data))

# Interpretation:
# - The normality tests (Shapiro-Wilk) are used to evaluate if the quantitative variables follow a normal distribution.
# - The results include the test statistic (W), p-value, and conclusion.

# 2. Analysis of Qualitative Variables
summary(ecommerce_data)
library(tidyr)

##################### Stacked Bar Charts ########
# Select relevant variables and reshape them into a long format
ecommerce_data <- ecommerce_data %>%
  mutate(Reached.on.Time_Y.N = ifelse(Reached.on.Time_Y.N == 1, "NPA", "A"))

# Check the modifications
summary(ecommerce_data)
str(ecommerce_data["Reached.on.Time_Y.N"])

long_data <- ecommerce_data %>%
  select(Mode_of_Shipment, Warehouse_block, Gender, Product_importance, Reached.on.Time_Y.N) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

# Calculate percentages for each category within each variable
long_data <- long_data %>%
  group_by(Variable, Value) %>%
  summarise(Count = n()) %>%
  mutate(Total = sum(Count, na.rm = TRUE)) %>%
  mutate(Percent = Count / Total * 100)

# Create the stacked bar chart
ggplot(long_data, aes(x = Variable, fill = Value)) +
  geom_bar(position = "fill") +
  geom_text(aes(label = sprintf("%.1f%%", Percent), y = Percent),
            position = position_fill(vjust = 0.5)) +  # Add percentages
  labs(title = "Distribution of categorical variables (stacked bar chart)", x = "", y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom")

##################### Pie Chart #########
long_data <- ecommerce_data %>%
  select(Mode_of_Shipment, Warehouse_block, Gender, Product_importance, Reached.on.Time_Y.N) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

# Calculate percentages for each category within each variable
long_data <- long_data %>%
  group_by(Variable, Value) %>%
  summarise(Count = n()) %>%
  mutate(Total = sum(Count, na.rm = TRUE)) %>%
  mutate(Percent = Count / Total * 100)

# Create the pie chart
ggplot(long_data, aes(x = "", y = Percent, fill = Value)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~ Variable) +
  labs(title = "Distribution of categorical variables (pie chart)", x = "", y = "") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom")

# Display frequencies for qualitative variables
for (var in c("Mode_of_Shipment", "Warehouse_block", "Gender", "Product_importance", "Reached.on.Time_Y.N")) {
  print(paste("Frequencies for", var, ":"))
  print(table(ecommerce_data[[var]]))
}
################### Bivariate Analysis ###############################
data3 = dataImp

View(data3)
dataencod <- na.omit(data3)
data3 = dataencod

# Create a contingency table
table_importance <- table(data3$Product_importance, data3$Reached.on.Time_Y.N)

# Chi-square test
result_chi_square_importance <- chisq.test(table_importance)
print(result_chi_square_importance)
# p-value: 0.002194
# The p-value is below the significance threshold of 0.05, suggesting strong evidence against the null hypothesis of independence.
# This indicates that there is a significant association between product importance and whether the delivery was on time or not.
# Conclusion: We reject the null hypothesis and conclude that there is a significant association between product importance and delivery punctuality.

# Order product importance levels
data3$Product_importance <- factor(data3$Product_importance, levels = c("low", "medium", "high"), ordered = TRUE)

# Bar plot for Product_importance vs. Reached.on.Time_Y.N
ggplot(data3, aes(x = reorder(Product_importance, as.numeric(Product_importance)), fill = as.factor(Reached.on.Time_Y.N))) +
  geom_bar(position = "dodge", color = "black", stat = "count") +
  labs(title = "Effect of Product Importance on On-Time Delivery",
       x = "Product Importance",
       y = "Count") +
  scale_fill_manual(values = c("lightgreen", "lightcoral"),
                    labels = c("On Time", "Not On Time")) +
  theme_minimal()

# Create a contingency table for Warehouse_block vs. Reached.on.Time_Y.N
table_warehouse <- table(data3$Warehouse_block, data3$Reached.on.Time_Y.N)

# Chi-square test
result_chi_square_warehouse <- chisq.test(table_warehouse)
print(result_chi_square_warehouse)
# p-value: 0.8819
# The p-value is well above the significance threshold of 0.05. This suggests that there is not enough evidence to reject the null hypothesis of independence.
# Conclusion: The warehouse block and delivery punctuality are not significantly associated, according to the Chi-square test.

# Bar plot for Warehouse_block vs. Reached.on.Time_Y.N
ggplot(data3, aes(x = Warehouse_block, fill = as.factor(Reached.on.Time_Y.N))) +
  geom_bar(position = "dodge", color = "black", stat = "count") +
  labs(title = "Effect of Warehouse Block on On-Time Delivery",
       x = "Warehouse Block",
       y = "Count") +
  scale_fill_manual(values = c("lightgreen", "lightcoral"),
                    labels = c("On Time", "Not On Time")) +
  theme_minimal()

# Split the data into on-time and not-on-time deliveries
on_time <- data3$Weight_in_gms[data3$Reached.on.Time_Y.N == 0]
not_on_time <- data3$Weight_in_gms[data3$Reached.on.Time_Y.N == 1]

# Perform a Wilcoxon-Mann-Whitney test for weight data
result_wilcoxon_weight <- wilcox.test(on_time, not_on_time)
print(result_wilcoxon_weight)
# p-value: < 2.2e-16
# The p-value is extremely small, well below the significance threshold of 0.05.
# This suggests strong evidence against the null hypothesis that there is no difference in the median position between the two groups.
# Hence, there is a significant difference in the median weight between on-time and delayed deliveries.

# Box plot for Weight_in_gms vs. Reached.on.Time_Y.N
ggplot(data3, aes(x = as.factor(Reached.on.Time_Y.N), y = Weight_in_gms)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Effect of Weight on On-Time Delivery",
       x = "On-Time Delivery",
       y = "Weight in grams") +
  theme_minimal()

# Correlation for "Customer_rating" vs. "Prior_purchases"
correlation_achats <- cor.test(data3$Customer_rating, data3$Prior_purchases)
print(correlation_achats)

# Interpretation of the correlation test:
# The correlation test evaluates whether there is a significant linear relationship between customer rating and the number of prior purchases.
# In this case, the p-value is high (p-value = 0.46), indicating that there is not enough evidence to reject the null hypothesis of no correlation.
# Thus, there is no significant correlation between customer rating and the number of prior purchases.

# 3. Examine the relationship between the discount offered and whether the product was delivered on time or not

# Box plot
boxplot(Discount_offered ~ Reached.on.Time_Y.N, data = data3, col = c("lightblue", "lightgreen"),
        xlab = "Reached on Time_Y.N", ylab = "Discount Offered", main = "Boxplot of Discount Offered by Reached on Time_Y.N")

result_wilcoxon_discount <- wilcox.test(Discount_offered ~ Reached.on.Time_Y.N, data = data3)
print(result_wilcoxon_discount)
# The test results suggest that the discounts offered for on-time and delayed deliveries are not independent.
# The extremely small p-value (< 2.2e-16) indicates strong evidence against the null hypothesis of independence.
# Therefore, we can conclude that there is a significant difference in the distribution of discounts offered between on-time and delayed deliveries.


###################################################################################################################

# ============================  Part 3: Modeling and Evaluation  ===============================

####################### 1. Regression of the target quantitative variable ###########################

# Regress the target quantitative variable based on the other variables

# We will perform a multiple linear regression of the target quantitative variable 'Prior_purchases' based on the other variables.
# Multiple linear regression model
# The multiple linear regression model is the simplest model for regressing a quantitative variable based on several explanatory variables.
# The model is as follows:

# Y = a + b_1 X_1 + b_2 X_2 + ... + b_n X_n
# where:

# Y is the target quantitative variable
# X_1, X_2, ..., X_n are the explanatory variables
# a is the constant
# b_1, b_2, ..., b_n are the regression coefficients
# We will use the lm() function in R to estimate the multiple linear regression model.
dataEncod <- subset(dataEncod, select = -c(Reached.on.Time_Y.N_imp, Weight_in_gms_imp, Discount_offered_imp, Gender_imp, Product_importance_imp, Prior_purchases_imp, Cost_of_the_Product_imp, Customer_rating_imp, Customer_care_calls_imp, Mode_of_Shipment_imp, Warehouse_block_imp))
str(dataEncod)
data = dataEncod
# Display the names of the variables
names(data)

-------------------------- Visualizing Relationships -------------------------------------------
# Setting up a layout for multiple plots
par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
plot(data$Prior_purchases, data$Customer_care_calls, main="Diagram")
plot(data$Prior_purchases, data$Mode_of_Shipment, main="Diagram")
plot(data$Prior_purchases, data$Warehouse_block, main="Diagram")
plot(data$Prior_purchases, data$Customer_rating, main="Diagram")
plot(data$Prior_purchases, data$Cost_of_the_Product, main="Diagram")
plot(data$Prior_purchases, data$Gender, main="Diagram")
plot(data$Prior_purchases, data$Discount_offered, main="Diagram")
plot(data$Prior_purchases, data$Weight_in_gms, main="Diagram")
plot(data$Prior_purchases, data$Reached.on.Time_Y.N, main="Diagram")
# Resetting the default plot layout
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1

------------------------------- Model Construction --------------------------------------------
linear_model <- lm(Prior_purchases ~ ., data=data)

# Display the regression results
summary(linear_model)

########################### 2. Removal of non-significant variables ###########################
linear_model <- step(linear_model, direction="backward")
summary(linear_model)

# The step() function:

# - Calculates the model with all variables
# - Removes the least significant variable (highest p-value)
# - Evaluates if the model is better without that variable (AIC criterion)
# - Repeats steps 2 and 3 until removing a variable worsens the model
# - Finally, we obtain a model with only significant explanatory variables.

# The advantage of this approach is twofold:

# - Simplicity: we keep fewer variables
# - Performance: we avoid overfitting by keeping only truly predictive variables
# - The summary() of the final model shows which variables remained in the model.

# In the step() function used here,
# variables are added or removed to minimize the AIC and optimize this criterion.

# In summary:

# The lower the AIC, the better the model
# We select the model with the minimum AIC.

################# 3. Strategy for improving model performance ########################
#   Strategy to improve the model:
# - Add interactions between variables
# - Transform certain variables (log, square root, etc.)
# - Test other regression algorithms (Random Forest, GBM, etc.)
# - Use cross-validation to evaluate performance and avoid overfitting
# - Optimize hyperparameters
# Metrics: RMSE, R2

################# 4. Modeling the discount/on-time delivery relationship #######################

# Creating the binary target variable
data$OnTime <- as.numeric(data$Reached.on.Time_Y.N)

# Logistic regression model
fit_log <- glm(OnTime ~ Discount_offered, data=data, family="binomial")
summary(fit_log)

# Interpretation of the results:

# - Overall significance of the model:
#    - The likelihood ratio test compares the current model (with the variable Discount_offered) to a reduced model with only the intercept.
#    - Since the p-value is < 0.001, we reject the null hypothesis, meaning the model with the discount is significantly better.

# - Coefficient of Discount_offered:
#   - Positive, so a higher discount increases the probability of being delivered on time
#   - Also significant (p-value < 0.01)
#   - An increase of 1 unit in the offered discount increases the log-odds of being delivered on time by 0.0232.

# - Model fit:
#   - The residual deviance (10689) is slightly lower than the null deviance (10699), meaning the model fits the data reasonably well.
#   - However, the difference is small, so the predictive power of the model is limited.

# In conclusion, the model shows that a higher discount significantly increases the probability of on-time delivery.
# However, the model fit is limited, and additional predictors may need to be added.

############################# 5. Clustering Techniques #####################################
# Use k-means or hierarchical clustering on customer characteristics:
# - purchase history
# - interactions
# - ratings
# This helps identify homogeneous customer segments.
############################# 5. Clustering Techniques #####################################
# Use k-means or hierarchical clustering on customer characteristics:
# - purchase history
# - interactions
# - ratings
# This helps identify homogeneous customer segments.
library(cluster)

# DBscan
dbscan_result <- dbscan(data, eps = 0.5, MinPts = 5)
hclust_result <- hclust(dist(data), method = "complete")
kmeans_result <- kmeans(data, centers = 3, nstart = 20)  # Adjust 'centers' based on your expected number of clusters

# Silhouette Score
silhouette_dbscan <- silhouette(dbscan_result$cluster, dist(data))
silhouette_hclust <- silhouette(cutree(hclust_result, k = 3), dist(data))
silhouette_kmeans <- silhouette(kmeans_result$cluster, dist(data))

# Davies-Bouldin Index
db_dbscan <- cluster.stats(dist(data), dbscan_result$cluster)$db
db_hclust <- cluster.stats(dist(data), cutree(hclust_result, k = 3))$db
db_kmeans <- cluster.stats(dist(data), kmeans_result$cluster)$db

# Adjusted Rand Index
ari_dbscan <- cluster.stats(dist(data), dbscan_result$cluster)$ari
ari_hclust <- cluster.stats(dist(data), cutree(hclust_result, k = 3))$ari
ari_kmeans <- cluster.stats(dist(data), kmeans_result$cluster)$ari

# Print or compare the results
print("Silhouette Score:")
print(c(DBSCAN = mean(silhouette_dbscan$width), Hierarchical = mean(silhouette_hclust$width), KMeans = mean(silhouette_kmeans$width)))

print("Davies-Bouldin Index:")
print(c(DBSCAN = db_dbscan, Hierarchical = db_hclust, KMeans = db_kmeans))

print("Adjusted Rand Index:")
print(c(DBSCAN = ari_dbscan, Hierarchical = ari_hclust, KMeans = ari_kmeans))

# Perform hierarchical clustering using precomputed distance matrix
distance_matrix <- dist(data)
hclust_result <- hclust(distance_matrix, method = "complete")

# Plot the dendrogram
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", xlab = "Index", sub = NULL)

# Cut the dendrogram to form clusters
clusters <- cutree(hclust_result, k = 3)

# Visualize the clusters (for 2D data)
plot(data, col = clusters, pch = 16, main = "Hierarchical Clustering")
legend("topright", legend = unique(clusters), col = unique(clusters), pch = 16, title = "Clusters")

# Generate a hypothetical dataset
set.seed(123)
data <- matrix(rnorm(100), ncol = 2)

# Perform DBSCAN clustering
dbscan_result <- dbscan(data, eps = 0.5, MinPts = 5)

# Print the cluster assignments
print(dbscan_result$cluster)

# Visualize the clusters (for 2D data)
plot(data, col = dbscan_result$cluster, pch = 16, main = "DBSCAN Clustering")
legend("topright", legend = unique(dbscan_result$cluster), col = unique(dbscan_result$cluster), pch = 16, title = "Clusters")

