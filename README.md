# E-commerce Data Analysis

## Overview
This project involves analyzing e-commerce data to study customer behavior and delivery performance. The analysis includes data preprocessing, exploratory data analysis (EDA), modeling, and evaluation of delivery times and customer purchases.

## Key Steps

### 1. Data Preprocessing
- **Outlier Detection**: Outliers in key variables such as `Cost_of_the_Product`, `Prior_purchases`, and `Discount_offered` are identified and replaced with missing values.
- **Handling Missing Values**: Missing data is imputed using K-Nearest Neighbors (KNN) imputation to maintain data integrity.
- **Encoding**: Categorical variables like `Product_importance` and `Mode_of_Shipment` are transformed into numeric values for analysis.
- **Normalization**: Numeric data is normalized to ensure all features are on the same scale for better model performance.

### 2. Exploratory Data Analysis (EDA)
- **Quantitative Analysis**: Descriptive statistics are generated to understand the distribution of continuous variables such as `Customer_rating`, `Weight_in_gms`, etc.
- **Qualitative Analysis**: Visualizations like stacked bar charts and pie charts are used to study categorical variables, including `Mode_of_Shipment` and `Reached.on.Time_Y.N`.
- **Bivariate Analysis**: Relationships between variables like `Product_importance` and delivery punctuality are analyzed using statistical tests such as the chi-square test.

### 3. Modeling and Evaluation
- **Multiple Linear Regression**: A regression model is built to predict customer behavior, particularly `Prior_purchases`.
- **Logistic Regression**: Used to model the probability of on-time deliveries based on the discounts offered.
- **Model Improvement**: Techniques like feature interaction and cross-validation are employed to enhance model performance.

### 4. Clustering
- **K-Means Clustering**: Customer segmentation is performed using clustering algorithms to identify distinct customer groups based on their behavior and purchase history.

## Conclusion
This project demonstrates the application of data preprocessing techniques, exploratory analysis, and machine learning models to extract meaningful insights from e-commerce data. Through modeling and evaluation, we gain a deeper understanding of customer behavior and factors affecting delivery performance.

## Technologies Used
- **R**: Data processing, visualization, and modeling
- **Libraries**: `ggplot2`, `caret`, `RANN`, `VIM`, `cluster`

## Future Improvements
- **Enhancing models** by incorporating more advanced techniques such as Random Forest or Gradient Boosting.
- **Improving prediction accuracy** using hyperparameter tuning and further cross-validation.
