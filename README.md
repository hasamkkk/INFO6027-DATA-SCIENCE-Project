# INFO6027-DATA-SCIENCE-Project
Files associated with INFO6027 project
🎯 Project Description

This project involves:

Preprocessing music dataset by handling missing values, scaling numeric features, and treating outliers.
Performing exploratory data analysis (EDA) through visualizations and correlation analysis.
Building and evaluating predictive models like Linear Regression and Random Forest.
Comparing model performance using metrics like RMSE, MAE, and R-squared.

Features:

Data cleaning and preprocessing.
Statistical analysis and outlier treatment.
Visualizing distributions and feature correlations.
Predictive modeling using:
Linear Regression
Random Forest
Evaluation of model performance metrics

Installation:

Install the required R packages:

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("caTools")
install.packages("Metrics")
install.packages("moments")
install.packages("randomForest")
install.packages("caret")

Load the libraries in your R script:

library(tidyverse)
library(ggplot2)
library(tidyr)
library(caTools)
library(Metrics)
library(moments)
library(randomForest)
library(caret)

Ensure you have the dataset file (dataset.csv) in your working directory.

Usage guidline:

Preprocess the dataset:
Drop unnecessary columns (Unnamed: 0).
Handle missing values and duplicates.
Scale numeric features.

Analyze data:
View summary statistics for numeric features.
Visualize data distributions and outlier treatment.

Model training:
Split the dataset into training (70%) and testing (30%) subsets.
Train a Linear Regression model.
Train a Random Forest model with feature importance.

Evaluate models:
Calculate metrics like RMSE, MAE, and R-squared.
Visualize actual vs. predicted values for both models.

Key visualizations:
Histograms and boxplots for feature distributions.
Correlation matrix for numeric predictors.
Scatter plots with regression lines.

Model Evaluation:

RMSE: 22.08
MAE: 18.44
R-squared: 0.02

RMSE:  15.38
MAE:  11.22
R-squared:  0.55

Dependencies

tidyverse: For data manipulation and visualization.
ggplot2: For plotting.
tidyr: For data tidying.
caTools: For dataset splitting.
Metrics: For evaluation metrics.
moments: For skewness calculations.
randomForest: For Random Forest model.
caret: For preprocessing and feature scaling.



