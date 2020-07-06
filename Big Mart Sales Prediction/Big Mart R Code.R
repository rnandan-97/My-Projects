#...............Big Mart Sales Prediction Using R.................

#.......................Loading Packages..........................
library(data.table)           #used for reading and manipulation of data
library(dplyr)                #used for data manipulation and joining
library(ggplot2)              #used for ploting
install.packages("caret")
library(caret)                #used for modeling
install.packages("corrplot")
library(corrplot)             #used for making correlation plot
install.packages("xgboost")
library(xgboost)              #used for building XGBoost model
install.packages("cowplot")
library(cowplot)              #used for combining multiple plots

getwd()
setwd("C:/Users/user/Desktop/Projects/Big Mart Sales Prediction (Analytics Vidya)")

#........................Reading Data..............................

#The Train file contains 11 independent variables and 1 target variable,
#i.e., Item_Outlet_Sales.
#The Test file also contains the same set of independent variables,
#but there is no target variable because that is what we have to predict.
#The Sample Submissions file contains the format 
#in which we have to submit our predictions.

train = fread("train_file.csv")
test = fread("test_file.csv")
submission = fread("sample_submission.csv")

#.......................Understanding the Data.....................

#........................Dimensions of Data........................
dim(train)
dim(test)

#........................Features of Data..........................
names(train)
names(test)

#........................Structure of Data.........................
str(train)
str(test)

#there are 4 numeric and 7 categorical variables.

#........................Combine Train and Test....................

#Combining train and test sets saves a lot of time and effort because 
#if we have to make any modification in the data, we would make the 
#change only in the combined data and not in train and test data separately.

test$Item_Outlet_Sales<- NA
combi = rbind(train, test) # combining train and test datasets 

dim(combi)
names(combi)

#..................Exploratory Data Analysis (EDA).................

#........................Univariate Analysis........................

#Note: ggplot2 package has been used to generate all the plots

#........................Target Variable............................

#Since our target variable is continuous, 
#we can visualise it by plotting its histogram.

ggplot(train)+geom_histogram(aes(train$Item_Outlet_Sales),binwidth = 100,fill = "darkgreen")+xlab("Item_Outlet_Sales")

#It is a right skewd variable and would need some data 
#transformation to treat its skewness.

#.............Independent Variables (numeric variables)...............

p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")

plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package

# Observations
#
# There seems to be no clear-cut pattern in Item_Weight.
# Item_Visibility is right-skewed and should be 
# transformed to curb its skewness.
# We can clearly see 4 different distributions for Item_MRP. 
# It is an interesting insight.


#............Independent Variables (categorical variables)..........

#summarise(Count = n()) is from dplyr

#A categorical variable or feature can have only a 
#finite set of values. Let’s first plot Item_Fat_Content.

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular" 

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1") 
#Plot for Item_Fat_Content
#ggplot(combi)+geom_bar(mapping=aes(Item_Fat_Content),fill="coral1")+
#xlab("Item_Fat_Content") 
#Alternative representation of plot for Item_Fat_Content

#Now let’s check the other categorical variables.

p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
     geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +  
     xlab("") + geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5)+ 
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
     ggtitle("Item_Type") 

p4   # Plot for Item_Type

p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n()))+ 
     geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1")+
     geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) + 
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p5   # Plot for Outlet_Identifier

combi$Outlet_Size[combi$Outlet_Size == ""] = NA
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +
     geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
     geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p6   # Plot for Outlet_Size

second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)

# In Outlet_Size’s plot, for 4016 observations, 
# Outlet_Size is blank or missing.We will check for this in the 
# bivariate analysis to substitute the missing values in the Outlet_Size.

# We’ll also check the remaining categorical variables.

p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +
     geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
     geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
     xlab("Outlet_Establishment_Year") + theme(axis.text.x = element_text(size = 8.5)) 

p7   # Plot for Outlet_Establishment_Year

p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +
     geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") + 
     geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) + 
     theme(axis.text.x = element_text(size = 8.5)) 

p8   # Plot for Outlet_Type

plot_grid(p7, p8, ncol = 2) # Ploting both plots together

# Observations
# 
# Lesser number of observations in the data for the outlets 
# established in the year 1998 as compared to the other years.
# Supermarket Type 1 seems to be the most popular category of Outlet_Type.

#........................Bivariate Analysis.........................

# We will make use of scatter plots for the continuous or numeric variables 
# and violin plots for the categorical variables.

train = combi[1:nrow(train)] # extracting train data from the combined data

#.........Target Variable vs Independent Numerical Variables........

# Let’s explore the numerical variables first

p9 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
     theme(axis.title = element_text(size = 8.5)) 

p9   # Item_Weight vs Item_Outlet_Sales

p10 = ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
      theme(axis.title = element_text(size = 8.5)) 

p10   # Item_Visibility vs Item_Outlet_Sales

p11 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
      theme(axis.title = element_text(size = 8.5)) 

p11   # Item_MRP vs Item_Outlet_Sales

second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)

# Observations
# 
# Item_Outlet_Sales is spread well across the 
# entire range of the Item_Weightwithout any obvious pattern.
# In Item_Visibility vs Item_Outlet_Sales, there is a string of points at 
# Item_Visibility = 0.0 which seems strange as item visibility cannot be 
# completely zero. We will take note of this issue and deal with it later on.
# In the third plot of Item_MRP vs Item_Outlet_Sales, 
# we can clearly see 4 segments of prices that can be 
# used in feature engineering to create a new variable.

#.........Target Variable vs Independent Categorical Variables......

# Now we’ll visualise the categorical variables with respect to Item_Outlet_Sales.

# We could have used boxplots here, but instead we’ll use the violin plots as 
# they show the full distribution of the data. The width of a violin plot at a 
# particular level indicates the concentration or density of data at that level. 
# The height of a violin tells us about the range of the target variable values.

p12 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6), axis.title = element_text(size = 8.5)) 

p12   # Item_Type vs Item_Outlet_Sales

train$Item_Fat_Content[train$Item_Fat_Content == "LF"] = "Low Fat" 
train$Item_Fat_Content[train$Item_Fat_Content == "low fat"] = "Low Fat" 
train$Item_Fat_Content[train$Item_Fat_Content == "reg"] = "Regular" 

p13 = ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 8), axis.title = element_text(size = 8.5)) 

p13   # Item_Fat_Content vs Item_Outlet_Sales

p14 = ggplot(train) + geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 8), axis.title = element_text(size = 8.5)) 

p14   # Outlet_Identifier vs Item_Outlet_Sales

second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)

# Observations
# 
# Distribution of Item_Outlet_Sales across the categories of Item_Type 
# is not very distinct and same is the case with Item_Fat_Content.
# The distribution for OUT010 and OUT019 categories of Outlet_Identifier are 
# quite similar and very much different from the rest of the categories of Outlet_Identifier.

# Checking the distribution of the target variable across Outlet_Size
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

# The distribution of ‘Small’ Outlet_Size is almost identical to the 
# distribution of the blank category (first vioin) of Outlet_Size. 
# So, we can substitute the blanks in Outlet_Size with ‘Small’. 
# But there are other ways to impute missing values.
combi$Outlet_Size[is.na(combi$Outlet_Size)] <- "Small"

# Checking the distribution of remaining variables
p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")

plot_grid(p15, p16, ncol = 1)

# Observations
# 
# Tier 1 and Tier 3 locations of Outlet_Location_Type look similar.
# In the Outlet_Type plot, Grocery Store has most of its data points 
# around the lower sales values as compared to the other categories.

#........................Missing Value Treatment....................

# Some of the common techniques are as follows:
#   Deletion of rows
#   Mean/Median/Mode Imputation
#   Building Prediction Model

# We can try the following code to quickly find missing values in a variable.
  
sum(is.na(combi$Item_Weight))

# We have missing values in Item_Weight and Item_Outlet_Sales. 
# Missing data in Item_Outlet_Sales can be ignored since they belong to 
# the test dataset. We’ll now impute Item_Weight with mean weight based on 
# the Item_Identifier variable.

missing_index = which(is.na(combi$Item_Weight))
for (i in missing_index) {
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = TRUE)
}

sum(is.na(combi$Item_Weight))

#0 missing values! It means we have successfully imputed the missing data in the feature.

# Replacing 0’s in Item_Visibility variable

# Similarly, zeroes in Item_Visibility variable can be replaced with 
# Item_Identifier wise mean values of Item_Visibility. It can be visualized 
# in the plot below.

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

# Let’s replace the zeroes.

zero_index = which(combi$Item_Visibility == 0)
for (i in zero_index) {
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = TRUE)
}

# After the replacement of zeroes, We’ll plot the histogram of Item_Visibility 
# again. In the histogram, we can see that the issue of zero item visibility 
# has been resolved.

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

#........................Feature Engineering........................

# Most of the times, the given features in a dataset are not sufficient to 
# give satisfactory predictions. In such cases, we have to create new features 
# which might help in improving the model’s performance. Let’s try to create 
# some new features for our dataset.
# 
# In this section we will create the following new features:
#   
#   Item_Type_new: Broader categories for the variable Item_Type.
#   Item_category: Categorical variable derived from Item_Identifier.
#   Outlet_Years: Years of operation for outlets.
#   price_per_unit_wt: Item_MRP/Item_Weight
#   Item_MRP_clusters: Binned feature for Item_MRP.

# Categorizing Item_Type

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

# create a new feature 'Item_Type_new' 

combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]
combi$Item_Type_new

# Let’s compare Item_Type with the first 2 characters of Item_Identifier, 
#i.e., ‘DR’, ‘FD’, and ‘NC’. These identifiers most probably stand for drinks, food, and non-consumable.

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

# Based on the above table we can create a new feature. Let’s call it Item_category.

combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]
combi$Item_category

# We will also change the values of Item_Fat_Content wherever Item_category
#is ‘NC’ because non-consumable items cannot have any fat content.

combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"
combi$Item_Fat_Content

# Now we will also create a couple of more features — Outlet_Years 
#(years of operation) and price_per_unit_wt (price per unit weight)

# years of operation for outlets

combi$Outlet_Establishment_Year <- as.numeric(combi$Outlet_Establishment_Year)
combi$Outlet_Years = 2020 - combi$Outlet_Establishment_Year
combi$Outlet_Years  

combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 

# Price per unit weight

combi[,price_per_unit_wt := Item_MRP/Item_Weight]
combi$price_per_unit_wt

# Earlier in the Item_MRP vs Item_Outlet_Sales plot, we saw Item_MRP was 
# spread across in 4 chunks. Now let’s assign a label to each of these 
# chunks and use this label as a new variable.

# creating new independent variable - Item_MRP_clusters 

combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st", ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd", ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]
combi$Item_MRP_clusters

#....................Encoding Categorical Variables..................

# Most of the machine learning algorithms produce better result with 
# numerical variables only. So, it is essential to treat the categorical 
# variables presentin the data.

# We will use 2 techniques — Label Encoding and One Hot Encoding.
# Label encoding simply means converting each category in a variable to 
# a number. It is more suitable for ordinal variables — 
# categorical variables with some order.

#In One hot encoding, each category of a categorical variable is 
#converted into a new binary column (1/0).

#........Label encoding for the ordinal categorical variables........

#We will label encode Outlet_Size and Outlet_Location_Type as these are ordinal variables

combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0, ifelse(Outlet_Size == "Medium", 1, 2))] 
combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0, ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]
combi$Outlet_Size_num
combi$Outlet_Location_Type_num

# removing categorical variables after label encoding 

combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

#.....One hot encoding for the non-ordinal categorical variable......

# dummyVars is from caret

ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T) 
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)
combi

#........................PreProcessing Data...........................

# In simple words, pre-processing refers to the transformations applied to 
# our data before feeding it to the algorithm. It invloves further cleaning 
# of data, data transformation, data scaling and many more things.
# 
# For our data, we will deal with the skewness and scale the numerical variables

#........................Removing Skewness............................

# Skewness in variables is undesirable for predictive modeling. Some machine 
# learning methods assume normally distributed data and a skewed variable can 
# be transformed by taking its log, square root, or cube root so as to make 
# its distribution as close to normal distribution as possible. In our data, 
# variables Item_Visibility and price_per_unit_wt are highly skewed. So, we 
# will treat their skewness with the help of log transformation.

combi[,Item_Visibility := log(Item_Visibility + 1)]
# log + 1 to avoid division by zero 
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]

#....................Scaling numeric predictors.......................

# Let’s scale and center the numeric variables to make them have a mean of 
# zero, standard deviation of one and scale of 0 to 1. Scaling and centering 
# is required for linear regression models.

#preProcess from caret
num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
string <- setdiff(num_vars_names, "Item_Outlet_Sales")
length(string)
q <- which(colnames(combi) == string[1])
for (i in 2:length(string)) {
  q <- c(q,which(colnames(combi) == string[i]))
}
q
combi_numeric = combi[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33)]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)

# Splitting the combined data combi back to train and test set

train = combi[1:nrow(train)]
test = combi[(nrow(train) + 1):nrow(combi)] 
test[,Item_Outlet_Sales := NULL] 
# removing Item_Outlet_Sales as it contains only NA for test dataset

# Correlated Variables

cor_train = cor(train[,-c("Item_Identifier")]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

# A blueish pie indicates positive correlation and reddish pie indicates 
# negative correlation. The magnitude of the correlation is denoted by the 
# area covered by the pie.

# Variables price_per_unit_wt and Item_Weight are highly correlated as the 
# former one was created from the latter. Similarly price_per_unit_wt and 
# Item_MRP are highly correlated for the same reason.

# Model Building

# We will build the following models in the next sections.
# 
# Linear Regression
# Lasso Regression
# Ridge Regression
# RandomForest
# XGBoost

# Evaluation Metrics for Regression

# Mean Absolute Error (MAE)
# Mean Squared Error (MSE)
# Root Mean Squared Error (RMSE) 

# Linear Regression

# Building Model

linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])

# Making Predictions on test Data

# preparing dataframe for submission and writing it in a csv file 

submission$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")])

# If the Item_Outlet_Sales is negative, that is not possible. So, from the
# train file, I observed that if we take simply the modulus, then it goes 
# near about the real value

submission$Item_Outlet_Sales = abs(submission$Item_Outlet_Sales) 
write.csv(submission, "Linear_Reg_submit.csv", row.names = FALSE)

# Leaderboard score: 1192.60 (Based on RMSE)

#....................Regularized Linear Regression....................
#.....................Lasso and Ridge Regression......................

# Regularised regression models can handle the correlated independent 
# variables well and helps in overcoming overfitting.
# Ridge penalty shrinks the coefficients of correlated 
# predictors towards each other.
# Lasso tends to pick one of a pair of correlated features and discard the 
# other. The tuning parameter lambda controls the strength of the penalty.

# Lasso Regression

install.packages("glmnet")
library(glmnet)

set.seed(1235)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002))

lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales, method='glmnet', trControl= my_control, tuneGrid = Grid)

# preparing dataframe for submission and writing it in a csv file 

submission$Item_Outlet_Sales = predict(lasso_linear_reg_mod, test[,-c("Item_Identifier")])

# If the Item_Outlet_Sales is negative, that is not possible. So, from 
# the train file, I observed that if we take simply the modulus, then it 
# goes near about the real value

submission$Item_Outlet_Sales = abs(submission$Item_Outlet_Sales) 
write.csv(submission, "lasso_linear_reg_mod.csv", row.names = FALSE)

# Mean validation score: 1130.02
# Leaderboard score: 1193.07

# Ridge Regression

set.seed(1236)
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002))

ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales, method='glmnet', trControl= my_control, tuneGrid = Grid)

# preparing dataframe for submission and writing it in a csv file 

submission$Item_Outlet_Sales = predict(ridge_linear_reg_mod, test[,-c("Item_Identifier")])

# If the Item_Outlet_Sales is negative, that is not possible. So, from 
# the train file, I observed that if we take simply the modulus, then it 
# goes near about the real value

submission$Item_Outlet_Sales = abs(submission$Item_Outlet_Sales) 
write.csv(submission, "ridge_linear_reg_mod.csv", row.names = FALSE)

# Mean validation score: 1135.08
# Leaderboard score: 1200.68

# Random Forest

# RandomForest is a tree based bootstrapping algorithm wherein a certain 
# number of weak learners (decision trees) are combined to make a powerful 
# prediction model. We will now build a RandomForest model with 400 trees. 
# The other tuning parameters used here are mtry — no. of predictor variables 
# randomly sampled at each split, and min.node.size — minimum size of terminal
# nodes (setting this number large causes smaller trees and reduces overfitting).

install.packages("ranger")
library(ranger)

set.seed(1237) 
my_control = trainControl(method="cv", number=5) # 5-fold CV 
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)
rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method='ranger',
               trControl= my_control,
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")

# preparing dataframe for submission and writing it in a csv file 

submission$Item_Outlet_Sales = predict(rf_mod, test[,-c("Item_Identifier")])
write.csv(submission, "rf_mod.csv", row.names = FALSE)

# Mean validation score: 1088.05
# Leaderboard score: 1161.55

# Best Model Parameters

plot(rf_mod)

# As per the plot shown above, the best score is achieved at mtry = 5 
# and min.node.size = 20.

# Variable Importance

# Let’s plot feature importance based on the RandomForest model

plot(varImp(rf_mod))

# As expected Item_MRP is the most important variable in predicting the 
# target variable. New features created by us, like price_per_unit_wt, 
# Outlet_Years, Item_MRP_Clusters, are also among the top most important 
# variables. This is why feature engineering plays such a crucial role in 
# predictive modeling.

# XGBoost

# XGBoost works only with numeric variables and we have already replaced the 
# categorical variables with numeric variables. There are many tuning 
# parameters in XGBoost which can be broadly classified into General 
# Parameters, Booster Parameters and Task Parameters.

# General parameters refer to which booster we are using to do boosting. 
# The commonly used are tree or linear model
# Booster parameters depend on which booster you have chosen
# Learning Task parameters that decide on the learning scenario, for example, 
# regression tasks may use different parameters with ranking tasks.

# Let’s have a look at the parameters that we are going to use in our model.
# 
# eta: It is also known as the learning rate or the shrinkage factor. 
#      It actually shrinks the feature weights to make the boosting process 
#      more conservative. The range is 0 to 1. Low eta value means the model 
#      is more robust to overfitting.
# gamma: The range is 0 to ∞. Larger the gamma more conservative the 
#        algorithm is.
# max_depth: We can specify maximum depth of a tree using this parameter.
# subsample: It is the proportion of rows that the model will randomly
#            select to grow trees.
# colsample_bytree: It is the ratio of variables randomly chosen to build 
#                   each tree in the model.

param_list = list(objective = "reg:linear", eta=0.01, gamma = 1, max_depth=6, subsample=0.8, colsample_bytree=0.5)
dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label= train$Item_Outlet_Sales) 
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))

# Cross Validation

# We are going to use the xgb.cv() function for cross validation.
# Here we are using cross validation for finding the optimal value of nrounds.

set.seed(1121)
xgbcv = xgb.cv(params = param_list, data = dtrain, nrounds = 1000, nfold = 5, print_every_n = 10, early_stopping_rounds = 30, maximize = FALSE)

# Model Training

# As per the verbose above, we got the best validation/test score at the 435th
# iteration. Hence, we will use nrounds = 435 for building the XGBoost model

xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 435)

# preparing dataframe for submission and writing it in a csv file 

submission$Item_Outlet_Sales = predict(xgb_model, as.matrix(test[,-c("Item_Identifier")]))

# If the Item_Outlet_Sales is negative, that is not possible. So, from 
# the train file, I observed that if we take simply the modulus, then it 
# goes near about the real value

submission$Item_Outlet_Sales = abs(submission$Item_Outlet_Sales)
write.csv(submission, "xgb_model.csv", row.names = FALSE)

# Mean validation score: 1089.79
# Leaderboard score: 1154.80

# Variable Importance

var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), model = xgb_model)
xgb.plot.importance(var_imp)

# Again the features created by us, like price_per_unit_wt, Outlet_Years, 
# Item_MRP_Clusters, are among the top most important variables.

# Endnote : After trying and testing 5 different algorithms, the best score 
# on the public leaderboard has been achieved by XGBoost (1154.80), 
# followed by RandomForest (1161.55). 

# Bar plot showing Mean validation score (mvs in blue) and Leaderboard score (ls in red) comparing 4 different types of regression

mvs_ls <- c(c(1130.02,1193.07),c(1135.08,1200.68),c(1088.05,1161.55),c(1089.79,1154.80))
names <- rep(c("Lasso Regression", "Ridge Regression", "RandomForest", "XGBoost"),each = 2)

barplot(mvs_ls, beside = TRUE, col = c("blue","red"),names.arg=names, cex.names = .8)
legend("topright", 
       legend = mvs_ls, 
       fill = rep(c("blue","red"),4), ncol = 2,
       cex = 0.45)
legend("bottomright", 
       legend = c("mvs","ls"), 
       fill = c("blue","red"), ncol = 2,
       cex = 0.45)
