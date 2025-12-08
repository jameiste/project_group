# install.packages("xml2")
# install.packages("jsonlite")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("glue")
# install.packages("glmnet")
# install.packages("rpart")
# install.packages("rpart.plot)
# install.packages("gbm")

library("xml2")
library("jsonlite")
library("ggplot2")
library("readxl")
library("glue")
library("glmnet")
library("rpart")
library("rpart.plot")
library("gbm")

# --- Load data  ---
rent_data <- read.csv("data/apartments_for_rent_classified_10K.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = "",
  fileEncoding = "latin1"
)
seed <- 2000
set.seed(seed)


# Modify the data
columns_of_interest <- c("price", "square_feet", "bedrooms", "bathrooms", "cityname", "state", "longitude", "latitude", "price_type")
rent_data <- rent_data[, columns_of_interest]

# --- Prepare data for regression ---
# Some are weekly listed
rent_data[grepl("Weekly", rent_data$price_type, fixed = TRUE), "price"] <- rent_data[grepl("Weekly", rent_data$price_type, fixed = TRUE), "price"] * 4
rent_data$log_price <- log(rent_data$price)
rent_data[rent_data == "null"] <- NA
rent_data[rent_data == ""] <- NA
# Drop rows with NA (inspiration: https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame)
rent_data <- rent_data[complete.cases(rent_data),]

# Cut outliers
rent_data <- rent_data[(rent_data$price <= quantile(rent_data$price, 0.95)) & (rent_data$price >= quantile(rent_data$price, 0.01)), ]

# There are too many states, hence, separate them in 5 regions (https://www.jagranjosh.com/general-knowledge/regions-of-united-states-complete-list-history-and-importance-1721218579-1)
northeast <- c("ME","NH","VT","MA","RI","CT","NY","PA","NJ")
southeast <- c("DE","MD","DC","VA","WV","NC","SC","GA","FL","KY","TN","AL","MS","AR","LA")
midwest <- c("OH","MI","IN","IL","WI","MN","IA","MO","ND","SD","NE","KS")
southwest <- c("TX","OK","NM","AZ")
west <- c("CA","NV","UT","CO","WY","MT","ID","OR","WA","AK","HI") 
rent_data$region <- "Other"
region_list = c("Northeast", "Southeast","Midwest", "Southwest", "West")
# Apply the mapping
for (region in region_list) {
  list <- get(tolower(region))
  rent_data[rent_data$state %in% list, "region"] <- region
}

# Who in the world uses feet
rent_data$square_feet <- rent_data$square_feet * 0.3048^2
names(rent_data)[names(rent_data) == "square_feet"] <- "square_meter"
# rent_data[, "latitude_sq"] <- as.numeric(rent_data$latitude)^2
# rent_data[, "longitude_sq"] <- as.numeric(rent_data$longitude)^2
# rent_data[, "lon_lat"] <- as.numeric(rent_data$latitude) * as.numeric(rent_data$longitude)
# Predictors for regression (TODO: We could limit that to certain states, otherwise all 51)

# Define the columns of interest
regional_parameter <- "region" # Decide here on which variable it should rely
target <- "log_price"
regression_columns <- c("square_meter", "bedrooms", "bathrooms", regional_parameter)
# Numeric columns 
numeric_columns <- c("price", "log_price", "square_meter", "bedrooms", "bathrooms")
rent_data[numeric_columns] <- lapply(rent_data[numeric_columns], as.numeric)
numeric_regression_cols <- setdiff(names(rent_data)[sapply(rent_data, is.numeric)], setdiff(names(rent_data), regression_columns))

# Handle state as categorical
regression_data <- rent_data
regression_data[, regional_parameter] <- as.factor(regression_data[, regional_parameter])

# Split the data in test and training data
ratio_train <- 0.8
amount_rows <- nrow(regression_data)
train_indices <- sample(1:amount_rows, round(amount_rows * ratio_train))

# Divide data sets
regression_data_train <- regression_data[train_indices,]
regression_data_test <- regression_data[-train_indices, ]
# Keep only those states that are in train (error at prediction otherwise)
train_states <- unique(regression_data_train$state)
regression_data_test <- regression_data_test[regression_data_test$state %in% train_states, ]
target_train <- regression_data[train_indices, target]
target_test <- regression_data[-train_indices, target]

# Re-scale the data
mean_std <- list(
  mean = sapply(regression_data_train[, numeric_regression_cols], mean),
  std  = sapply(regression_data_train[, numeric_regression_cols], sd)
) # List storing mean and std for standardization
regression_data_train_scale <- regression_data_train
regression_data_test_scale <- regression_data_test
# Apply scaling on training and test data
regression_data_train_scale[, numeric_regression_cols] <- (regression_data_train[, numeric_regression_cols] - mean_std$mean) / mean_std$std
regression_data_test_scale[, numeric_regression_cols] <- (regression_data_test[, numeric_regression_cols] - mean_std$mean) / mean_std$std

# Build regression formula
regression_formula <- as.formula(glue("{target} ~ {paste(regression_columns, collapse = ' + ')}"))
# Design matrix
regression_matrix <- model.matrix(regression_formula, data = regression_data_train_scale)[, -1]
test_matrix <- model.matrix(regression_formula, data = regression_data_test_scale)[, -1]
target_array <- as.numeric(target_train)

# --- Linear regression ---
linear_regression <- lm(regression_formula, data = regression_data_train)
coefficients_linear <- coef(linear_regression)
linear_regression_coefficient <- data.frame(
  parameter = rownames(summary(linear_regression)$coefficients),
  parameter_plot = gsub("^state", "", rownames(summary(linear_regression)$coefficients)),
  coefficient = summary(linear_regression)$coefficients[, "Estimate"],
  variance = summary(linear_regression)$coefficients[, "Std. Error"],
  row.names  = NULL
)
fitted_values_df <- data.frame(
  actual = exp(target_train),
  fitted = exp(predict(linear_regression)),
  residuals = exp(target_train) - exp(predict(linear_regression))
)
# Plot results
l_r_coef_inte <- linear_regression_coefficient[linear_regression_coefficient$parameter != "(Intercept)",] # Kick out the intercept
plot_lm_parameter <- ggplot(l_r_coef_inte, aes(y = parameter_plot)) +
  geom_point(aes(x = coefficient, color = "Coefficient"), size = 2) +
  geom_errorbar(aes(x = coefficient, y = parameter_plot, xmin = coefficient - variance, xmax = coefficient + variance),  color = "#6D6E71", width = 0.3, alpha = 0.5) +
  scale_color_manual( values = c("Coefficient"= "#002F5F" )) +
  labs(x = "Value", y = "Parameter", color = "Regression") + theme_light() +
  theme(
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.1),
    legend.title = element_text(size = 8, face = "bold"))

plot_regression <- ggplot(fitted_values_df, aes(x = fitted, y = actual)) +
  geom_point(color = "black", fill = "#ABDEE6", alpha = 0.6, shape=21, size=2) + # SU colors
  # Check the 10 smallest values, otherwise it would be overloaded
  geom_segment(data = subset(fitted_values_df, abs(residuals) > 0.3), aes(xend = fitted, yend = fitted), color = "#b00020", alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "#002F5F", linewidth = 1) +
  labs(
    x = "Fitted values",
    y = "Price",
    title = "Linear regression"
  ) +
  theme_minimal() +
  theme(legend.position =  "top",
        legend.background = element_rect(fill="transparent", color="black", linewidth=0.1),
        legend.title = element_blank())
# Print and store
print(plot_regression)
print(plot_lm_parameter)

# --- --- Ridge & Lasso regression --- ---
# CV (Leave-one-out, nfold = amount of entries is leave one out cross validation (Lecture))
cv_ridge <- cv.glmnet(regression_matrix, target_array, alpha = 0, nfolds = 5)
cv_lasso <- cv.glmnet(regression_matrix, target_array, alpha = 1, nfolds = 5)
min_lambda_ridge <- cv_ridge$lambda.min
min_lambda_lasso <- cv_lasso$lambda.min

# Regression
ridge <- glmnet(regression_matrix, target_array, alpha = 0, lambda = min_lambda_ridge)
lasso <- glmnet(regression_matrix, target_array, alpha = 1, lambda = min_lambda_lasso)

# Analyze regressions
# Take the minimum for each ticker
coef_ridge <- as.matrix(coef(ridge))
coef_lasso <- as.matrix(coef(lasso))

shrinkage_coef <- data.frame(
  parameter = rownames(coef_ridge),
  parameter_plot = gsub("^state", "", rownames(coef_ridge)),
  coef_ridge = coef_ridge[, 1],
  coef_lasso = coef_lasso[, 1],
  row.names = NULL
)

# Plot
shrinkage_coef_plot <- shrinkage_coef[shrinkage_coef$parameter != "(Intercept)",]
plot_mininimal_lambda <- ggplot(shrinkage_coef_plot, aes(y = parameter_plot)) +
  geom_point(aes(x = coef_ridge, color = "Ridge"), size = 2) +
  geom_point(aes(x = coef_lasso, color = "Lasso"), size = 2) +
  scale_color_manual(values = c("Ridge" = "#002F5F", "Lasso" = "#ABDEE6")) +
  labs(x = "Value", y = "Ticker", color = "Regression") + theme_light() +
  theme(
    legend.position = c(0.9, 0.85),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.1),
    legend.title = element_text(size = 8, face = "bold"))

#Print and store
print(plot_mininimal_lambda)

# --- --- Regression Tree --- ----
# Tree
tree_columns <- c("square_meter", "bedrooms", "bathrooms", "region")
tree_formula <- as.formula(glue("price ~ {paste(tree_columns, collapse = ' + ')}"))
regression_tree <- rpart(tree_formula, data = regression_data)

# Plot
plot_tree <- rpart.plot(
  regression_tree,
  type = 3,
  extra = 101,
  fallen.leaves = TRUE,
 cex = 0.6 
)

# Print and Store 
print(plot_tree)

# --- --- Boosting --- ---
boosting <- gbm(
  formula = log_price ~., 
  data = data.frame( log_price = target_array, regression_matrix),
  distribution = "gaussian", 
  n.trees = 1000, 
  interaction.depth = 4, 
  shrinkage = 0.01, 
  cv.folds = 5
)
best_trees <- gbm.perf(boosting, method='cv')

test_gbm <- data.frame(log_price = regression_data_test$log_price, test_matrix)

# --- --- Prediction Analysis --- ---
prediction_linear <- as.matrix(predict(linear_regression, newdata = regression_data_test))
prediction_ridge <- predict(ridge, s = min_lambda_ridge, newx = test_matrix)
prediction_lasso <- predict(lasso, s = min_lambda_lasso, newx = test_matrix)
prediction_boosting <- predict(boosting, newdata = data.frame(log_price = regression_data_test$log_price, test_matrix), n.trees = best_trees)

# Compute metrics
prediciton_comparison <- data.frame(
  model = character(),
  MSE = numeric(),
  RSS = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)
predicition_data_list <- c(prediction_linear = "linear", prediction_lasso = "lasso", prediction_ridge = "ridge", prediction_boosting = "boosting")
test_price <- regression_data_test[, target]
for (key in names(predicition_data_list)) {
  name <- predicition_data_list[[key]]
  data <- get(key)
  
  # Compute properties
  mse <- mean((data - test_price)^2)
  rmse <- round(sqrt(mse), 2)
  mape <- round(mean(abs((exp(data) - exp(test_price)) / exp(test_price))) * 100, 2)
  rss <- sum((data - test_price)^2)
  
  # Add data
  new_row <- list(model = name, MSE = mse, RSS = rss, RMSE = rmse, MAPE = mape)
  prediciton_comparison[nrow(prediciton_comparison) + 1,] <- new_row
}