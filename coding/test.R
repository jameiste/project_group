# install.packages("xml2")
# install.packages("jsonlite")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("glue")
# install.packages("glmnet")
# install.packages("rpart")
# install.packages("rpart.plot)

library("xml2")
library("jsonlite")
library("ggplot2")
library("readxl")
library("glue")
library("glmnet")
library("rpart")
library("rpart.plot")
# Read the UCI dataset page
# url_page <- "https://archive.ics.uci.edu/dataset/477/real+estate+valuation+data+set"
# page <- read_html(url_page)

# links <- xml_find_all(page, ".//a")
# hrefs <- xml_attr(links, "href")

# # Pick the first link ending with ".data"
# idx <- grep("\\.zip$", hrefs)
# if (length(idx) == 0) stop("No .data link found.")
# zip_href <- hrefs[idx[1]]

# # Full URL
# base <- "https://archive.ics.uci.edu"
# if (!grepl("^https?://", zip_href)) {
#   data_url <- paste0(base, zip_href)
# } else {
#   data_url <- zip_href
# }
# # Download zip
# tmp <- tempfile(fileext = ".zip") # temporary store
# download.file(data_url, tmp, mode = "wb")
# files <- unzip(tmp, list = TRUE,)

# # Get data
# unzip(tmp, exdir = tempdir())
# data_path <- file.path(tempdir(), files$Name[1])
# suffix <- tools::file_ext(data_path)
# if (suffix == "xlsx") {
#   real_estate <- data.frame(read_excel(data_path))
# } else if (suffix == "csv") {
#   real_estate <- read.csv(data_path)
# }
# head(df)

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


# Predictors for regression (TODO: We could limit that to certain states, otherwise all 51)
regression_columns <- c("square_feet", "bedrooms", "bathrooms", "latitude", "state", "longitude")
# Numeric columns 
numeric_columns <- c("price", "log_price", "square_feet", "bedrooms", "bathrooms", "latitude", "longitude")
rent_data[numeric_columns] <- lapply(rent_data[numeric_columns], as.numeric)
target <- "log_price"
numeric_regression_cols <- setdiff(names(rent_data)[sapply(rent_data, is.numeric)],c(target, "price"))

# Handle state as categorical
# rent_data <- rent_data[!(rent_data$state == names(sort(table(rent_data$state)))[1]),] # kick out the state with least entries (dummy-variable trap)
regression_data <- rent_data
regression_data$state <- as.factor(regression_data$state)

# Split the data in test and training data
ratio_train <- 0.8
amount_rows <- nrow(regression_data)
train_indices <- sample(1:amount_rows, round(amount_rows * ratio_train))
# Divide data sets
regression_data_train <- regression_data[train_indices,]
regression_data_test <- regression_data[-train_indices, ]
target_train <- regression_data[train_indices, target]
target_test <- regression_data[-train_indices, target]
# Re-scale the data
mean_std <- list(
  mean = sapply(regression_data_train[, numeric_regression_cols], mean),
  std  = sapply(regression_data_train[, numeric_regression_cols], sd)
) # List storing mean and std for standardization

# Apply scaling on training and test data
regression_data_train[, numeric_regression_cols] <- (regression_data_train[, numeric_regression_cols] - mean_std$mean) / mean_std$std
regression_data_test[, numeric_regression_cols] <- (regression_data_test[, numeric_regression_cols] - mean_std$mean) / mean_std$std

# Keep only those states that are in train (error at prediciton otherwise)
train_states <- unique(regression_data_train$state)
regression_data_train <- regression_data_train[regression_data_train$state %in% train_states, ]
regression_data_test <- regression_data_test[regression_data_test$state %in% train_states, ]

# Build regression formula
regression_formula <- as.formula(glue("{target} ~ {paste(regression_columns, collapse = ' + ')}"))
# Design matrix
regression_matrix <- model.matrix(regression_formula, data = regression_data_train)[, -1]
test_matrix <- model.matrix(regression_formula, data = regression_data_test)[, -1]
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
cv_ridge <- cv.glmnet(regression_matrix, target_array, alpha = 0, nfolds = nrow(regression_matrix))
cv_lasso <- cv.glmnet(regression_matrix, target_array, alpha = 1, nfolds = nrow(regression_matrix))
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
# There are too many states, hence, separate them in 5 regions (https://www.jagranjosh.com/general-knowledge/regions-of-united-states-complete-list-history-and-importance-1721218579-1)
northeast <- c("ME","NH","VT","MA","RI","CT","NY","PA","NJ")
southeast <- c("DE","MD","DC","VA","WV","NC","SC","GA","FL","KY","TN","AL","MS","AR","LA")
midwest <- c("OH","MI","IN","IL","WI","MN","IA","MO","ND","SD","NE","KS")
southwest <- c("TX","OK","NM","AZ")
west <- c("CA","NV","UT","CO","WY","MT","ID","OR","WA","AK","HI") 
regression_data$region <- "Other"
region_list = c("Northeast", "Southeast","Midwest", "Southwest", "West")
# Apply the mapping
for (region in region_list) {
  list <- get(tolower(region))
  regression_data[regression_data$state %in% list, "region"] <- region
}

# Tree
tree_columns <- c("square_feet", "bedrooms", "bathrooms", "region")
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
# --- --- Prediction Analysis --- ---
prediction_linear <- as.matrix(predict(linear_regression, newdata = regression_data_test))
prediction_ridge <- predict(ridge, s = min_lambda_ridge, newx = test_matrix)
prediction_lasso <- predict(lasso, s = min_lambda_lasso, newx = test_matrix)

# Compute metrics
prediciton_comparison <- data.frame(
  model = character(),
  MSE = numeric(),
  RSS = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)
predicition_data_list <- c(prediction_linear = "linear", prediction_lasso = "lasso", prediction_ridge = "ridge")
test_price <- regression_data_test$price
for (key in names(predicition_data_list)) {
  name <- predicition_data_list[[key]]
  log_data <- get(key)
  data <- exp(log_data)
  
  # Compute properties
  mse <- mean((data - test_price)^2)
  rmse <- round(sqrt(mse), 2)
  mape <- round(mean(abs((data - test_price) / test_price)) * 100, 2)
  rss <- sum((data - test_price)^2)
  
  # Add data
  new_row <- list(model = name, MSE = mse, RMSE = rmse, MAPE = mape, RSS = rss)
  prediciton_comparison[nrow(prediciton_comparison) + 1,] <- new_row
}

