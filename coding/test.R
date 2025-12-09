# install.packages("xml2")
# install.packages("jsonlite")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("glue")
# install.packages("glmnet")
# install.packages("rpart")
# install.packages("rpart.plot)
# install.packages("gbm")
# install.packages("usmap")
# install.packages("sf")
# install.packages("gridExtra")

library("xml2")
library("jsonlite")
library("ggplot2")
library("readxl")
library("glue")
library("glmnet")
library("rpart")
library("rpart.plot")
library("gbm")
library("usmap")
library("sf")
library("gridExtra")

# Function: Save a graph
save_figure <- function(name, plot, width = 7, height = 5, dpi = 300) {
  ggsave(
    filename = glue("images/", name, ".png"),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "transparent"
  )
}
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
state_region_map <- data.frame(
  state  = c(northeast, southeast, midwest, southwest, west),
  region = c(
    rep("Northeast", length(northeast)),
    rep("Southeast", length(southeast)),
    rep("Midwest",   length(midwest)),
    rep("Southwest", length(southwest)),
    rep("West",      length(west))
  ),
  stringsAsFactors = FALSE
)
rent_data$region <- "Other"
# Apply the mapping
for (region in unique(state_region_map$region)) {
  list <- get(tolower(region))
  rent_data[rent_data$state %in% list, "region"] <- region
}

# Who in the world uses feet
rent_data$square_feet <- rent_data$square_feet * 0.3048^2
names(rent_data)[names(rent_data) == "square_feet"] <- "square_meter"

# Predictors for regression (TODO: We could limit that to certain states, otherwise all 51)
# Define the columns of interest
regional_parameter <- "region" # Decide here on which variable it should rely
target <- "log_price"
regression_columns <- c("square_meter", "bedrooms", "bathrooms", regional_parameter)
# Numeric columns 
numeric_columns <- c("price", "log_price", "square_meter", "bedrooms", "bathrooms")
rent_data[numeric_columns] <- lapply(rent_data[numeric_columns], as.numeric)
numeric_regression_cols <- c(names(rent_data[, regression_columns])[sapply(rent_data[,regression_columns], is.numeric)], setdiff(names(rent_data[, numeric_columns]), regression_columns))

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
  actual = if (target == "log_price") exp(target_train) else target_train,
  fitted = if (target == "log_price") exp(predict(linear_regression)) else predict(linear_regression),
  residuals = if(target == "log_price") exp(target_train) - exp(predict(linear_regression)) else target_train - predict(linear_regression)
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
save_figure("plot_lm_parameter", plot_lm_parameter)
save_figure("plot_regression", plot_regression)

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
save_figure("plot_mininimal_lambda", plot_mininimal_lambda)

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
  mape <- if(target == "log_price") round(mean(abs((exp(data) - exp(test_price)) / exp(test_price))) * 100, 2)
  else round(mean(abs((data - test_price) / test_price)) * 100, 2)
  rss <- sum((data - test_price)^2)
  
  # Add data
  new_row <- list(model = name, MSE = mse, RSS = rss, RMSE = rmse, MAPE = mape)
  prediciton_comparison[nrow(prediciton_comparison) + 1,] <- new_row
}

# --- --- Map prediction for each state --- ---
# Found at (https://github.com/cran/usmap/blob/master/README.md)
states_in_model <- levels(regression_data_train[[regional_parameter]]) # Get states
base_setting <- data.frame(
  square_meter = 70,
  bedrooms = 2,
  bathrooms = 1,
  target = 0,
  dummy = states_in_model,
  stringsAsFactors = FALSE
)
names(base_setting)[names(base_setting) == "dummy"] <- regional_parameter
names(base_setting)[names(base_setting) == "target"] <- target
# --- Prediction ---
# Scale data
base_setting_scaled <- base_setting
prediction_numeric_cols <- setdiff(names(base_setting)[sapply(base_setting, is.numeric)], target)
base_setting_scaled[, prediction_numeric_cols] <- (base_setting[, prediction_numeric_cols] - mean(mean_std$mean[prediction_numeric_cols])) / mean(mean_std$std[prediction_numeric_cols])
base_setting_matrix <- model.matrix(regression_formula, data = base_setting_scaled)[, -1]

# Store prediction
pred_lin_price <- if (target == "log_price") exp(predict(linear_regression, newdata = base_setting)) else predict(linear_regression, newdata = base_setting)
pred_ridge_price <- if (target == "log_price") exp(predict(ridge, s = min_lambda_ridge, newx = base_setting_matrix)) else predict(ridge, s = min_lambda_ridge, newx = base_setting_matrix)
pred_lasso_price <- if (target == "log_price") exp(predict(lasso, s = min_lambda_lasso, newx = base_setting_matrix)) else predict(lasso, s = min_lambda_lasso, newx = base_setting_matrix)
pred_boosting_price <- if (target == "log_price") exp(predict(boosting, newdata = data.frame(log_price = 0, base_setting_matrix), n.trees = best_trees)) else predict(boosting, newdata = data.frame(log_price = 0, base_setting_matrix), n.trees = best_trees)
# Map data
state_data <- st_as_sf(us_map(regions = "states"))
names(state_data)[names(state_data) == "abbr"] <- "state"
# Add area and center points for plotting (https://gis.stackexchange.com/questions/287602/how-to-calculate-the-polygon-area-and-create-a-column-of-results-in-r)
state_data$area <- st_area(state_data) / 10e6
state_data$center <- st_centroid(state_data$geom)
state_data$x <- st_coordinates(state_data$center)[,1]
state_data$y <- st_coordinates(state_data$center)[,2]

# Make a list only containing big enough states
large_states <- state_data$state[state_data$area >= 0.8 *mean(state_data$area)]
state_predictions <- data.frame(
  dummy = states_in_model,
  price_linear = round(pred_lin_price, 2),
  price_ridge = round(pred_ridge_price, 2),
  price_lasso = round(pred_lasso_price, 2),
  price_boosting = round(pred_boosting_price, 2)
)
names(state_predictions)[names(state_predictions) == "dummy"] <- regional_parameter
if (regional_parameter == "region") {
  state_predictions <- merge(state_region_map, state_predictions, by = regional_parameter)
}  
state_predictions <- merge(state_predictions, state_data, by = "state")

# Plot
plot_price_method <- "price_linear"
state_predictions <- sort_by.data.frame(
  state_predictions,
  state_predictions[, "area"], decreasing = FALSE)
small_states_df <- state_predictions[!duplicated(state_predictions[[regional_parameter]]), ]
small_states <- tableGrob(small_states_df[ -(small_states_df$state %in% large_states), c(regional_parameter, plot_price_method)],
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 5,
    core = list(bg_params = list(fill = c("white", "#ABDEE6"), col = NA), fg_params = list(col = "black", fontface = "plain")),
    colhead = list(fg_params = list(col = "white", fontface = "bold"), bg_params = list(fill = "#002F5F"))
  ))
# Add Table to ggplot (https://stackoverflow.com/questions/12318120/adding-table-within-the-plotting-region-of-a-ggplot-in-r)
plot_table_small_states <- ggplot() + annotation_custom(small_states) + theme_minimal() +
  theme(plot.background  = element_rect(fill = "transparent", color = NA),panel.background = element_rect(fill = "transparent", color = NA)) 
plot_region_prediction <- plot_usmap(data = state_predictions, values = plot_price_method) +
  scale_fill_continuous(
    low = "white", high = "#002F5F",
    name = "Predicted Rent"
  ) +
  (if (regional_parameter == "state") {
  geom_text(
    data = state_predictions[state_predictions$state %in% large_states,],
    aes(x = x, y = y,
        label = paste0(state, "\n$", round(.data[[plot_price_method]], 0))),
    size = 1.5, color = "#3A3A3A"
    )}  else {NULL}) + 
  labs(
    title = glue("Predicted Rent by State ({plot_price_method}) "),
    subtitle = "Setting: 70sqm, 2 bedrooms, 1 bathroom"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom", legend.title.position = "top",legend.key.width = unit(1, "null"))

plot_region_table <- grid.arrange(
  plot_region_prediction,
  plot_table_small_states,
  ncol = 2,
  widths = c(3, 1)
)

print(plot_region_table)
save_figure(glue("plot_{regional_parameter}_table"), plot_region_table)