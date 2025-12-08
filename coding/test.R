# install.packages("xml2")
# install.packages("jsonlite")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("glue")
# install.packages("glmnet")

library("xml2")
library("jsonlite")
library("ggplot2")
library("readxl")
library("glue")
library("glmnet")
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
# --- Prepare data for regression ---
rent_data$log_price <- log(rent_data$price)
rent_data[rent_data == "null"] <- NA
rent_data[rent_data == ""] <- NA
# Drop rows with NA (inspiration: https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame)
rent_data <- rent_data[complete.cases(rent_data),]

# Modify the data
columns_of_interest <- c("price", "log_price", "square_feet", "bedrooms", "bathrooms", "cityname", "state", "longitude", "latitude")
rent_data <- rent_data[, columns_of_interest]


# Predictors for regression
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
regression_data[, numeric_regression_cols] <- scale(regression_data[, numeric_regression_cols])

# Build regression formula
regression_formula <- as.formula(glue("{target} ~ {paste(regression_columns, collapse = ' + ')}"))
# Design matrix
regression_matrix <- model.matrix(regression_formula, data = regression_data)[, -1]
target_array <- as.numeric(regression_data[, target])

# --- Linear regression ---
linear_regression <- lm(regression_formula, data = regression_data)
coefficients_linear <- coef(linear_regression)
linear_regression_coefficient <- data.frame(
  parameter = rownames(summary(linear_regression)$coefficients),
  coefficient = summary(linear_regression)$coefficients[, "Estimate"],
  variance = summary(linear_regression)$coefficients[, "Std. Error"],
  row.names  = NULL
)
fitted_values_df <- data.frame(
  actual = rent_data$price,
  fitted = exp(predict(linear_regression)),
  residuals = rent_data$log_price - (predict(linear_regression))
)
# Plot results
l_r_coef_inte <- linear_regression_coefficient[linear_regression_coefficient$parameter != "(Intercept)",] # Kick out the intercept
plot_lm_parameter <- ggplot(l_r_coef_inte, aes(y = parameter)) +
  geom_point(aes(x = coefficient, color = "Coefficient"), size = 2) +
  geom_errorbar(aes(x = coefficient, y = parameter, xmin = coefficient - variance, xmax = coefficient + variance),  color = "#6D6E71", width = 0.3, alpha = 0.5) +
  scale_color_manual( values = c("Coefficient"= "#002F5F" )) +
  labs(x = "Value", y = "Paramter", color = "Regression") + theme_light() +
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

# Regression
ridge <- glmnet(regression_matrix, target_array, alpha = 0, standardize = TRUE)
lasso <- glmnet(regression_matrix, target_array, alpha = 1, standardize = TRUE)

# Analyze regressions
# Take the minimum for each ticker
coef_ridge <- as.matrix(coef(cv_ridge, s = "lambda.min"))
coef_lasso <- as.matrix(coef(cv_lasso, s = "lambda.min"))

shrinkage_coef <- data.frame(
  parameter = rownames(coef_ridge),
  coef_ridge = coef_ridge[, 1],
  coef_lasso = coef_lasso[, 1],
  row.names = NULL
)

# Plot
shrinkage_coef_plot <- shrinkage_coef[shrinkage_coef$parameter != "(Intercept)",]
plot_mininimal_lambda <- ggplot(shrinkage_coef_plot, aes(y = parameter)) +
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
