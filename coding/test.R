# install.packages("xml2")
# install.packages("jsonlite")
# install.packages("ggplot2")
# install.packages("readxl")

library("xml2")
library("jsonlite")
library("ggplot2")
library("readxl")
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
#   df <- data.frame(read_excel(data_path))
# } else if (suffix == "csv") {
#   df <- read.csv(data_path)
# }
# head(df)

# Load data 
df <- data.frame(read_excel("data/Real estate valuation data set.xlsx"))
# Modify the data
column_names<- c("no", "transaction_date", "house_wage", "distance_metro", "distance_store", "latitude", "longitude", "house_price")
colnames(df) <- column_names
