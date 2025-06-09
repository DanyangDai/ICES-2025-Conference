## This script is for online scraping 

library(httr)
library(jsonlite)
library(dplyr)


search_term <- "milk"


url <- paste0("https://www.woolworths.com.au/apis/ui/Search/products?searchTerm=", search_term)

res <- GET(url)

data <- content(res, as = "text", encoding = "UTF-8")

json <- fromJSON(data)

products <- json$Products



# Build dataframe
df <- data.frame(
  name = sapply(products, function(x) x$Products),
  price = sapply(products, function(x) x$Price),
  stringsAsFactors = FALSE
)


