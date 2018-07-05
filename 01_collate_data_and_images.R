library(tidyverse)
library(magrittr)
library(occamsrazR)
library(odbc)
library(rvest)
library(caret)


# get my teradata credentials
td_user <- Sys.getenv("teradata_user")
td_pass <- Sys.getenv("teradata_pass")

# helper function to read a .sql file
parse_sql_file <- function(path) {
  query <- readr::read_delim(path, "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, col_types = cols()) %>%
    dplyr::mutate(combined = paste(X1, collapse = " ")) %>%
    .[1,2] %>%
    as.character() %>%
    tolower() 
  return(query)
}

# download the data
product_data <- td_download(parse_sql_file("get_products_all.sql"), td_user, td_pass)

# clean up the data
colnames(product_data) <- tolower(colnames(product_data))
product_data$product_number <- tolower(product_data$product_number)
product_data$category <- tolower(product_data$category) %>% 
  gsub(" ", "_", .)
product_data$product_image_url <- tolower(product_data$product_image_url)
product_data$merch_fmb_desc <- tolower(product_data$merch_fmb_desc)

# save the data, it be big
saveRDS(product_data, "data/product_data.rds")

# read in the scraped data from before
ww_img_links <- readRDS("data/ww_img_links.rds")

# first get a list of the products not from our scrape for womenswear which have the info we need
no_scrape <- product_data %>%
  filter(merch_fmb_desc == "womenswear", !is.na(product_image_url), !is.na(category)) %>%
  anti_join(ww_img_links, by = c("product_number" ="prod_num"))

# clean up the product url
text <- no_scrape$product_image_url %>%
  gsub("://", "", .) %>%
  gsub("//", "/", .) %>%
  gsub("thumb", "", .)

end <- text %>%
  str_locate("products/") %>%
  .[,2]+1

no_scrape$image_url <- paste0("https://images2.drct2u.com/main_product/products/", substr(text, end, nchar(text)))
no_scrape$image_url <- trimws(no_scrape$image_url)

# letters determine whether its just product or includes model (there is a system)
no_scrape$letter <- substr(no_scrape$image_url, nchar(no_scrape$image_url)-4, nchar(no_scrape$image_url)-4)

# validates whether the image_url has the expected pattern. if not i'll have to deal with that later.
no_scrape$valid <- grepl("[a-z]{1,2}[0-9]{2}[a-z]{2}[0-9]{5,6}[a-z].jpg", no_scrape$image_url)

# extract the file name. let's use the handy regexp from before
no_scrape$filename <- substr(no_scrape$image_url, 59, nchar(no_scrape$image_url))

# join scrape data onto product data
ww_img_links_join <- ww_img_links %>% 
  inner_join(product_data, by = c("prod_num" = "product_number"))

scraped <- ww_img_links_join[,c(5,2,3,6,8,9,4)] 
not_scraped <- no_scrape[,c(1,6,7,9,3,4,8)]
colnames(scraped) <- colnames(not_scraped)

# combine them together into a masterlist
master_list <- bind_rows(scraped, not_scraped) %>%
  unique()

# save off this master list
saveRDS(master_list, "data/master_list.rds")

master_list <- master_list %>%
  filter(!grepl("content", image_url))

# lets get down to our scrape list, choose top 22 categories
categories <- master_list %>%
  filter(merch_fmb_desc == "womenswear") %>%
  group_by(category) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(22)

image_links <- master_list %>%
  semi_join(categories)

# divvy the data into 60/20/20
splits <- createDataPartition(image_links$category, p = 0.6, list = FALSE)

train_data <- image_links[splits,]
valid_test <- image_links[-splits,]

splits_v <- createDataPartition(valid_test$category, p = 0.5, list = FALSE)

valid_data <- valid_test[splits_v, ]
test_data <- valid_test[-splits_v, ]

# create the directories for the data
if(!dir.exists("images/train")){dir.create("images/train")}
if(!dir.exists("images/test")){dir.create("images/test")}
if(!dir.exists("images/validate")){dir.create("images/validate")}

# iterate through all the train data and download
# using a for loop here becuase i want some progress please and pbapply can't mapply
start <- Sys.time()

for (row in 15641:nrow(train_data)) {
  url <- train_data$image_url[row]
  product_number <- train_data$product_number[row]
  category <- train_data$category[row]
  filename <- train_data$filename[row]
  data_type <- "train"
  directory <- paste0("images/", data_type, "/", category)
  if(!dir.exists(directory)){dir.create(directory)}
  
  dest_file <- paste0(directory, "/", filename)
  
  tryCatch({download.file(url, destfile = dest_file, quiet = TRUE, mode =
                            "wb")}, silent = FALSE, condition = function(err) { } )
  
  if(file.size(dest_file) == 0 || is.na(file.size(dest_file)))
  {
    print(paste0(row, "/", nrow(train_data)," - ", product_number, " failed image download. Skipping..."))
  } else {
    print(paste0(row, "/", nrow(train_data)," - ", product_number, " image downloaded. ", round(Sys.time()-start, digits = 2)))
  }
}

for (row in 1:nrow(test_data)) {
  url <- test_data$image_url[row]
  product_number <- test_data$product_number[row]
  category <- test_data$category[row]
  filename <- test_data$filename[row]
  data_type <- "test"
  directory <- paste0("images/", data_type, "/", category)
  if(!dir.exists(directory)){dir.create(directory)}
  
  dest_file <- paste0(directory, "/", filename)
  
  tryCatch({download.file(url, destfile = dest_file, quiet = TRUE, mode =
                            "wb")}, silent = FALSE, condition = function(err) { } )
  if(file.size(dest_file) == 0)
  {
    print(paste0(row, "/", nrow(test_data)," - ", product_number, " failed image download. Skipping..."))
  } else {
    print(paste0(row, "/", nrow(test_data)," - ", product_number, " image downloaded. ", round(Sys.time()-start, digits = 2)))
  }
}

for (row in 1:nrow(valid_data)) {
  url <- valid_data$image_url[row]
  product_number <- valid_data$product_number[row]
  category <- valid_data$category[row]
  filename <- valid_data$filename[row]
  data_type <- "validate"
  directory <- paste0("images/", data_type, "/", category)
  if(!dir.exists(directory)){dir.create(directory)}
  
  dest_file <- paste0(directory, "/", filename)
  
  tryCatch({download.file(url, destfile = dest_file, quiet = TRUE, mode =
                            "wb")}, silent = FALSE, condition = function(err) { } )
  if(file.size(dest_file) == 0)
  {
    print(paste0(row, "/", nrow(valid_data)," - ", product_number, " failed image download. Skipping..."))
  } else {
    print(paste0(row, "/", nrow(valid_data)," - ", product_number, " image downloaded. ", round(Sys.time()-start, digits = 2)))
  }
}


