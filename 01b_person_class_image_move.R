library(tidyverse)
library(caret)

# bring in the master list from the data directory
master_list <- readRDS("data/master_list.rds")

# mark whether a product is a person or just the product
master_list <- master_list %>% 
  mutate(person = case_when(letter %in% c("c", "s") ~ "product", TRUE ~ "person"))

# get a list of all the images 
all_images_path <- list.files(path = "images",
                              recursive = TRUE) %>%
  as.tibble()

# append person or product for first model, grab the cateogry
all_images_move <- all_images_path %>% 
  left_join(master_list[,c(4,6,8)], by = c("value" = "filename")) %>%
  mutate(old_path = paste0("images/", value))

# summarise, remove low volume categories, those under 10 and where one of prod-person under 100
summary <- all_images_move %>% 
  group_by(person, category) %>%
  summarise(n = n()) %>%
  filter(n > 10) %>%
  filter(!category %in% c("jeggings","tunic_dresses")) %>%
  filter(person == "person")

# cut down the move file
all_images_move <- all_images_move %>% 
  inner_join(summary) 

# get all possible combinations
grid <- expand.grid(c("train", "test", "valid"), summary$category, stringsAsFactors = FALSE)

# create directories if the don't exist
if(!dir.exists("modelling_images/person_classification/train")){dir.create("modelling_images/person_classification/train")}
if(!dir.exists("modelling_images/person_classification/valid")){dir.create("modelling_images/person_classification/valid")}
if(!dir.exists("modelling_images/person_classification/test")){dir.create("modelling_images/person_classification/test")}

# iterate through all combinations to build directory structure
directory_structure <- paste0("modelling_images/person_classification/", grid$Var1, "/", grid$Var2) %>%
  map(~ (if(!dir.exists(.)){dir.create(.)}))

# split into train, validate, test 85/10/5
splits <- createDataPartition(all_images_move$category, p = 0.85, list = FALSE)

images_train <- all_images_move[splits,]
valid_data <- all_images_move[-splits,]

images_train <- images_train %>%
  mutate(new_path = paste0("modelling_images/person_classification/train/", category, "/", value))

splits_v <- createDataPartition(valid_data$person, p = 0.66, list = FALSE)

images_valid <- valid_data[splits_v, ]
images_test <- valid_data[-splits_v, ]

images_valid <- images_valid %>%
  mutate(new_path = paste0("modelling_images/person_classification/valid/", category, "/", value))
images_test <- images_test %>%
  mutate(new_path = paste0("modelling_images/person_classification/test/", category, "/", value))

# move files into appropriate directory
file.copy(images_train$old_path, images_train$new_path)
file.copy(images_valid$old_path, images_valid$new_path)
file.copy(images_test$old_path, images_test$new_path)

