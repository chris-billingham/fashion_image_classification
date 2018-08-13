library(tidyverse)
library(keras)

conv_base <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(150, 150, 3)
)

conv_base

train_dir <- "data/train"
validation_dir <- "data/validate"
test_dir <- "data/test"

datagen <- image_data_generator(rescale = 1/255)
batch_size <- 20

extract_features <- function(directory, sample_count, num_class) {
  
  features <- array(0, dim = c(sample_count, 4, 4, 512))  
  labels <- array(0, dim = c(sample_count, num_class))
  
  generator <- flow_images_from_directory(
    directory = directory,
    generator = datagen,
    target_size = c(150, 150),
    batch_size = batch_size,
    class_mode = "categorical"
  )
  
  i <- 0
  while(TRUE) {
    batch <- generator_next(generator)
    inputs_batch <- batch[[1]]
    labels_batch <- batch[[2]]
    
    features_batch <- conv_base %>% predict(inputs_batch)
    
    index_range <- ((i * batch_size)+1):((i + 1) * batch_size)
    features[index_range,,,] <- features_batch
    labels[index_range] <- labels_batch
    
    i <- i + 1
    if (i * batch_size >= sample_count)
      # Note that because generators yield data indefinitely in a loop, 
      # you must break after every image has been seen once.
      break
  }
  
  list(
    features = features, 
    labels = labels
  )
}

train <- extract_features(train_dir, 100, 18)
validation <- extract_features(validation_dir, length(list.files(validation_dir, recursive = TRUE)))
test <- extract_features(test_dir, length(list.files(test_dir, recursive = TRUE)))


