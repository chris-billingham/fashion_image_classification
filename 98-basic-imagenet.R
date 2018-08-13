library(tidyverse)
library(keras)

# loads in imagenet
model <- application_resnet50(weights = 'imagenet')

# read in the image at imagenet res
image_path <- "images/weird_face.jpg"
img <- image_load(image_path, target_size = c(224, 224))

# preprocess
img_tensor <- image_to_array(img)
img_tensor <- array_reshape(img_tensor, c(1, 224, 224, 3))
photo_processed <- imagenet_preprocess_input(img_tensor)

# run through imagenet model
preds <- (model %>%
            predict(photo_processed) %>%
            imagenet_decode_predictions(top = 8))[[1]]

# visualise that sucker
preds %>% 
  mutate(class_description = str_replace(class_description, '_', ' ')) %>%
  ggplot(aes(x = reorder(class_description, score),
             y = score)) +
  coord_flip() +
  geom_pointrange(aes(ymin = 0, ymax = score))
  
