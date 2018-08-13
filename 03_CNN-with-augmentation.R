library(tidyverse)
library(keras)

model <- keras_model_sequential() %>%
  layer_separable_conv_2d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(150, 150, 3)) %>%
  layer_separable_conv_2d(filters = 64, kernel_size = 3, activation = "relu") %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_separable_conv_2d(filters = 64, kernel_size = 3, activation = "relu") %>%
  layer_separable_conv_2d(filters = 128, kernel_size = 3, activation = "relu") %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_separable_conv_2d(filters = 64, kernel_size = 3, activation = "relu") %>%
  layer_separable_conv_2d(filters = 128, kernel_size = 3, activation = "relu") %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 18, activation = "softmax")

summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "rmsprop",
  metrics = c("accuracy")
)

#datagen <- image_data_generator(
#  rescale = 1/255,
#  rotation_range = 40,
#  width_shift_range = 0.2,
#  height_shift_range = 0.2,
#  shear_range = 0.2,
#  zoom_range = 0.2,
#  horizontal_flip = TRUE,
#  fill_mode = "nearest"
#)

train_datagen <- image_data_generator(rescale = 1/255)

test_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
  directory = "data/train",
  train_datagen,
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "categorical"
)

validation_generator <- flow_images_from_directory(
  directory = "data/validate",
  test_datagen,
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "categorical"
)

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 100,
  validation_data = validation_generator,
  validation_steps = 50
)

model %>% save_model_hdf5("models/womenswear_2.h5")

plot(history)

