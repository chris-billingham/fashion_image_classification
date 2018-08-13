library(tidyverse)
library(keras)

model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, 
                kernel_size = c(3, 3), 
                activation = "relu", 
                input_shape = c(150, 150, 3)) %>%
  layer_conv_2d(filters = 32, 
                kernel_size = c(3, 3), 
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, 
                kernel_size = c(3, 3), 
                activation = "relu") %>%
  layer_conv_2d(filters = 64, 
                kernel_size = c(3, 3), 
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = "relu") %>%
#  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 22, activation = "softmax")

summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_sgd(lr = 0.01,
                            decay = 1e-6,
                            momentum = 0.9,
                            nesterov = T),
  metrics = c("accuracy")
)

train_datagen <- image_data_generator(rescale = 1/255)
validation_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
  directory = "images/train",
  generator = train_datagen,
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "categorical"
)

validation_generator <- flow_images_from_directory(
  directory = "images/validate",
  generator = validation_datagen,
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "categorical"
)

# batch <- generator_next(train_generator)

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 100,
  validation_data = validation_generator,
  validation_steps = 50
)

model %>% save_model_hdf5("models/womenswear_1.h5")

plot(history)

