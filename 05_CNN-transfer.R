library(keras)

train_directory <- "data/train"
test_directory <- "data/validate"

datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(train_directory, 
                                              generator = datagen,
                                              target_size = c(150, 150),
                                              class_mode = "categorical", 
                                              batch_size = 20
                                              )

validation_generator <- flow_images_from_directory(test_directory, 
                                                   generator = datagen,
                                                   target_size = c(150, 150),
                                                   class_mode = "categorical", 
                                                   batch_size = 20
                                                   )


################### Section 2 #########################
#base_model <- application_inception_v3(weights = 'imagenet', include_top = FALSE)
base_model <- application_vgg16(weights = 'imagenet', include_top = FALSE)
### use vgg16 -  as inception won't converge --- 

################### Section 3 #########################
## add your custom layers
predictions <- base_model$output %>% 
  layer_global_average_pooling_2d(trainable = T) %>% 
  layer_dense(64, trainable = T) %>%
  layer_activation("relu", trainable = T) %>%
  layer_dropout(0.4, trainable = T) %>%
  layer_dense(18, trainable=T) %>%    ## important to adapt to fit the 27 classes in the dataset!
  layer_activation("softmax", trainable=T)
# this is the model we will train
model <- keras_model(inputs = base_model$input, outputs = predictions)

################### Section 4 #########################
for (layer in base_model$layers)
  layer$trainable <- FALSE

################### Section 5 #########################
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.003, decay = 1e-6),  ## play with the learning rate
  metrics = "accuracy"
)

hist <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100, 
  epochs = 20, 
  validation_data = validation_generator,
  validation_steps = 50
)

