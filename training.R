#setwd("Desktop/Code/R")
#source("prepro.R")
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)


#Needed because of error in training
PIL <- import("PIL")
PIL$ImageFile$LOAD_TRUNCATED_IMAGES <- TRUE

#install_tensorflow(extra_packages = "pillow")
#install_keras(tensorflow = "gpu")
#Check if gpu is available
#tensorflow::tf_gpu_configured()

width <- 224
height <- 224
rgb <- 3
batch_size <- 128
output_n <- length(styles)
target_size <- c(width, height)


path_train <- file.path("wikiart", "wikiart/")

train_data_gen <- image_data_generator(rescale = 1/255,
                                       validation_split = .2,
                                       rotation_range = 40,
                                       width_shift_range = 0.2,
                                       height_shift_range = 0.2,
                                       shear_range = 0.2,
                                       zoom_range = 0.2,
                                       horizontal_flip = TRUE,
                                       fill_mode = "nearest")

train_images <- flow_images_from_dataframe(
  dataframe = df.train,
  directory = path_train,
  subset = "training",
  x_col = "file_name",
  y_col = "style",
  generator = train_data_gen,
  target_size = target_size,
  class_mode = "categorical",
  batch_size=batch_size,
  shuffle = F,
  classes = styles,
  seed = seed
)

#[Number of batch][[1 = data, 2 = classes vector]][Number of image, width, height, rgb channel]
#plot(as.raster(train_images[1][[1]][1,,,]))
par(mfrow = c(3, 3))
batch.number = 2
for(i in 1:9){
  plot(as.raster(train_images[[batch.number]][[1]][i,,,]))
}


validation_images <- flow_images_from_dataframe(
  dataframe = df.train,
  directory = path_train,
  subset = "validation",
  x_col = "file_name",
  y_col = "style",
  generator = train_data_gen,
  target_size = target_size,
  class_mode = "categorical",
  classes = styles,
  seed = seed
)


mod_base <- application_resnet50_v2(weights = "imagenet",
                                    include_top = FALSE, input_shape = c(width, height, 3))


#mod_base <- application_vgg16(weights = "imagenet",input_shape = c(width, height, 3),include_top = FALSE)


freeze_weights(mod_base)

model_function <- function(learning_rate = 0.001, 
                           dropoutrate = 0.15, 
                           n_dense=512){
  k_clear_session()
  
  model <- keras_model_sequential() %>%
    mod_base %>%
    layer_global_average_pooling_2d() %>%
    layer_dense(units= n_dense, activation = "relu") %>%
    layer_dropout(dropoutrate) %>%
    layer_dense(units=output_n, activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(learning_rate = learning_rate),
    metrics = "accuracy"
  )
  
  return(model)
}

model <- model_function()
model

early_stop <-callback_early_stopping(monitor = "val_loss", patience = 3)
log <- callback_csv_logger("training_history.csv", append=TRUE)
epochs <- 10
#hist <- model %>% fit(
#  train_images,
#  steps_per_epoch = train_images$n %/% batch_size,
#  epochs = epochs,
#  validation_data = validation_images,
#  validation_steps = validation_images$n %/% batch_size,
#  verbose = 1,
#  callbacks = list(log, early_stop)
#)

#save_model_tf(model, "modello_finale")
#save_model_tf(model, "sicuro")
#save_model_hdf5(model, "hdf_model")
#save_model_hdf5(model, "model23")
#save_model_weights_hdf5(model, "weights_hdf5")
#save_model_weights_tf(model, "weights_tf")

#2 ore di addestramento






