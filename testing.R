library(caret)
model <- load_model_tf("modello_finale")


test_data_gen <- image_data_generator(rescale = 1/255)

library(magick)
df.test2 <- df.test[-c(which(str_detect(df.test[,2], "[^ -~]"))), ]

test_images <- flow_images_from_dataframe(
  dataframe = df.test2,
  directory = path_train,
  x_col = "file_name",
  y_col = "style",
  generator = test_data_gen,
  target_size = target_size,
  class_mode = "categorical",
  shuffle = F,
  classes = styles,
  seed = seed,
)

predictions <- model %>% predict(test_images)
pred <- apply(predictions, 1, which.max)

predicted_value <- factor(styles[pred])
expected_value <- factor(c(df.test2[, 1]))

#Creating confusion matrix
library(grid)
library(gridExtra)
library(likert)
cm <- confusionMatrix(data=predicted_value, reference = expected_value)
cm_d <- as.data.frame(cm$table) # extract the confusion matrix values as data.frame
cm_st <-data.frame(cm$overall) # confusion matrix statistics as data.frame
cm_st$cm.overall <- round(cm_st$cm.overall,2) # round the values
cm_d$diag <- cm_d$Prediction == cm_d$Reference # Get the Diagonal
cm_d$ndiag <- cm_d$Prediction != cm_d$Reference # Off Diagonal     
cm_d[cm_d == 0] <- NA # Replace 0 with NA for white tiles
cm_d$Reference <-  reverse.levels(cm_d$Reference) # diagonal starts at top left
cm_d$ref_freq <- cm_d$Freq * ifelse(is.na(cm_d$diag),-1,1)
plt1 <-  ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
  scale_x_discrete(position = "top") +
  geom_tile( data = cm_d,aes(fill = ref_freq)) +
  scale_fill_gradient2(guide = FALSE ,low="red3",high="orchid4", midpoint = 0,na.value = 'white') +
  geom_text(aes(label = Freq), color = 'black', size = 3)+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
  )
grid.arrange(plt1, plt2, nrow = 1, ncol = 2, 
             top=textGrob("Confusion Matrix",gp=gpar(fontsize=25,font=1)))
#Display results
cm

