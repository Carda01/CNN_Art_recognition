library(ggplot2)
seed <- 101
set.seed(seed)
setwd("Desktop/Code/R")

#1 Artist: Artist Name
#2 Style: Style/Movement of the artwork with dates
#3 Genre: Genre of artwork
#4 Movement: Main assiocated movement to the artist
#5 Tags: Text tags associated with the artwork
#6 Url: Main original wikiart URL
#7 Img: Direct image URL
#8 File_Name: File name of the image file associated

#Utility to plot the histogram
histo_plotter <- function(df){
  ggplot(
    df, aes(x = factor(style))
  ) + geom_bar(
    fill = "coral", alpha = 0.5
  ) + theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1))
}


dataset <- read.csv("wikiart_art_pieces.csv")
num.records <- nrow(dataset)

#Remove all rows with Unknown class
dataset.reduced <- dataset[!dataset$style=="Unknown", ]

#Remove unnecessary columns
dataset.reduced <- dataset.reduced[, c(2,8)]

#Plot the histogram of the styles
histo_plotter(dataset)


#Remove all rows that have a class with a frequency lower than 1%
stat.frequency <- 0.025
styles <- unique(dataset.reduced[, "style"])
for(style in styles){
  f <- sum(dataset.reduced$style == style)/num.records
  if(f < stat.frequency){
    dataset.reduced <- dataset.reduced[!dataset.reduced$style == style, ] 
  }
}

histo_plotter(dataset.reduced)

styles <- unique(dataset.reduced[, "style"])

max.tuple <- 100000
dataset.shuffled <- dataset.reduced[sample(1:nrow(dataset.reduced)),]
df <- data.frame()
for(styled in styles){
  df<- rbind(df, head(subset(dataset.shuffled, style == styled),max.tuple))  
}

histo_plotter(df)

df <- df[sample(1:nrow(df)),]



#Split data into train and test
num.records <- nrow(df)
percentage.training = 0.7
sample <- sample.int(n=nrow(df), size=floor(percentage.training*nrow(df)), replace = F)
df.train <- df[sample, ]
df.test <- df[-sample, ]

#Move files into two different folders
#for(filename in df.train$file_name){
#  file.move(paste("wikiart/wikiart/", filename, sep=""), "wikiart/train")
#}
#for(filename in df.test$file_name){
#  file.move(paste("wikiart/wikiart/", filename, sep=""), "wikiart/test")
#}

