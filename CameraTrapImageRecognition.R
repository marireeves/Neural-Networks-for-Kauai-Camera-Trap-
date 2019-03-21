# Mari K Reeves
#Mess around with images and image recognition model2s
#Take files from folders for training and test data and run keras/tensorflow based neural network models on them 
#These ones use balanced data and 256*256 scaled photo arrays to fit binomial (presence/absence) models for rats
#in camera trap data photos from kauai. 

#code adapted from the following sources:
# https://github.com/tensorflow/tensorflow
# keras tutorial planes and cars: https://www.youtube.com/watch?v=iExh0qj2Ouo
# https://www.youtube.com/watch?v=aircAruvnKk
# https://tensorflow.rstudio.com/keras/
# One hot encoding explanation: https://hackernoon.com/what-is-one-hot-encoding-why-and-when-do-you-have-to-use-it-# e3c6186d008f


#Install EBImage

#Load Packages




# Read in Base Packages ---------------------------------------------------
pckg <- c("dplyr", "tidyr","RColorBrewer", "ggplot2","curl", 
          "RCurl","parallel","gdalUtils","rgdal","rgeos",  
          "spatial","sp", "raster", "maptools", "spatialEco", 
          "SpatialPack", "spatstat", "microbenchmark","jpeg", "knitr",
          "snowfall", "stringi", "stringr", "tictoc", "EBImage", "keras") 

# READING IN PACKAGES
for(i in 1:length(pckg)){
  if ((!pckg[i] %in% installed.packages())==T) {
    install.packages(pckg[i], repos="http://cran.us.r-project.org", 
                     dependencies = T)
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }else{
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }
}

library(keras)
install_keras()

#browseVignettes("EBImage")

rm(list = ls()) #remove all past worksheet variables

set.seed(333)

# Read images
basedir<-"C:/Users/marireeves/Documents/RatTrapFever/Data/"
setwd(basedir)
outdir<-"C:/Users/marireeves/Documents/RatTrapFever/Data/Animals/DataOut/"
#make list of training pics
trapdirTrain<-paste0(basedir, "Animals/Empty/")
trappics1 <- list.files(trapdirTrain, pattern = "JPG", full.names = T)
#randomly subsample 1000 trap pics - there are too many -
trappicsTrain<-sample(trappics1, 693, replace = F, prob = NULL)
ratdirTrain<-paste0(basedir, "Animals/Rats/")
ratpicsTrain <- list.files(ratdirTrain, pattern = "JPG", full.names = T)

#make list of test pics
trapdirTest<-paste0(basedir, "MD08/RNoRats/")
trappics2<-list.files(trapdirTest, pattern = ".JPG", full.names = T)
#randomly subsample 1000 trap pics - there are too many -
trappicsTest<-sample(trappics2, 130, replace = FALSE, prob = NULL) 

ratdirTest<-paste0(basedir, "MD08/RRats/")
ratpics2<-list.files(ratdirTest, pattern = ".JPG", full.names = T)
ratpicsTest<-sample(ratpics2, 130, replace = FALSE, prob = NULL) 


#some stuff for later. 
# #make list of people pics
# humandir<-paste0(basedir, "Animals/Humans/")
# peoplepics<-list.files(humandir)
# #make list of pig pics
# pigdir<-paste0(basedir, "Animals/Pig/")


#make lists of photos for the training and test sets






#make a list of these vectors for the function to operate on - waiting on function, getting code to run first in loops
# listofpics<-list(ratpicstrain, ratpicstest, trappicstrain, trappicstest)
# str(listofpics)
#write a function to read in pictures as images, reduce resolution, 
#write them out as smaller files, and transform them to arrays.


#for (mylist in listofpics) {
# createarray<-function(mypic){
rattest <- NULL
for (mypic in ratpicsTest){
  pic <- readImage(mypic)
  pic<- resize(pic, 256, 256)
  #give smaller pic a name and write it out as a jpg
  #blurpic<-paste0(outdir, as.character(mypic), "_28x28.jpg")
  #writeJPEG(pic, target = blurpic)
  # Reshape
  myarray <- array_reshape(pic, c(256, 256,3))
  # Row Bind arrays of photos
  rattest<- rbind(rattest, myarray)
  #trainx <- rbind(trainx, mypic[[i]])
  assign("rattest", rattest , envir = globalenv())
}
#}
str (rattest)
#take in some pics from the MD16 trap area just 100 for now, I'm going to see if it can just do
#binomial model2 and how it does - problem with this is it will cue on rats caught at other traps. But let's just see.

#doing this manually with for loops now because my brain can't get behind how to make it into a function right now.

traptest <- NULL
for (mypic in trappicsTest){
  pic <- readImage(mypic)
  pic<- resize(pic, 256, 256)
  #give smaller pic a name and write it out as a jpg
  #blurpic<-paste0(outdir, as.character(mypic), "_28x28.jpg")
  #writeJPEG(pic, target = blurpic)
  # Reshape
  myarray <- array_reshape(pic, c(256, 256,3))
  # Row Bind arrays of photos
  traptest<- rbind(traptest, myarray)
  #trainx <- rbind(trainx, mypic[[i]])
  assign("traptest", traptest , envir = globalenv())
}
#}
str (traptest)


rattrain <- NULL
for (mypic in ratpicsTrain){
  pic <- readImage(mypic)
  pic<- resize(pic, 256, 256)
  #give smaller pic a name and write it out as a jpg
  #blurpic<-paste0(outdir, as.character(mypic), "_28x28.jpg")
  #writeJPEG(pic, target = blurpic)
  # Reshape
  myarray <- array_reshape(pic, c(256, 256,3))
  # Row Bind arrays of photos
  rattrain<- rbind(rattrain, myarray)
  #trainx <- rbind(trainx, mypic[[i]])
  assign("rattrain", rattrain , envir = globalenv())
}
#}
str (rattrain)


traptrain <- NULL
for (mypic in trappicsTrain){
  pic <- readImage(mypic)
  pic<- resize(pic, 256, 256)
  #give smaller pic a name and write it out as a jpg
  #blurpic<-paste0(outdir, as.character(mypic), "_28x28.jpg")
  #writeJPEG(pic, target = blurpic)
  # Reshape
  myarray <- array_reshape(pic, c(256, 256,3))
  # Row Bind arrays of photos
  traptrain<- rbind(traptrain, myarray)
  #trainx <- rbind(trainx, mypic[[i]])
  assign("traptrain", traptrain , envir = globalenv())
}
#}
str (traptrain)


#create  training and test data from proportion of trap only and rat photos
#MARI update numbers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testx <- rbind(rattest, traptest)
testx<-testx/255 #scale
ratcodex<-rep(1, (nrow(rattest)))
trapcodex<- rep(0, nrow(traptest))
testy <- c(ratcodex, trapcodex)

trainx<-rbind(rattrain, traptrain)
trainx<-trainx/255
ratcodey<-rep(1, nrow(rattrain))
trapcodey<-rep(0, nrow(traptrain))
trainy <- c(ratcodey, trapcodey)

# One Hot Encoding
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)

# model2
model2 <- keras_model_sequential()
model2 %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(196608)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model2)

# Compile
model2 %>%
  compile(loss = "binary_crossentropy",
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy'))

# Fit model2
history <- model2 %>%
  fit(trainx,
      trainLabels,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

plot(history)

# Evaluation & Prediction - train data
model2 %>% evaluate(trainx, trainLabels)
pred <- model2 %>% predict_classes(trainx)
table(Predicted = pred, Actual = trainy)
prob <- model2 %>% predict_proba(trainx)
cbind(prob, Prected = pred, Actual= trainy)
#it is really not finding those rats! So why does it say it's getting such high accuracy?
predtest<-model2 %>% predict_classes(testx)
table(Predicted = predtest, Actual = testy)
probtest<- model2 %>% predict_proba(testx)
cbind(probtest, Predicted = predtest, Actual = testy)