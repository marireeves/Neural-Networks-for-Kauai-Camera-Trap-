#WARNING DOES NOT WORK...FAILS AT FIT SECTION WITH THE FOLLOWING ERROR:
# Error in py_call_impl(callable, dots$args, dots$keywords) : 
 # ImportError: Could not import PIL.Image. The use of `array_to_img` req

---
  title: "Deep Neural Network Photo Analysis"
author:  "[Mari K Reeves](https://github.com/marireeves)"
date: "`r Sys.Date()`"
output: html_document
---
  #   
  #   {r setup, include=FALSE, root.dir = "C:/Users/marireeves/Documents/RatTrapFever/Data/"}
  # knitr::opts_chunk$set(echo = TRUE)
  # 
  # 
  # {r global_options, echo=FALSE, message=FALSE}
  # knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path='Figs/',
  #                       echo=TRUE, warning=FALSE, message=FALSE)
  rm(list = ls()) #remove all past worksheet variables

#Load Packages
# Read in Base Packages ---------------------------------------------------
pckg <- c("dplyr", "tidyr","RColorBrewer", "ggplot2","curl", "caret",
          "RCurl","parallel","snowfall","rmarkdown","jpeg", "knitr",
          "tictoc", "EBImage", "keras","tensorflow", "magick")

# READING IN PACKAGES
for(i in 1:length(pckg)){
  if ((!pckg[i] %in% installed.packages())==T) {
    if(pckg[i] == 'EBImage'){
      if (!requireNamespace("BiocManager", quietly = TRUE)){
        install.packages("BiocManager", repos="http://cran.us.r-project.org")
        BiocManager::install("EBImage")
      }
      
    } 
    install.packages(pckg[i], repos="http://cran.us.r-project.org",
                     dependencies = T)
    print(pckg[i])
    do.call("library", list(pckg[i]))
    
    if(pckg[i] == "tensorflow"){
      install_tensorflow()
    }
  }else{
    
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }
}

install_tensorflow()

#Some helpful resources: https://shirinsplayground.netlify.com/2018/06/keras_fruits/ and https://blogs.rstudio.com/tensorflow/posts/2018-10-11-activations-intro/

#Set up workspace and identify data directories


basedir<-"C:/Users/marireeves/Documents/RatTrapFever/Data/"
outdir<-"C:/Users/marireeves/Documents/RatTrapFever/Data/OutDir/"
traindir<-"C:/Users/marireeves/Documents/RatTrapFever/Data/19_07_11_Out/Training/"
testdir<-"C:/Users/marireeves/Documents/RatTrapFever/Data/19_07_11_Out/Validation/"


#Make Lists of All Training and Test Pics to subset

# 
set.seed(333)
#for empty frames
trapdir<-paste0(basedir, "Animals/Empty/")

#and for photos with rats
ratdir<-paste0(basedir, "Animals/Rats/")

#and for photos with mice
mousedir<-paste0(basedir, "Animals/Mice/")

#with pigs
pigdir<-paste0(basedir, "Animals/Pigs/")

#humans - not enough now
#peopledir<-paste0(basedir, "Animals/Humans/")

#birds
birdir<-paste0(basedir, "Animals/Birds/")


#read in pictures as images, reduce resolution, write them out as smaller files, and transform them to arrays
#Do this for the training and test sets for the rats and the empty frames


#Send photos to training and validation folders randomly
#first, make a list of all the photo groups 

folderlist<-c(trapdir, ratdir, mousedir, pigdir,  birdir)#peopledir,

for (myfolder in folderlist){
  
  mypics <- list.files(myfolder, pattern = "JPG" , full.names = T)
  mypics<-sample(mypics, 102, replace = T)#when more pics, set replace to F
  mypics<-na.omit(mypics)
  trainsize<-round(length(mypics)*0.8, digits = 0)
  trainindex<-sample(mypics, trainsize, replace = F)
  testindex<-mypics[!(mypics %in% trainindex)] 
  #give things names based on the name of the infolders
  foldername<-as.character(myfolder)
  minifolder<-sub("C:/Users/marireeves/Documents/RatTrapFever/Data/Animals/", "", myfolder)
  foldername<-sub("C:/Users/marireeves/Documents/RatTrapFever/Data/Animals/", "", myfolder)
  foldername<-sub("/", "_", foldername)
  testname<-paste0(foldername, "test")
  trainname<-paste0(foldername, "train")
  afolder<-paste0( as.character(myfolder)) 
  trainfolder<-sub("C:/Users/marireeves/Documents/RatTrapFever/Data/Animals/", traindir, afolder)
  testfolder<-sub("C:/Users/marireeves/Documents/RatTrapFever/Data/Animals/", testdir, afolder)
  #make this send photos to training and validation folders
  
  for (testpic in testindex){
    pic <- readImage(testpic)
    pic<- resize(pic, 256, 256)
    #give smaller pic a name and write it out as a jpg
    picid<-as.character(testpic)
    #This sub statement is not working!
    picid<-sub(afolder, "", picid)
    picname<-sub(".JPG", "_256x256.jpg", picid)
    #picname<-sub(minifolder, "", picname)
    outpic<-sub(afolder, testfolder, picname)
    outpic<-paste0(testfolder, outpic)
    writeJPEG(pic, target = outpic)
  }
  
  for (trainpic in trainindex){
    tpic<-readImage(trainpic)
    tpic<-resize(tpic, 256, 256)
    pictid<-as.character(trainpic)
    pictname<-sub(afolder, "", pictid)
    picntname<-sub(".JPG", "_256x256.jpg", pictid)
    outpict<-sub(afolder, trainfolder, pictname)
    outpict<-paste0(trainfolder, outpict)
    writeJPEG(tpic, target = outpict)
  }
}

# list of animals

names_list <- c("Empty", "Birds", "Rats", "Mice", "Pigs")

# number of output classes (i.e. fruits)
output_n <- length(names_list)

# image size to scale down to (original images are 100 x 100 px)
img_width <- 256
img_height <- 256
target_size <- c(img_width, img_height)

# RGB = 3 channels
channels <- 3

# path to image folders
train_image_files_path <-traindir 
valid_image_files_path <-testdir


#Set up the image data generator functions


# optional data augmentation
train_data_gen <- image_data_generator(traindir,
                                       rescale = 1/255 #,
                                       #rotation_range = 40,
                                       #width_shift_range = 0.2,
                                       #height_shift_range = 0.2,
                                       #shear_range = 0.2,
                                       #zoom_range = 0.2,
                                       #horizontal_flip = TRUE,
                                       #fill_mode = "nearest"
)

# Validation data shouldn't be augmented! But it should also be scaled.
valid_data_gen <- image_data_generator(testdir,
                                       rescale = 1/255
)  


#load images into memory and resize them
#use_condaenv("r-tensorflow")

# training images
train_image_array_gen <- flow_images_from_directory(train_image_files_path, 
                                                    train_data_gen,
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = names_list,
                                                    seed = 333)

# validation images
valid_image_array_gen <- flow_images_from_directory(valid_image_files_path, 
                                                    valid_data_gen,
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = names_list,
                                                    seed = 333)



cat("Number of images per class:")

table(factor(train_image_array_gen$classes))

cat("\nClass label vs index mapping:\n")

train_image_array_gen$class_indices

names_classes_indices <- train_image_array_gen$class_indices
save(names_classes_indices, file = paste0(basedir, "names_classes_indices.RData") )



# number of training samples
train_samples <- train_image_array_gen$n
# number of validation samples
valid_samples <- valid_image_array_gen$n

# define batch size and number of epochs
batch_size <- 32
epochs <- 10




#set model parameters and run fit model
#Use this next line if you don't want the model to keep adding new layers each time you
#run the code:
#k_clear_session()

# initialise model
model <- keras_model_sequential()

# add layers
model %>%
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(img_width, img_height, channels)) %>%
  layer_activation("relu") %>%
  
  # Second hidden layer
  layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  
  # Flatten max filtered output into feature vector 
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(100) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  
  # Outputs from dense layer are projected onto output layer
  layer_dense(output_n) %>% 
  layer_activation("softmax")

# compile
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
  metrics = "accuracy"
)





# fit

hist <- model %>% fit_generator(
  # training data
  train_image_array_gen,
  
  # epochs
  steps_per_epoch = as.integer(train_samples / batch_size),
  epochs = epochs,
  
  # validation data
  validation_data = valid_image_array_gen,
  validation_steps = as.integer(valid_samples / batch_size),
  
  # print progress
  verbose = 1,
  callbacks = list(
    # save best model after every epoch
    callback_model_checkpoint("C:/Users/marireeves/Documents/RatTrapFever/Data/OutDir/fruits_checkpoints.h5", save_best_only = TRUE),
    # only needed for visualising with TensorBoard
    callback_tensorboard(log_dir = "C:/Users/marireeves/Documents/RatTrapFever/Data/OutDir/keras/logs")
  )
)




