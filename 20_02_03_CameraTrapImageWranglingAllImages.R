#Mari K Reeves
#2/3/20
#This code takes all images from the Mohihi Camera Trap study, shrinks them in size and saves them 
#in a single folder with file names that match the camera location and date, currently given by the 
#file structure. 

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

#install_tensorflow()

#Some helpful resources: https://shirinsplayground.netlify.com/2018/06/keras_fruits/ and https://blogs.rstudio.com/tensorflow/posts/2018-10-11-activations-intro/

rm(list = ls()) #remove all past worksheet variables

#Make Lists of All Training and Test Pics to subset

# 
set.seed(333)
#Set up workspace and identify data directories


basedir<-"C:/Users/marireeves/Documents/RatTrapFever/Data/MohExp"
outdir<-"C:/Users/marireeves/Documents/RatTrapFever/Data/OutDir/RawPhotosMohExp/"


#read in pictures as images, reduce resolution, write them out as smaller files, 
#Do this for all photos in the MohExp folder recursively
#assign their names as the file structure KFBRP had in place, which will ID photo
#by sampling date, trap location, and camera type. 
#clean up the file names a little bit to get rid of DCIM mess


#first, make a list of all the photo groups 

picturelist<-list.files(basedir, recursive = T, pattern = "JPG", full.names = T)

for (mypic in picturelist){
  mypics<-sub("jpg", "JPG", mypic)#capitalize jpg
  mypics<-na.omit(mypics)
  mypics<-gsub("/", "_", mypics)#get rid of slashes so I can send them all to one folder
  mypics<-sub("-", "_", mypics)#get rid of dashes becauase I suspect they might cause problems
  mypics<-sub(" ", "", mypics)#get rid of spaces because they are ugly
  mypics<-sub("DCIM", "", mypics)#get rid of extra DCIM folders
  mypics<-sub("100RECNX", "", mypics)#get rid of extra reconnex folders because they are also ugly
  mypics<-sub("C:_Users_marireeves_Documents_RatTrapFever_Data_MohExp_", 
              "C:/Users/marireeves/Documents/RatTrapFever/Data/OutDir/RawPhotosMohExp/", 
              mypics)#change to out directory name
  #start reading in the pics and resizing them
    pic <- readImage(mypic)
    pic<- resize(pic, 256, 256)#reduce the size
    picid<-as.character(mypics) #give smaller pic a name and write it out as a jpg
    picid<-sub(".JPG", "_256x256.jpg", picid)
    #picname<-sub(minifolder, "", picname)
    writeJPEG(pic, target = picid)
  }
 


