###########################################
#    EVE 105 Coral Reef Fishes Project    #
#   Digitizing Body Shape in Stereomorph  #
###########################################

#First, you will need to tell R what folder you are working out of
#You do that with the command "setwd"
#Here's an example, where my "stereomorph" folder is in a folder named "EVE_105" on my desktop
setwd("~/Desktop/EVE_105/Stereomorph/") #I have a Mac, this will look a bit different if you have a PC
#Change this to match the location of your folder!

#You can check to make sure that it worked using getwd()
getwd()

#You will need to install the package "stereomorph"
install.packages("StereoMorph") #You only need to run this line once, ever!


#But, every time you reopen R, you will need to reload the package.
#You can do that with
library(StereoMorph)

#Next, we want need to open the digitizing application. Simply run this line of code
digitizeImages(image.file='my_images', shapes.file='my_data',
               landmarks.ref='105_coral_reef_fishes_lms_ids.txt', landmark.radius = 10, control.point.radius = 10)