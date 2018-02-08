# Calculate the distance between two places based on their zipcodes

# SUPPOSE LONGITUDE AND LATTITUDE FOR EACH ZIP WERE ALREDAY MERGED


# In this example, we calculate the distance between patients home and the facilities they went


install.packages("Imap")
library(Imap)

options(scipen=999) ######### Not use scientific format ######


dir="Z:/Donglei/20151214-Breast Readmission SPARCS Gabe/";   ######## THE FOLDER WHERE THE ZIP FILE WAS STORED #####

zip=read.csv(paste(dir,"twozips.csv",sep=""))   ##### FILE FROM DATA, INCLUDING NEWUPID, ZIP CODE FOR HOME, ZIP CODE FOR FACILITY, LONGITUDE AND LATTITUDE FOR EACH ZIP WERE ALREDAY MERGED IN SAS ######

############## MATCH UP ZIPCODE AND LATITUDE/LONGITUDE ################

hlat=zip$home_lat;		### HOME LATTITUDE ##
hlong=zip$home_lon;		### HOME LONGITUDE ##
flat=zip$facility_lat;		### FACILITY LATTITUDE ##
flong=zip$facility_lon;		### FACILITY LONGITUDE ##

dist=apply(as.matrix(1:dim(zip)[1]),1,function(i){print(i);gdist(hlong[i], hlat[i], flong[i], flat[i], units = "miles")}) ##CALCULATE DISTANCE ACCORDING TO LONGITUDE AND LATTITUDE ##

zip=data.frame(zip,dist);

write.csv(zip,file=paste(directory,"distance.csv",sep=""), na = "",col.names=names(zip), row.names = FALSE )
