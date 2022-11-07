save.image(file = "C:/Users/tonyb/OneDrive - Queen's University Belfast/3. PhD Year Three/Chapter 4 General ABM and Fragmentation Survey/Analysis/General ABM Analysis/general_abm_workspace.RData")
options(digits = 2)  

# installing Packages
install.packages ("Rtools")
install.packages ("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggpubr")
install.packages("viridis")
install.packages("patchwork")
install.packages("plyr")

# loading Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(viridis)
library(patchwork)
library(plyr)
library(abind)

{
# set working directory
setwd("C:/Users/tonyb/OneDrive - Queen's University Belfast/3. PhD Year Three/Chapter 4 General ABM and Fragmentation Survey/model_and_outputs")

# functions; continuous landscape model ####
   
field_graph1 <- function(meansdf, km, nematodes_flock){ggplot({{meansdf}}, aes({{km}},{{nematodes_flock}})) +
  geom_area(fill = "blue", alpha=0.2, color = "black", lwd = 0.5, linetype = 1) +
    labs(x = "", y ="") +
    geom_vline(aes(xintercept=0.19),linetype="dashed",colour="black",size=0.05)+
    geom_vline(aes(xintercept=0.37),linetype="dashed",colour="black",size=0.05)+
    geom_vline(aes(xintercept=0.55),linetype="dashed",colour="black",size=0.05)+
    geom_vline(aes(xintercept=0.73),linetype="dashed",colour="black",size=0.05)+
    theme(panel.background = element_rect(fill = "gray94"), #creates background colour
          panel.grid.major = element_blank(), #removes white lines crossing element rect
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 11, face = "plain"),
          axis.text.y = element_text(size= 10, color="grey3"), #size of axis text
          axis.title.y = element_text(size= 13, face="plain"), #size of axis title
          axis.line.y = element_line(size = 0.5, colour = "black", linetype=1),
          axis.text=element_text(size= 10, color="grey3"), #size of axis text
          axis.title.x= element_text(size= 13, face="plain"), #size of axis title #size of axis title
          axis.line.x = element_blank()) + #creates axis lines
    scale_y_continuous(expand=c(0, 0)) +
    scale_x_continuous(expand=c(0, 0)) +
    expand_limits(y = c(0, 70))} 
    

# data organisation for continuous model runs 
organise_data <- function(data){sort1 <- for_graphing[for_graphing$pxcor >= 11,] #use data only after xpixel
sort2 <- sort1[sort1$pxcor <= 101, ] #use data only before xpixel
sort3 <- sort2[sort2$pycor >= 44, ]#use data only after ypixel
sort4 <- sort3[sort3$pycor <= 68, ] #use data only before ypixe
sort4$pxcor <- sort4$pxcor/100 #divide x coordinates by 100 to get km values
sort4$pycor <- sort4$pycor/100 #divide y coordinates by 100 to get km values
sort4$pycor <- sort4$pycor - 0.05
sort4$pxcor <- sort4$pxcor - 0.05
colnames(sort4) <- c("x", "y","egg")
sort5 <- spread(sort4, x, egg) #make data wide format
sort5 <- sort5[-c(1)] #remove first column which is not egg data
colnames(sort5) <- c(1:91) #change column names to these numbers
rownames(sort5) <- c(1:25) #change row names to these numbers
means <- colMeans(sort5) #calculate mean values of every column
means # display mean values
meansdf <- as.data.frame(means)
meansdf <- cbind(meansdf,c(1:91 / 100))
colnames(meansdf) <- c("means", "km")
meansdf} 


#organise roe deer files ####

#list files
c_roe_90 <- list.files(pattern="croe[1:2:3:4][0-9].csv") # list file names in document folder
c_roe_50 <- list.files(pattern="croe[5:6:7:8][0-9].csv") 
c_roe_wfs1 <- list.files(pattern="croe[9][0-9].csv")
c_roe_wfs2 <- list.files (pattern="croe[1][0:1:2][0-9].csv")
c_roe_wfs <- c(c_roe_wfs1, c_roe_wfs2)
c_roe_mds <- list.files(pattern="croe[1][3:4:5:6][0-9].csv")  

#open data frames as lists by range for roe deer
c_roe_90 <- lapply(c_roe_90, read.csv, header=FALSE)
c_roe_50 <- lapply(c_roe_50, read.csv, header=FALSE)
c_roe_wfs <- lapply(c_roe_wfs, read.csv, header=FALSE)
c_roe_mds <- lapply(c_roe_mds, read.csv, header=FALSE)

# rename rows 
new_col_name <- c("pxcor", "pycor", "egg-number")

# add new names
c_roe_90 <- lapply(c_roe_90, setNames, nm = new_col_name)
c_roe_50 <- lapply(c_roe_50, setNames, nm = new_col_name)
c_roe_wfs <- lapply(c_roe_wfs, setNames, nm = new_col_name)
c_roe_mds <- lapply(c_roe_mds, setNames, nm = new_col_name)

# reorder data by pxcor and pycor 
c_roe_90 <- lapply(c_roe_90, function(df){
  df[order(df$pxcor,df$pycor),]})

c_roe_50 <- lapply(c_roe_50, function(df){
  df[order(df$pxcor,df$pycor),]})

c_roe_wfs <- lapply(c_roe_wfs, function(df){
  df[order(df$pxcor,df$pycor),]})

c_roe_mds <- lapply(c_roe_mds, function(df){
  df[order(df$pxcor,df$pycor),]})

# roe; 90% fences ####

coordinates <- lapply(c_roe_90, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
croe90_eggs_only <- lapply(c_roe_90, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
croe90_mean_eggs <- abind(croe90_eggs_only, along=3) #bind data for egg numbers
croe90_mean_eggs <- data.frame(apply(croe90_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, croe90_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
cont_roe_90 <- field_graph1(meansdf,km, eggs_deer)
sheep_roe_90 <- field_graph1(meansdf, km, nematodes_flock)
labels <- "all fields with strong perimeter"
cont_roe_90 <- cont_roe_90 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels) 
sheep_roe_90 <- sheep_roe_90 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)

sheep_roe_90


# roe; 50% fences ####

coordinates <- lapply(c_roe_50, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
croe50_eggs_only <- lapply(c_roe_50, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
croe50_mean_eggs <- abind(croe50_eggs_only, along=3) #bind data for egg numbers
croe50_mean_eggs <- data.frame(apply(croe50_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, croe50_mean_eggs) # join coordinates of final data frame and mean egg numbers


meansdf <- organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
cont_roe_50 <- field_graph1(meansdf,km, eggs_deer)
sheep_roe_50 <- field_graph1(meansdf, km, nematodes_flock)
labels <- "all fields with weak perimeter"
cont_roe_50 <- cont_roe_50 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels = labels)
sheep_roe_50 <- sheep_roe_50 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels = labels)

sheep_roe_50


# roe; two weak fields ####

coordinates <- lapply(c_roe_wfs, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
croe2wfs_eggs_only <- lapply(c_roe_wfs, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
croe2wfs_mean_eggs <- abind(croe2wfs_eggs_only, along=3) #bind data for egg numbers
croe2wfs_mean_eggs <- data.frame(apply(croe2wfs_mean_eggs,c(1,2),mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, croe2wfs_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
cont_roe_2wfs <- field_graph1(meansdf,km, eggs_deer)
sheep_roe_2wfs <- field_graph1(meansdf, km, nematodes_flock)
labels <- c("strong", "weak", "strong", "weak","strong")
cont_roe_2wfs <- cont_roe_2wfs + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)
sheep_roe_2wfs <- sheep_roe_2wfs + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)

sheep_roe_2wfs


# roe; two meadows ####

coordinates <- lapply(c_roe_mds, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
croe2mds_eggs_only <- lapply(c_roe_mds, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
croe2mds_mean_eggs <- abind(croe2mds_eggs_only, along=3) #bind data for egg numbers
croe2mds_mean_eggs <- data.frame(apply(croe2mds_mean_eggs,c(1,2),mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, croe2mds_mean_eggs) # join coordinates of final data frame and mean egg numbers

meansdf <- organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
cont_roe_2mds <- field_graph1(meansdf,km, eggs_deer)
sheep_roe_2mds <- field_graph1(meansdf, km, nematodes_flock)
labels <- c("strong", "meadow", "strong", "meadow","strong")
cont_roe_2mds <- cont_roe_2mds + scale_x_continuous(expand=c(0, 0), breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)
sheep_roe_2mds <- sheep_roe_2mds + scale_x_continuous(expand=c(0, 0), breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)

sheep_roe_2mds

#organise fallow deer files ####

#list files by field type for fallow deer
c_fallow_90 <- list.files(pattern="cfallow[1:2:3:4][0-9].csv") # find files 50 to 89
c_fallow_50 <- list.files(pattern="cfallow[5:6:7:8][0-9].csv") # find files 10 to 49 
c_fallow_wfs1 <- list.files(pattern="cfallow[9][0-9].csv")
c_fallow_wfs2 <- list.files (pattern="cfallow[1][0:1:2][0-9].csv")
c_fallow_wfs <- c(c_fallow_wfs1,c_fallow_wfs2) 
c_fallow_mds <- list.files(pattern="cfallow[1][3:4:5:6][0-9].csv") # find files 10 to 49 

#open data frames as lists by range for fallow deer
c_fallow_90 <- lapply(c_fallow_90, read.csv, header=FALSE)
c_fallow_50 <- lapply(c_fallow_50, read.csv, header=FALSE)
c_fallow_wfs <- lapply(c_fallow_wfs, read.csv, header=FALSE)
c_fallow_mds <- lapply(c_fallow_mds, read.csv, header=FALSE)

# rename rows 
new_col_name <- c("pxcor", "pycor", "egg-number")

# add new names
c_fallow_90 <- lapply(c_fallow_90, setNames, nm = new_col_name)
c_fallow_50 <- lapply(c_fallow_50, setNames, nm = new_col_name)
c_fallow_wfs <- lapply(c_fallow_wfs, setNames, nm = new_col_name)
c_fallow_mds <- lapply(c_fallow_mds, setNames, nm = new_col_name)

# reorder data by pxcor and pycor 
c_fallow_90 <- lapply(c_fallow_90, function(df){
  df[order(df$pxcor,df$pycor),]})

c_fallow_50 <- lapply(c_fallow_50, function(df){
  df[order(df$pxcor,df$pycor),]})

c_fallow_wfs <- lapply(c_fallow_wfs, function(df){
  df[order(df$pxcor,df$pycor),]})

c_fallow_mds <- lapply(c_fallow_mds, function(df){
  df[order(df$pxcor,df$pycor),]})

# fallow; 90% fences ####

coordinates <- lapply(c_fallow_90, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
cfallow90_eggs_only <- lapply(c_fallow_90, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
cfallow90_mean_eggs <- abind(cfallow90_eggs_only, along=3) #bind data for egg numbers
cfallow90_mean_eggs <- data.frame(apply(cfallow90_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, cfallow90_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
cont_fallow_90 <- field_graph1(meansdf,km, eggs_deer)
sheep_fallow_90 <- field_graph1(meansdf, km, nematodes_flock)
labels <- "all fields with strong perimeter"
cont_fallow_90 <- cont_fallow_90 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)
sheep_fallow_90 <- sheep_fallow_90 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)

sheep_fallow_90

# fallow; 50% fences ####

coordinates <- lapply(c_fallow_50, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
cfallow50_eggs_only <- lapply(c_fallow_50, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
cfallow50_mean_eggs <- abind(cfallow50_eggs_only, along=3) #bind data for egg numbers
cfallow50_mean_eggs <- data.frame(apply(cfallow50_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, cfallow50_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
cont_fallow_50 <- field_graph1(meansdf,km, eggs_deer)
sheep_fallow_50 <- field_graph1(meansdf, km, nematodes_flock)
labels <- "all fields with weak perimeter"
cont_fallow_50 <- cont_fallow_50 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)
sheep_fallow_50 <- sheep_fallow_50 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)

sheep_fallow_50


# fallow; two weak fields ####
coordinates <- lapply(c_fallow_wfs, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
cfallow2wfs_eggs_only <- lapply(c_fallow_wfs, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
cfallow2wfs_mean_eggs <- abind(cfallow2wfs_eggs_only, along=3) #bind data for egg numbers
cfallow2wfs_mean_eggs <- data.frame(apply(cfallow2wfs_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, cfallow2wfs_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
cont_fallow_2wfs <- field_graph1(meansdf,km, eggs_deer)
sheep_fallow_2wfs <- field_graph1(meansdf, km, nematodes_flock)
labels <- c("strong", "weak", "strong", "weak","strong")
cont_fallow_2wfs <- cont_fallow_2wfs + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)
sheep_fallow_2wfs <- sheep_fallow_2wfs + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)

sheep_fallow_2wfs



# fallow; two meadows ####

coordinates <- lapply(c_fallow_mds, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
cfallow2mds_eggs_only <- lapply(c_fallow_mds, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
cfallow2mds_mean_eggs <- abind(cfallow2mds_eggs_only, along=3) #bind data for egg numbers
cfallow2mds_mean_eggs <- data.frame(apply(cfallow2mds_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, cfallow2mds_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
cont_fallow_2mds <- field_graph1(meansdf,km, eggs_deer)
sheep_fallow_2mds <- field_graph1(meansdf, km, nematodes_flock)
labels <- c("strong", "meadow", "strong", "meadow","strong")
cont_fallow_2mds <- cont_fallow_2mds + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)
sheep_fallow_2mds <- sheep_fallow_2mds + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)

sheep_fallow_2mds
}




ls()
rm(list = ls()) #environment
cat("\014")
dev.off()




