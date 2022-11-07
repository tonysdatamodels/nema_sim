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

field_graph2 <- function(meansdf, km, nematodes_flock){ggplot({{meansdf}}, aes({{km}},{{nematodes_flock}})) +
    geom_area(fill = "orange", alpha=0.2, color ="black", lwd = 0.5, linetype = 1) +
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


# data organisation for fragmented model runs 
frag_organise_data <- function(data){f1sort1 <- for_graphing[for_graphing$pxcor >= 11,] #use data only after x pixel
f1sort2 <- f1sort1[f1sort1$pxcor <= 28, ] #use data only before x pixel
f1sort3 <- f1sort2[f1sort2$pycor >= 18, ]#use data only after y pixel 
f1sort4 <- f1sort3[f1sort3$pycor <= 42, ] #use data only before y pixel 
f1sort4$pxcor <- f1sort4$pxcor/100 #divide x coordinates by 100 to get km values
f1sort4$pycor <- f1sort4$pycor/100 #divide y coordinates by 100 to get km values
f1sort4$pycor <- f1sort4$pycor - 0.05
f1sort4$pxcor <- f1sort4$pxcor - 0.05
colnames(f1sort4) <- c("x", "y","egg")
f1sort5 <- spread(f1sort4, x, egg) #make data wide format
f1sort5 <- f1sort5[-c(1)] #remove first column which is not egg data
colnames(f1sort5) <- c(1:18) #change column names to these numbers
rownames(f1sort5) <- c(1:25) #change row names to these numbers
f1means <- colMeans(f1sort5) #calculate mean values of every column
f1means # display mean values
f1meansdf <- as.data.frame(f1means) #put mean value in data frame
f1meansdf <- cbind(f1meansdf,c(1:18 / 100))
colnames(f1meansdf) <- c("means", "km")
f1meansdf

#farm2
f2sort1 <- for_graphing[for_graphing$pxcor >= 11,] #use data only after xpixel 5
f2sort2 <- f2sort1[f2sort1$pxcor <= 28, ] #use data only before xpixel  450
f2sort3 <- f2sort2[f2sort2$pycor >= 70, ]#use data only after ypixel 5
f2sort4 <- f2sort3[f2sort3$pycor <= 94, ] #use data only before ypixel  105
f2sort4$pxcor <- f2sort4$pxcor/100 #divide x coordinates by 100 to get km values
f2sort4$pycor <- f2sort4$pycor/100 #divide y coordinates by 100 to get km values
f2sort4$pycor <- f2sort4$pycor - 0.05
f2sort4$pxcor <- f2sort4$pxcor - 0.05
colnames(f2sort4) <- c("x", "y","egg")
f2sort5 <- spread(f2sort4, x, egg) #make data wide format
f2sort5 <- f2sort5[-c(1)] #remove first column which is not egg data
colnames(f2sort5) <- c(1:18) #change column names to these numbers
rownames(f2sort5) <- c(1:25) #change row names to these numbers
f2means <- colMeans(f2sort5) #calculate mean values of every column
f2means # display mean values
f2meansdf <- as.data.frame(f2means) #put mean value in data frame
f2meansdf <- cbind(f2meansdf,c(19:36 / 100))
colnames(f2meansdf) <- c("means", "km")
f2meansdf

#farm3
f3sort1 <- for_graphing[for_graphing$pxcor >= 46,] 
f3sort2 <- f3sort1[f3sort1$pxcor <= 63, ]
f3sort3 <- f3sort2[f3sort2$pycor >= 44, ]
f3sort4 <- f3sort3[f3sort3$pycor <= 68, ] 
f3sort4$pxcor <- f3sort4$pxcor/100 
f3sort4$pycor <- f3sort4$pycor/100 
f3sort4$pycor <- f3sort4$pycor - 0.05
f3sort4$pxcor <- f3sort4$pxcor - 0.05
colnames(f3sort4) <- c("x", "y","egg")
f3sort5 <- spread(f3sort4, x, egg) #make data wide format
f3sort5 <- f3sort5[-c(1)] #remove first column which is not egg data
colnames(f3sort5) <- c(1:18) #change column names to these numbers
rownames(f3sort5) <- c(1:25) #change row names to these numbers
f3means <- colMeans(f3sort5) #calculate mean values of every column
f3means # display mean values
f3meansdf <- as.data.frame(f3means) #put mean value in data frame
f3meansdf <- cbind(f3meansdf,c(37:54 / 100))
colnames(f3meansdf) <- c("means", "km")
f3meansdf

#farm4
f4sort1 <- for_graphing[for_graphing$pxcor >= 81,] #use data only after xpixel 5
f4sort2 <- f4sort1[f4sort1$pxcor <= 98, ] #use data only before xpixel  450
f4sort3 <- f4sort2[f4sort2$pycor >= 70, ]#use data only after ypixel 5
f4sort4 <- f4sort3[f4sort3$pycor <= 94, ] #use data only before ypixel  105
f4sort4$pxcor <- f4sort4$pxcor/100 #divide x coordinates by 100 to get km values
f4sort4$pycor <- f4sort4$pycor/100 #divide y coordinates by 100 to get km values
f4sort4$pycor <- f4sort4$pycor - 0.05
f4sort4$pxcor <- f4sort4$pxcor - 0.05
colnames(f4sort4) <- c("x", "y","egg")
f4sort5 <- spread(f4sort4, x, egg) #make data wide format
f4sort5 <- f4sort5[-c(1)] #remove first column which is not egg data
colnames(f4sort5) <- c(1:18) #change column names to these numbers
rownames(f4sort5) <- c(1:25) #change row names to these numbers
f4means <- colMeans(f4sort5) #calculate mean values of every column
f4means # display mean values
f4meansdf <- as.data.frame(f4means) #put mean value in data frame
f4meansdf <- cbind(f4meansdf,c(55:72 / 100))
colnames(f4meansdf) <- c("means", "km")
f4meansdf

#farm5
f5sort1 <- for_graphing[for_graphing$pxcor >= 81,] #use data only after xpixel 5
f5sort2 <- f5sort1[f5sort1$pxcor <= 99, ] #use data only before xpixel  450
f5sort3 <- f5sort2[f5sort2$pycor >= 18, ]#use data only after ypixel 5
f5sort4 <- f5sort3[f5sort3$pycor <= 42, ] #use data only before ypixel  105
f5sort4$pxcor <- f5sort4$pxcor/100 #divide x coordinates by 100 to get km values
f5sort4$pycor <- f5sort4$pycor/100 #divide y coordinates by 100 to get km values
f5sort4$pycor <- f5sort4$pycor - 0.05
f5sort4$pxcor <- f5sort4$pxcor - 0.05
colnames(f5sort4) <- c("x", "y","egg")
f5sort5 <- spread(f5sort4, x, egg) #make data wide format
f5sort5 <- f5sort5[-c(1)] #remove first column which is not egg data
colnames(f5sort5) <- c(1:19) #change column names to these numbers
rownames(f5sort5) <- c(1:25) #change row names to these numbers
f5means <- colMeans(f5sort5) #calculate mean values of every column
f5means # display mean values
f5meansdf <- as.data.frame(f5means) #put mean value in data frame
f5meansdf <- cbind(f5meansdf,c(73:91 / 100))
colnames(f5meansdf) <- c("means", "km")
f5meansdf

#make column names in each dataframe the same
new_col_name2 <- c("means", "km")

f1meansdf <- setNames(f1meansdf, new_col_name2)
f2meansdf <- setNames(f2meansdf, new_col_name2)
f3meansdf <- setNames(f3meansdf, new_col_name2)
f4meansdf <- setNames(f4meansdf, new_col_name2)
f5meansdf <- setNames(f5meansdf, new_col_name2)

#join data frames together vertically
meansdf <- bind_rows (f1meansdf, f2meansdf, f3meansdf, f4meansdf, f5meansdf)}


#organise roe deer files ####

#list files
f_roe_90 <- list.files(pattern="froe[1:2:3:4][0-9].csv") # list file names in document folder
f_roe_50 <- list.files(pattern="froe[5:6:7:8][0-9].csv") 
f_roe_wfs1 <- list.files(pattern="froe[9][0-9].csv")
f_roe_wfs2 <- list.files (pattern="froe[1][0:1:2][0-9].csv")
f_roe_wfs <- c(f_roe_wfs1, f_roe_wfs2)
f_roe_mds <- list.files(pattern="froe[1][3:4:5:6][0-9].csv")  

#open data frames as lists by range for roe deer
f_roe_90 <- lapply(f_roe_90, read.csv, header=FALSE)
f_roe_50 <- lapply(f_roe_50, read.csv, header=FALSE)
f_roe_wfs <- lapply(f_roe_wfs, read.csv, header=FALSE)
f_roe_mds <- lapply(f_roe_mds, read.csv, header=FALSE)

# rename rows 
new_col_name <- c("pxcor", "pycor", "egg-number")

# add new names
f_roe_90 <- lapply(f_roe_90, setNames, nm = new_col_name)
f_roe_50 <- lapply(f_roe_50, setNames, nm = new_col_name)
f_roe_wfs <- lapply(f_roe_wfs, setNames, nm = new_col_name)
f_roe_mds <- lapply(f_roe_mds, setNames, nm = new_col_name)

# reorder data by pxcor and pycor 
f_roe_90 <- lapply(f_roe_90, function(df){
  df[order(df$pxcor,df$pycor),]})

f_roe_50 <- lapply(f_roe_50, function(df){
  df[order(df$pxcor,df$pycor),]})

f_roe_wfs <- lapply(f_roe_wfs, function(df){
  df[order(df$pxcor,df$pycor),]})

f_roe_mds <- lapply(f_roe_mds, function(df){
  df[order(df$pxcor,df$pycor),]})

# roe; 90% fences ####

coordinates <- lapply(f_roe_90, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
froe90_eggs_only <- lapply(f_roe_90, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
froe90_mean_eggs <- abind(froe90_eggs_only, along=3) #bind data for egg numbers
froe90_mean_eggs <- data.frame(apply(froe90_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, froe90_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- frag_organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
frag_roe_90 <- field_graph2(meansdf,km, eggs_deer)
sheep_frag_roe_90 <- field_graph2(meansdf, km, nematodes_flock)
labels <- "all fields with strong perimeter"
frag_roe_90 <- frag_roe_90 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)
sheep_frag_roe_90 <- sheep_frag_roe_90 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)

sheep_frag_roe_90


# roe; 50% fences ####

coordinates <- lapply(f_roe_50, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
froe50_eggs_only <- lapply(f_roe_50, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
froe50_mean_eggs <- abind(froe50_eggs_only, along=3) #bind data for egg numbers
froe50_mean_eggs <- data.frame(apply(froe50_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, froe50_mean_eggs) # join coordinates of final data frame and mean egg numbers

meansdf <- frag_organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
frag_roe_50 <- field_graph2(meansdf,km, eggs_deer)
sheep_frag_roe_50 <- field_graph2(meansdf, km, nematodes_flock)
labels <- "all fields with weak perimeter"
frag_roe_50 <- frag_roe_50 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels = labels)
sheep_frag_roe_50 <- sheep_frag_roe_50 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)

sheep_frag_roe_50

# roe; two weak fields ####

coordinates <- lapply(f_roe_wfs, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
froe2wfs_eggs_only <- lapply(f_roe_wfs, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
froe2wfs_mean_eggs <- abind(froe2wfs_eggs_only, along=3) #bind data for egg numbers
froe2wfs_mean_eggs <- data.frame(apply(froe2wfs_mean_eggs,c(1,2),mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, froe2wfs_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- frag_organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
frag_roe_2wfs <- field_graph2(meansdf,km, eggs_deer)
sheep_frag_roe_2wfs <- field_graph2(meansdf, km, nematodes_flock)
labels <- c("strong", "weak", "strong", "weak","strong")
frag_roe_2wfs <- frag_roe_2wfs + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)
sheep_frag_roe_2wfs <- sheep_frag_roe_2wfs + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)

sheep_frag_roe_2wfs


# roe; two meadows ####

coordinates <- lapply(f_roe_mds, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
froe2mds_eggs_only <- lapply(f_roe_mds, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
froe2mds_mean_eggs <- abind(froe2mds_eggs_only, along=3) #bind data for egg numbers
froe2mds_mean_eggs <- data.frame(apply(froe2mds_mean_eggs,c(1,2),mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, froe2mds_mean_eggs) # join coordinates of final data frame and mean egg numbers

meansdf <- frag_organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
frag_roe_2mds <- field_graph2(meansdf,km, eggs_deer)
sheep_frag_roe_2mds <- field_graph2(meansdf, km, nematodes_flock)
labels <- c("strong", "meadow", "strong", "meadow","strong")
frag_roe_2mds <- frag_roe_2mds + scale_x_continuous(expand=c(0, 0), breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)
sheep_frag_roe_2mds <- sheep_frag_roe_2mds + scale_x_continuous(expand=c(0, 0), breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)

sheep_frag_roe_2mds

#organise fallow deer files ####

#list files by field type for fallow deer
f_fallow_90 <- list.files(pattern="ffallow[1:2:3:4][0-9].csv") # find files 50 to 89
f_fallow_50 <- list.files(pattern="ffallow[5:6:7:8][0-9].csv") # find files 10 to 49 
f_fallow_wfs1 <- list.files(pattern="ffallow[9][0-9].csv")
f_fallow_wfs2 <- list.files (pattern="ffallow[1][0:1:2][0-9].csv")
f_fallow_wfs <- c(f_fallow_wfs1,f_fallow_wfs2) 
f_fallow_mds <- list.files(pattern="ffallow[1][3:4:5:6][0-9].csv") # find files 10 to 49 

#open data frames as lists by range for fallow deer
f_fallow_90 <- lapply(f_fallow_90, read.csv, header=FALSE)
f_fallow_50 <- lapply(f_fallow_50, read.csv, header=FALSE)
f_fallow_wfs <- lapply(f_fallow_wfs, read.csv, header=FALSE)
f_fallow_mds <- lapply(f_fallow_mds, read.csv, header=FALSE)

# rename rows 
new_col_name <- c("pxcor", "pycor", "egg-number")

# add new names
f_fallow_90 <- lapply(f_fallow_90, setNames, nm = new_col_name)
f_fallow_50 <- lapply(f_fallow_50, setNames, nm = new_col_name)
f_fallow_wfs <- lapply(f_fallow_wfs, setNames, nm = new_col_name)
f_fallow_mds <- lapply(f_fallow_mds, setNames, nm = new_col_name)

# reorder data by pxcor and pycor 
f_fallow_90 <- lapply(f_fallow_90, function(df){
  df[order(df$pxcor,df$pycor),]})

f_fallow_50 <- lapply(f_fallow_50, function(df){
  df[order(df$pxcor,df$pycor),]})

f_fallow_wfs <- lapply(f_fallow_wfs, function(df){
  df[order(df$pxcor,df$pycor),]})

f_fallow_mds <- lapply(f_fallow_mds, function(df){
  df[order(df$pxcor,df$pycor),]})

# fallow; 90% fences ####

coordinates <- lapply(f_fallow_90, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
ffallow90_eggs_only <- lapply(f_fallow_90, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
ffallow90_mean_eggs <- abind(ffallow90_eggs_only, along=3) #bind data for egg numbers
ffallow90_mean_eggs <- data.frame(apply(ffallow90_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, ffallow90_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- frag_organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
frag_fallow_90 <- field_graph2(meansdf,km, eggs_deer)
sheep_frag_fallow_90 <- field_graph2(meansdf, km, nematodes_flock)
labels <- "all fields with strong perimeter"
frag_fallow_90 <- frag_fallow_90 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)
sheep_frag_fallow_90 <- sheep_frag_fallow_90 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)

sheep_frag_fallow_90

# fallow; 50% fences ####

coordinates <- lapply(f_fallow_50, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
ffallow50_eggs_only <- lapply(f_fallow_50, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
ffallow50_mean_eggs <- abind(ffallow50_eggs_only, along=3) #bind data for egg numbers
ffallow50_mean_eggs <- data.frame(apply(ffallow50_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, ffallow50_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- frag_organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
frag_fallow_50 <- field_graph2(meansdf,km, eggs_deer)
sheep_frag_fallow_50 <- field_graph2(meansdf, km, nematodes_flock)
labels <- "all fields with weak perimeter"
frag_fallow_50 <- frag_fallow_50 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)
sheep_frag_fallow_50 <- sheep_frag_fallow_50 + scale_x_continuous(expand=c(0, 0), breaks = 0.455, labels= labels)

sheep_frag_fallow_50

# fallow; two weak fields ####
coordinates <- lapply(f_fallow_wfs, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
ffallow2wfs_eggs_only <- lapply(f_fallow_wfs, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
ffallow2wfs_mean_eggs <- abind(ffallow2wfs_eggs_only, along=3) #bind data for egg numbers
ffallow2wfs_mean_eggs <- data.frame(apply(ffallow2wfs_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, ffallow2wfs_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- frag_organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
frag_fallow_2wfs <- field_graph2(meansdf,km, eggs_deer)
sheep_frag_fallow_2wfs <- field_graph2(meansdf, km, nematodes_flock)
labels <- c("strong", "weak", "strong", "weak","strong")
frag_fallow_2wfs <- frag_fallow_2wfs + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)
sheep_frag_fallow_2wfs <- sheep_frag_fallow_2wfs + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)

sheep_frag_fallow_2wfs

# fallow; two meadows ####

coordinates <- lapply(f_fallow_mds, function(x) x%>% select(pxcor, pycor)) # create separate list of data frames with coordinates
coordinates <- as.data.frame(coordinates[[40]]) #extract the latest coordinate data frame (frame 19, the final one) as row numbers line up with egg data for future merge.
ffallow2mds_eggs_only <- lapply(f_fallow_mds, function(x) x%>% select("egg-number")) # gather egg  number from all data frames in list
ffallow2mds_mean_eggs <- abind(ffallow2mds_eggs_only, along=3) #bind data for egg numbers
ffallow2mds_mean_eggs <- data.frame(apply(ffallow2mds_mean_eggs,c(1,2), mean)) #find mean of all egg numbers from each model run
for_graphing <- cbind(coordinates, ffallow2mds_mean_eggs) # join coordinates of final data frame and mean egg numbers

# organise data for graphing
meansdf <- frag_organise_data (for_graphing)
colnames(meansdf) <- c("eggs_deer", "km")
larvae_herbage <- meansdf$eggs_deer * 0.2 #chance that larvae are on herbage
rate_injestion_larvae <- 1.4/(2000*2) # daily rate of injestion of L3 on pasture 1.4 - daily herbage intake per host, 2000 - standing biomass kg per hectare, 5 is hectares per field
amount_larvae_injested <- larvae_herbage * (rate_injestion_larvae * 61) # 61 days 
nematode_per_sheep <- amount_larvae_injested * (0.4)
nematode_per_flock <- nematode_per_sheep * 20
meansdf <- cbind (meansdf,nematode_per_flock)
colnames(meansdf) <- c("eggs_deer", "km","nematodes_flock")
frag_fallow_2mds <- field_graph2(meansdf,km, eggs_deer)
sheep_frag_fallow_2mds <- field_graph2(meansdf, km, nematodes_flock)
labels <- c("strong", "meadow", "strong", "meadow","strong")
frag_fallow_2mds <- frag_fallow_2mds + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)
sheep_frag_fallow_2mds <- sheep_frag_fallow_2mds + scale_x_continuous(expand=c(0, 0),breaks = c(0.1, 0.28, 0.46, 0.64, 0.82), labels= labels)

sheep_frag_fallow_2mds
}





ls() #list
rm(list = ls()) #environment
cat("\014") # console
dev.off() # plots
