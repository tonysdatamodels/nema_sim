options(scipen=999)
options(digits = 2) 

install.packages("gdata")

library(gdata)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(viridis)
library(patchwork)
library(plyr)
library(abind)

{
# remove all files which are not needed
rm(list = ls()[!ls() %in% c("sheep_roe_90", "sheep_frag_roe_90",
                            "sheep_roe_50", "sheep_frag_roe_50",
                             "sheep_roe_2wfs","sheep_frag_roe_2wfs",
                            "sheep_roe_2mds", "sheep_frag_roe_2mds",
                            "sheep_fallow_90","sheep_frag_fallow_90",
                            "sheep_fallow_50", "sheep_frag_fallow_50",
                            "sheep_fallow_2wfs","sheep_frag_fallow_2wfs",
                            "sheep_fallow_2mds", "sheep_frag_fallow_2mds")])


# graph 1 roe non fragmented ####
  
graph3_1 <- sheep_roe_90 / sheep_roe_50 / sheep_roe_2wfs / sheep_roe_2mds
graph3_1

graph3_1 [[1]] <- graph3_1[[1]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  ggtitle("roe deer continuous landscape") +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) +
  expand_limits(y = c(0, 70))

graph3_1 [[2]] <- graph3_1[[2]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph3_1 [[3]] <- graph3_1[[3]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph3_1 [[4]] <- graph3_1[[4]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph3_1


# graph 2 roe fragmented ####

sheep_frag_roe_50
graph3_2 <- sheep_frag_roe_90 / sheep_frag_roe_50 / sheep_frag_roe_2wfs /sheep_frag_roe_2mds
graph3_2

graph3_2 [[1]]<- graph3_2[[1]] + theme(axis.title.y = element_blank(),
                                       axis.title.x=element_blank(),
                                       axis.ticks.x = element_blank(),
                                       panel.background = element_rect(colour = "black")) +
  ggtitle("roe deer fragmented landscape") +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph3_2 [[2]] <- graph3_2[[2]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) +
  expand_limits(y = c(0, 70))

graph3_2 [[3]] <- graph3_2[[3]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph3_2 [[4]] <- graph3_2[[4]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) +
  expand_limits(y = c(0, 70))

graph3_2

# graph 3 fallow non fragmented ####
graph4_1 <- sheep_fallow_90 / sheep_fallow_50 / sheep_fallow_2wfs / sheep_fallow_2mds
graph4_1

graph4_1 [[1]] <- graph4_1[[1]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  ggtitle("fallow deer continuous landscape") +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) +
  expand_limits(y = c(0, 70))

graph4_1 [[2]] <- graph4_1[[2]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph4_1 [[3]] <- graph4_1[[3]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph4_1 [[4]] <- graph4_1[[4]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph4_1

# graph 4 fallow fragmented ####
graph4_2 <- sheep_frag_fallow_90 / sheep_frag_fallow_50 / sheep_frag_fallow_2wfs /sheep_frag_fallow_2mds
graph4_2

graph4_2 [[1]]<- graph4_2[[1]] + theme(axis.title.y = element_blank(),
                                       axis.title.x=element_blank(),
                                       axis.ticks.x = element_blank(),
                                       panel.background = element_rect(colour = "black")) +
  ggtitle("fallow deer fragmented landscape") +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph4_2 [[2]] <- graph4_2[[2]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph4_2 [[3]] <- graph4_2[[3]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph4_2 [[4]] <- graph4_2[[4]] + theme(axis.title.y = element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.ticks.x = element_blank(),
                                        panel.background = element_rect(colour = "black")) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 60, 20)) + 
  expand_limits(y = c(0, 70))

graph4_2


# final figures ####
figure1 <- graph3_1 | graph4_1 
figure1

figure2 <- graph3_2 | graph4_2 
figure2

}

figure1
figure2
#for pdf output 8 by 6 inch
#or 8.5 by 12
#14 by 12 portrait pdf
#950 1300

# make dataframes for analysis ####
{
data_sheep_roe_90 <- as.data.frame(sheep_roe_90[[1]])
data_sheep_frag_roe_90 <- as.data.frame(sheep_frag_roe_90[[1]])
data_sheep_roe_50 <- as.data.frame(sheep_roe_50[[1]])
data_sheep_frag_roe_50 <- as.data.frame(sheep_frag_roe_50[[1]])
data_sheep_roe_2wfs <- as.data.frame(sheep_roe_2wfs[[1]])
data_sheep_frag_roe_2wfs <- as.data.frame(sheep_frag_roe_2wfs[[1]])
data_sheep_roe_2mds <- as.data.frame(sheep_roe_2mds[[1]])
data_sheep_frag_roe_2mds <- as.data.frame(sheep_frag_roe_2mds[[1]])

data_sheep_fallow_90 <- as.data.frame(sheep_fallow_90[[1]])
data_sheep_frag_fallow_90 <- as.data.frame(sheep_frag_fallow_90[[1]])
data_sheep_fallow_50 <- as.data.frame(sheep_fallow_50 [[1]])
data_sheep_frag_fallow_50 <- as.data.frame(sheep_frag_fallow_50[[1]])
data_sheep_fallow_2wfs <- as.data.frame(sheep_fallow_2wfs[[1]])
data_sheep_frag_fallow_2wfs <- as.data.frame(sheep_frag_fallow_2wfs[[1]])
data_sheep_fallow_2mds <- as.data.frame(sheep_fallow_2mds[[1]])
data_sheep_frag_fallow_2mds <- as.data.frame(sheep_frag_fallow_2mds[[1]])
}

# data analysis; strong fences landscapes ####

cr90 <- data_sheep_roe_90[-c(1, 10, 19, 28, 37, 46, 55, 64, 73, 82, 9),] 
fr90 <- data_sheep_frag_roe_90[-c(1, 10, 19, 28, 37, 46, 55, 64, 73, 82, 9),]
cf90 <- data_sheep_fallow_90[-c(1, 10, 19, 28, 37, 46, 55, 64, 73, 82, 9),]
ff90 <- data_sheep_frag_fallow_90[-c(1, 10, 19, 28, 37, 46, 55, 64, 73, 82, 9),]

all_90 <- data.frame(cbind(cr90$nematodes_flock,
                           fr90$nematodes_flock,
                           cf90$nematodes_flock,
                           ff90$nematodes_flock))

colnames(all_90) <- c("croe90", "froe90", "cfallow90","ffallow90")
all_90_means <- colMeans(all_90)
all_90_sd <- apply(all_90,2,sd) # standard deviation

hist(all_90$croe90)
hist(all_90$froe90)
hist(all_90$cfallow90)
hist(all_90$ffallow90)

shapiro.test(all_90$croe90)
shapiro.test(all_90$froe90)
shapiro.test(all_90$cfallow90)
shapiro.test(all_90$ffallow90)

all_90 <- as.data.frame(gather(all_90))

attach(all_90)
pairwise.wilcox.test(value, key, p.adjust.method = "bonf",exact=FALSE)
detach(all_90)

# data analysis; weak fields landscapes ####

cr50 <- data_sheep_roe_50[-c(1, 10, 19, 28, 37, 46, 55, 64, 73, 82, 91),] 
fr50 <- data_sheep_frag_roe_50[-c(1, 10, 19, 28, 37, 46, 55, 64, 73, 82, 91),] 
cf50 <- data_sheep_fallow_50[-c(1, 10, 19, 28, 37, 46, 55, 64, 73, 82, 91),] 
ff50 <- data_sheep_frag_fallow_50[-c(1, 10, 19, 28, 37, 46, 55, 64, 73, 82, 91),] 

all_50 <- data.frame(cbind(cr50$nematodes_flock,
                           fr50$nematodes_flock,
                           cf50$nematodes_flock,
                           ff50$nematodes_flock))

colnames(all_50) <- c("croe50", "froe50", "cfallow50","ffallow50")
all_50_means <- colMeans(all_50)
all_50_sd <- apply(all_50,2,sd) # standard deviation

hist(all_50$croe50)
hist(all_50$froe50)
hist(all_50$cfallow50)
hist(all_50$ffallow50)

shapiro.test(all_50$croe50)
shapiro.test(all_50$froe50)
shapiro.test(all_50$cfallow50)
shapiro.test(all_50$ffallow50)

all_50 <- as.data.frame(gather(all_50))

attach(all_50)
pairwise.wilcox.test(value, key, p.adjust.method = "bonf",exact=FALSE)
detach(all_50)


# data analysis; (two weak fields) landscapes ####

#strong####
cr_strong_fields <- data_sheep_roe_2wfs[-c(1,10,19:37,46, 55:73, 82, 91),] 
fr_strong_fields <- data_sheep_frag_roe_2wfs[-c(1,10,19:37,46, 55:73, 82, 91),]
cf_strong_fields<- data_sheep_fallow_2wfs[-c(1,10,19:37,46, 55:73, 82, 91),]
ff_strong_fields <- data_sheep_frag_fallow_2wfs[-c(1,10,19:37,46, 55:73, 82, 91),]


strong_2wfs <- data.frame(cbind(cr_strong_fields$nematodes_flock,
                           fr_strong_fields$nematodes_flock,
                           cf_strong_fields$nematodes_flock,
                           ff_strong_fields$nematodes_flock))

colnames(strong_2wfs) <- c("croe_strong_fields", "froe_strong_fields", "cfallow_strong_fields","ffallow_strong_fields")
all_strong_fields_means <- colMeans(strong_2wfs)
all_strong_fields_sd <- apply(strong_2wfs,2,sd) # standard deviation

hist(strong_2wfs$croe_strong_fields)
hist(strong_2wfs$froe_strong_fields)
hist(strong_2wfs$cfallow_strong_fields)
hist(strong_2wfs$ffallow_strong_fields)

shapiro.test(strong_2wfs$croe_strong_fields)
shapiro.test(strong_2wfs$froe_strong_fields)
shapiro.test(strong_2wfs$cfallow_strong_fields)
shapiro.test(strong_2wfs$ffallow_strong_fields)

strong_2wfs <- as.data.frame(gather(strong_2wfs))

attach(strong_2wfs)
pairwise.wilcox.test(value, key, p.adjust.method = "bonf",exact=FALSE)
detach(strong_2wfs)

#weak fields####
cr_weak_fields <- data_sheep_roe_2wfs[-c(1:19,28,37:55,64,73:91),] 
fr_weak_fields <- data_sheep_frag_roe_2wfs[-c(1:19,28,37:55,64,73:91),]
cf_weak_fields<- data_sheep_fallow_2wfs[-c(1:19,28,37:55,64,73:91),]
ff_weak_fields <- data_sheep_frag_fallow_2wfs[-c(1:19,28,37:55,64,73:91),]

weak_2wfs <- data.frame(cbind(cr_weak_fields$nematodes_flock,
                              fr_weak_fields$nematodes_flock,
                              cf_weak_fields$nematodes_flock,
                              ff_weak_fields$nematodes_flock))

colnames(weak_2wfs) <- c("croe_weak_fields", "froe_weak_fields", "cfallow_weak_fields","ffallow_weak_fields")
all_weak_fields_means <- colMeans(weak_2wfs)
colMeans(weak_2wfs)
all_weak_fields_sd <- apply(weak_2wfs,2,sd) # standard deviation

hist(weak_2wfs$croe_weak_fields)
hist(weak_2wfs$froe_weak_fields)
hist(weak_2wfs$cfallow_weak_fields)
hist(weak_2wfs$ffallow_weak_fields)

shapiro.test(weak_2wfs$croe_weak_fields)
shapiro.test(weak_2wfs$froe_weak_fields)
shapiro.test(weak_2wfs$cfallow_weak_fields)
shapiro.test(weak_2wfs$ffallow_weak_fields)

weak_2wfs <- as.data.frame(gather(weak_2wfs))

attach(weak_2wfs)
pairwise.wilcox.test(value, key, p.adjust.method = "bonf",exact=FALSE)
detach(weak_2wfs)

# data analysis; (two meadows) landscapes ####

#fields####
cr_strong_fields_meadow <- data_sheep_roe_2mds[-c(1,10,19:37,46, 55:73, 82, 91),] 
fr_strong_fields_meadow <- data_sheep_frag_roe_2mds[-c(1,10,19:37,46, 55:73, 82, 91),]
cf_strong_fields_meadow<- data_sheep_fallow_2mds[-c(1,10,19:37,46, 55:73, 82, 91),]
ff_strong_fields_meadow <- data_sheep_frag_fallow_2mds[-c(1,10,19:37,46, 55:73, 82, 91),]

strong_2mds <- data.frame(cbind(cr_strong_fields_meadow$nematodes_flock,
                                fr_strong_fields_meadow$nematodes_flock,
                                cf_strong_fields_meadow$nematodes_flock,
                                ff_strong_fields_meadow$nematodes_flock))

colnames(strong_2mds) <- c("croe_strong_fields_meadow", "froe_strong_fields_meadow", "cfallow_strong_fields_meadow","ffallow_strong_fields_meadow")
all_strong2md_means <- colMeans(strong_2mds)
all_strong2md_sd <- apply(strong_2mds,2,sd) # standard deviation

hist(strong_2mds$croe_strong_fields_meadow)
hist(strong_2mds$froe_strong_fields_meadow)
hist(strong_2mds$cfallow_strong_fields_meadow)
hist(strong_2mds$ffallow_strong_fields_meadow)

shapiro.test(strong_2mds$croe_strong_fields_meadow)
shapiro.test(strong_2mds$froe_strong_fields_meadow)
shapiro.test(strong_2mds$cfallow_strong_fields_meadow)
shapiro.test(strong_2mds$ffallow_strong_fields_meadow)

strong_2mds <- as.data.frame(gather(strong_2mds))

attach(strong_2mds)
pairwise.wilcox.test(value, key, p.adjust.method = "bonf",exact=FALSE)
detach(strong_2mds)

#meadows####
cr_meadows <- data_sheep_roe_2mds[-c(1:19,28,37:55,64,73:91),] 
fr_meadows <- data_sheep_frag_roe_2mds[-c(1:19,28,37:55,64,73:91),]
cf_meadows<- data_sheep_fallow_2mds[-c(1:19,28,37:55,64,73:91),]
ff_meadows <- data_sheep_frag_fallow_2mds[-c(1:19,28,37:55,64,73:91),]

meadows <- data.frame(cbind(cr_meadows$nematodes_flock,
                            fr_meadows$nematodes_flock,
                            cf_meadows$nematodes_flock,
                            ff_meadows$nematodes_flock))

colnames(meadows) <- c("croe_meadows", "froe_meadows", "cfallow_meadows","ffallow_meadows")
all_meadows_means <- colMeans(meadows)
all_meadows_sd <- apply(meadows,2,sd) # standard deviation

hist(meadows$croe_meadows)
hist(meadows$froe_meadows)
hist(meadows$cfallow_meadows)
hist(meadows$ffallow_meadows)

shapiro.test(meadows$croe_meadows)
shapiro.test(meadows$froe_meadows)
shapiro.test(meadows$cfallow_meadows)
shapiro.test(meadows$ffallow_meadows)

meadows <- as.data.frame(gather(meadows))

attach(meadows)
pairwise.wilcox.test(value, key, p.adjust.method = "bonf",exact=FALSE)
detach(meadows)

# all descriptive statistics ####
all_means <- as.data.frame(
  cbind(all_90_means,all_50_means,all_strong_fields_means,
      all_weak_fields_means,
      all_strong2md_means,
      all_meadows_means))

rownames(all_means) <- c("continuous roe", "fragmented roe", "continuous fallow", "fragmented fallow")

write.csv(all_means,"C:/Users/tonyb/OneDrive - Queen's University Belfast/3. PhD Year Three/Chapter 4 General ABM and Fragmentation Survey/Data/model_output_means.csv", row.names = TRUE)

all_sd <- all_sd <- as.data.frame(
  cbind(all_90_sd,all_50_sd,all_strong_fields_sd,
        all_weak_fields_sd,
        all_strong2md_sd,
        all_meadows_sd))

rownames(all_sd) <- c("continuous roe", "fragmented roe", "continuous fallow", "fragmented fallow")

write.csv(all_sd,"C:/Users/tonyb/OneDrive - Queen's University Belfast/3. PhD Year Three/Chapter 4 General ABM and Fragmentation Survey/Data/model_output_sds.csv", row.names = TRUE)


# all roe continuous ####

all_roe_cont <- cbindX(cr90, cr50, cr_strong_fields, cr_strong_fields_meadow)
all_roe_cont <- all_roe_cont[, -c(1,2,4,5,7,8,10,11)]

colnames(all_roe_cont) <- c("cr90", "cr50", "cr_strong_fields", "cr_strong_fields_meadow")
colMeans(all_roe_cont, na.rm = T)

all_roe_cont <- as.data.frame(gather(all_roe_cont))

attach(all_roe_cont)
pairwise.wilcox.test(value, key, p.adjust.method = "bonf",exact=FALSE)
detach(all_roe_cont)

# all fallow continuous ####

all_fallow_cont <- cbindX(cf90, cf50, cf_strong_fields, cf_strong_fields_meadow)
all_fallow_cont <- all_fallow_cont[, -c(1,2,4,5,7,8,10,11)]

colnames(all_fallow_cont) <- c("cf90", "cf50", "cf_strong_fields", "cf_strong_fields_meadow")
colMeans(all_fallow_cont, na.rm = T)

all_fallow_cont <- as.data.frame(gather(all_fallow_cont))

attach(all_fallow_cont)
pairwise.wilcox.test(value, key, p.adjust.method = "bonf",exact=FALSE)
detach(all_fallow_cont)

#all roe fragmented ####

all_roe_frag <- cbindX(fr90, fr50, fr_strong_fields, fr_strong_fields_meadow)
all_roe_frag <- all_roe_frag[, -c(1,2,4,5,7,8,10,11)]

colnames(all_roe_frag) <- c("fr90", "fr50", "fr_strong_fields", "fr_strong_fields_meadow")
colMeans(all_roe_frag, na.rm = T)

all_roe_frag <- as.data.frame(gather(all_roe_frag))

attach(all_roe_frag)
pairwise.wilcox.test(value, key, p.adjust.method = "bonf",exact=FALSE)
detach(all_roe_frag)

#all fallow fragmented ####

all_fallow_frag <- cbindX(ff90, ff50, ff_strong_fields, ff_strong_fields_meadow)
all_fallow_frag <- all_fallow_frag[, -c(1,2,4,5,7,8,10,11)]

colnames(all_fallow_frag) <- c("ff90", "ff50", "ff_strong_fields", "ff_strong_fields_meadow")
colMeans(all_fallow_frag, na.rm = T)

all_fallow_frag <- as.data.frame(gather(all_fallow_frag))

attach(all_fallow_frag)
pairwise.wilcox.test(value, key, p.adjust.method = "bonf",exact=FALSE)
detach(all_fallow_frag)







ls() #list
rm(list = ls()) #environment
cat("\014") # console
dev.off() # plots
