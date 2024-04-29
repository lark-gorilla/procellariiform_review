#Initial import of data and formatting

library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)

rm(list=ls())
setwd("C:/Users/mmil0049/OneDrive - Monash University/projects/01 southern seabird OWF review/")

# make duplicate detector or maybe just print studies next to each other per species for manual checking

#read in data and formatting

#flight height sheet
dat_FLH<-read_xlsx('C:/Users/mmil0049/Downloads/procellariiform_OWF_review_FORMATTED (3).xlsx', sheet='flight.height', skip=1) # skip top 'checking' row
which(is.na(dat_FLH[,1]))
dat_FLH<-dat_FLH[-which(is.na(dat_FLH[,1])),] # remove NA row normally.. at end

height_meta<-data.frame(ref=paste(dat_FLH[1,7:ncol(dat_FLH)]), str_split_fixed(dat_FLH[3,7:ncol(dat_FLH)], "@", 5))
names(height_meta)[2:6]=c("data.type", "place", "country", "marine region", "stage")

dat_FLH<-dat_FLH[-c(2,3),] # leaves reference row in there

#### *** Flight height review *** ####

## PERC RSZ ##

perc_height<-dat_FLH%>%dplyr::select(c(1:6, starts_with('perc'))) # selects first 6 cols with sp info then finds variable header word
names(perc_height)<-perc_height[1,];perc_height<-perc_height[-1,] # removes selection of variable name row...and sets with reference name

# calculate weighted mean based on three categories. Pool for RSZ height (>10, >20, >30)
wt_mn<-function(x){
  t1<-str_split_fixed(x[7:ncol(perc_height)], "@", 3)%>%as.data.frame()
  t1$V2<-str_replace_all(t1$V2,c(L="0.33", M="0.66", H="1"))
  t1<-t1 %>% mutate_if(is.character,as.numeric)
  return(weighted.mean(t1$V1, t1$V2, na.rm=T))}

#n studies, split per confidence class for meta
n_conf<-function(x){
  t1<-str_split_fixed(x[7:ncol(perc_height)], "@", 3)%>%as.data.frame()
  t2<-count(t1, V2)
  t3<-data.frame(n_H=0, n_M=0, n_L=0)
  if(length(t2[t2$V2=='H',]$n)>0){t3$n_H<-t2[t2$V2=='H',]$n}
  if(length(t2[t2$V2=='M',]$n)>0){t3$n_M<-t2[t2$V2=='M',]$n}
  if(length(t2[t2$V2=='L',]$n)>0){t3$n_L<-t2[t2$V2=='L',]$n}
  return(t3)}

perc_height$ave_perc<-apply(perc_height, 1, FUN=wt_mn)

perc_height<-cbind(perc_height, do.call("rbind", apply(perc_height, 1, FUN=n_conf)))

# Plots

#make genus level average
mn_mean<-perc_height%>%group_by(`Genus common`)%>%summarise(mnmn_perc=mean(ave_perc, na.rm=T))

#order by decreasing risk
perc_height$`Genus common`<-factor(perc_height$`Genus common`, levels=mn_mean[order(mn_mean$mnmn_perc, decreasing =T),] $ `Genus common`)
mn_mean$`Genus common`<-factor(mn_mean$`Genus common`, levels=mn_mean[order(mn_mean$mnmn_perc, decreasing =T),] $ `Genus common`)

#Species + genus averaged plot

ggplot()+
  geom_jitter(data=perc_height, aes(x=`Genus common`, y=ave_perc), height=0, width=0.15, alpha=0.3, size=2)+
  geom_point(data=mn_mean, aes(x=`Genus common`, y=mnmn_perc), size=4, colour='blue')+
  scale_y_continuous(breaks=seq(0, 20, 2))+
  labs(y="Percent time in Rotor Swept Zone")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.title.x = element_blank())

# Species plot to present reasoning for pooling RSZ heights: basically lots of error. Could test with stats if needed

perc_height_nona<-perc_height[-which(is.na(perc_height$ave_perc)),] #remove NA species
perc_height_nona$ave_perc<-NULL #remove ave column

perc_height_long<-NULL 
for(i in 1:nrow(perc_height))
{
  perc_height_long<-rbind(perc_height_long, data.frame(sp=perc_height_nona[i,]$`Common name`, gen=perc_height_nona[i,]$`Genus common`,
                                                       str_split_fixed(perc_height_nona[i,7:ncol(perc_height_nona)], "@", 3))) 
}

perc_height_long<-na.omit(perc_height_long) # rm NAs
perc_height_long$X1<-as.numeric(perc_height_long$X1) # convert to numeric

perc_height_long$gen<-factor(perc_height_long$gen, levels=mn_mean[order(mn_mean$mnmn_perc, decreasing =T),] $ `Genus common`)
perc_height_long$sp<-factor(perc_height_long$sp, levels=unique(perc_height_long[order(perc_height_long$gen),] $ sp))
perc_height_long$X2<-factor(perc_height_long$X2, levels=c("L", "M", "H"))

# one STSH outlier at 50 removed
ggplot()+
  geom_point(data=perc_height_long[perc_height_long$X1<26,], aes(x=sp, y=X1, colour=X3, size=X2), shape=1)+
  labs(y="Percent time in Rotor Swept Zone")+theme_bw()+
  scale_size_discrete(name = "Study\nconfidence")+
scale_colour_discrete(name = "Minimum height\nof RSZ (m)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  
## FLIGHT HEIGHT ##

fl_height<-dat_FLH%>%dplyr::select(c(1:6, starts_with('mean')))
names(fl_height)<-fl_height[1,];fl_height<-fl_height[-1,]

wt_mn<-function(x){
  t1<-str_split_fixed(x[7:ncol(fl_height)], "@", 2)%>%as.data.frame()
  t1$V2<-str_replace_all(t1$V2,c(L="0.33", M="0.66", H="1"))
  t1<-t1 %>% mutate_if(is.character,as.numeric)
  return(weighted.mean(t1$V1, t1$V2, na.rm=T))}

fl_height$ave_fl_height<-apply(fl_height, 1, FUN=wt_mn)

fl_height<-cbind(fl_height, do.call("rbind", apply(fl_height, 1, FUN=n_conf)))

#Plots

fl_height_nona<-fl_height[-which(is.na(fl_height$ave_fl_height)),]

fl_height_nona$`Common name`<-factor(fl_height_nona$`Common name`, levels=fl_height_nona[order(fl_height_nona$ave_fl_height, decreasing =T),] $ `Common name`)
fl_height_nona$Studies<-rowSums(fl_height_nona%>%select(c('n_H', 'n_M', 'n_L')))

ggplot()+
  geom_point(data=fl_height_nona, aes(x=`Common name`, y=ave_fl_height, colour=`Genus common`, shape=as.factor(Studies)), size=3)+
  geom_text_repel(data=fl_height_nona, aes(x=`Common name`, y=ave_fl_height, label= `Common name`), size=5)+
  scale_y_continuous(breaks=seq(0, 12, 1))+
  labs(y="Mean flight height (m)")+
  scale_shape_discrete(name="n Studies")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.9, 0.8), 
        legend.text = element_text(size=12))

## WRITE OUT TABLES ##

#write_xlsx(height_meta, "outputs/height_meta.xlsx")
#write_xlsx(perc_height, "outputs/height_perc.xlsx")
#write_xlsx(fl_height, "outputs/height_absl.xlsx")

#### ***  *** ####

#### *** Flight speed review *** ####
