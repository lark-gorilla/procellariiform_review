#Initial import of data and formatting

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

rm(list=ls())

# make duplicate detector or maybe just print studies next to each other per species for manual checking

#read in data and formatting

#flight height sheet
dat_FLH<-read_xlsx('C:/Users/mmil0049/Downloads/procellariiform_OWF_review_FORMATTED (3).xlsx', sheet='flight.height', skip=1) # skip top 'checking' row

height_meta<-data.frame(ref=paste(dat_FLH[1,7:ncol(dat_FLH)]), str_split_fixed(dat_FLH[3,7:ncol(dat_FLH)], "@", 5))
names(height_meta)[2:6]=c("data.type", "place", "country", "marine region", "stage")

dat_FLH<-dat_FLH[-c(2,3),] # leaves reference row in there



#### Flight height review ####

## PERC RSZ

perc_height<-dat_FLH%>%dplyr::select(c(1:6, starts_with('perc'))) # selects first 6 cols with sp info then finds variable header word
names(perc_height)<-perc_height[1,];perc_height<-perc_height[-1,] # removes selection of variable name row...and sets with reference name

# takes from matrix format into Long format for calculating weighted mean --- NOT NEEDED?
perc_height_long<-NULL 
for(i in 1:nrow(perc_height))
{
  perc_height_long<-rbind(perc_height_long, data.frame(sp=perc_height[i,]$`Common name`,
  str_split_fixed(perc_height[i,7:ncol(perc_height)], "@", 3))) 
}

wt_mn<-function(x){
  t1<-str_split_fixed(x[7:ncol(perc_height)], "@", 2)%>%as.data.frame()
  t1$V2<-str_replace_all(t1$V2,c(L="0.33", M="0.66", H="1"))
  t1<-t1 %>% mutate_if(is.character,as.numeric)
  return(weighted.mean(t1$V1, t1$V2, na.rm=T))}

perc_height$ave_perc<-apply(perc_height, 1, FUN=wt_mn)


n_studies<-perc_height%>%group_by(`Genus common`)%>%summarise_all(funs(sum(!is.na(.))))

n_studies[,7:16][n_studies[,7:16]>0]<-1 # replace n with 1 to sum

n_stud_df<-data.frame(n_studies[,1:2], n_studies=rowSums(n_studies[,7:16]))

mn_mean<-perc_height%>%group_by(`Genus common`)%>%summarise(mnmn_perc=mean(ave_perc, na.rm=T))

perc_height$`Genus common`<-factor(perc_height$`Genus common`, levels=mn_mean[order(mn_mean$mnmn_perc, decreasing =T),] $ `Genus common`)
mn_mean$`Genus common`<-factor(mn_mean$`Genus common`, levels=mn_mean[order(mn_mean$mnmn_perc, decreasing =T),] $ `Genus common`)


ggplot()+
  geom_jitter(data=perc_height, aes(x=`Genus common`, y=ave_perc), height=0, width=0.15, alpha=0.3, size=2)+
  geom_point(data=mn_mean, aes(x=`Genus common`, y=mnmn_perc), size=4, colour='blue')+
  scale_y_continuous(breaks=seq(0, 20, 2))+
  labs(y="Percent time in Rotor Swept Zone")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.title.x = element_blank())
  
  

## FLIGHT HEIGHT

fl_height<-dat_FLH%>%dplyr::select(c(1:6, starts_with('mean')))
names(fl_height)<-fl_height[1,];fl_height<-fl_height[-1,]

fl_height_long<-NULL
for(i in 1:nrow(fl_height))
{
  fl_height_long<-rbind(fl_height_long, data.frame(sp=fl_height[i,]$`Common name`,
                                                       str_split_fixed(fl_height[i,7:ncol(fl_height)], "@", 2))) 
}

wt_mn<-function(x){
  t1<-str_split_fixed(x[7:ncol(fl_height)], "@", 2)%>%as.data.frame()
  t1$V2<-str_replace_all(t1$V2,c(L="0.33", M="0.66", H="1"))
  t1<-t1 %>% mutate_if(is.character,as.numeric)
  return(weighted.mean(t1$V1, t1$V2, na.rm=T))}

fl_height$ave_fl_height<-apply(fl_height, 1, FUN=wt_mn)


fl_height_nona<-fl_height[-which(is.na(fl_height$ave_fl_height)),]

fl_height_nona$`Common name`<-factor(fl_height_nona$`Common name`, levels=fl_height_nona[order(fl_height_nona$ave_fl_height, decreasing =T),] $ `Common name`)


n_studies<-fl_height_nona%>%group_by(`Common name`, ave_fl_height)%>%summarise_all(funs(sum(!is.na(.))))

n_studies[,7:13][n_studies[,7:13]>0]<-1 # replace n with 1 to sum

n_studies$nstud=rowSums(n_studies[,7:13])

n_stud_df$Common.name<-factor(n_stud_df$Common.name, levels=fl_height_nona[order(fl_height_nona$ave_fl_height, decreasing =T),] $ `Common name`)


ggplot()+
  geom_point(data=fl_height_nona, aes(x=`Common name`, y=ave_fl_height, colour=`Genus common`), size=3)+
  geom_text_repel(data=fl_height_nona, aes(x=`Common name`, y=ave_fl_height, label= `Common name`), size=5)+
  scale_y_continuous(breaks=seq(0, 12, 1))+
  labs(y="Altitude (m)")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.9, 0.8), 
        legend.text = element_text(size=12))




  geom_point(data=n_studies%>%filter(nstud>1), aes(x=`Common name`, y=ave_fl_height), fill=NA, size=5, shape=1)+

ggplot()+
  geom_jitter(data=perc_height, aes(x=`Genus common`, y=ave_perc), height=0, width=0.15, alpha=0.3, size=2)+
  geom_point(data=mn_mean, aes(x=`Genus common`, y=mnmn_perc), size=4, colour='blue')+
  scale_y_continuous(breaks=seq(0, 20, 2))+
  labs(y="Percent time in Rotor Swept Zone")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.title.x = element_blank())

