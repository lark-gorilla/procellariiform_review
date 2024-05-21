#Initial import of data and formatting

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(meta)
library(estmeansd)


rm(list=ls())
setwd("C:/Users/mmil0049/OneDrive - Monash University/projects/01 southern seabird OWF review/")

# make duplicate detector or maybe just print studies next to each other per species for manual checking

#### *** Read in data and formatting *** ####

#flight height sheet
dat_FLH<-read_xlsx('C:/Users/mmil0049/Downloads/procellariiform_OWF_review_FORMATTED (12).xlsx', sheet='flight.height', skip=1) # skip top 'checking' row

height_meta<-data.frame(ref=paste(dat_FLH[1,7:ncol(dat_FLH)]), str_split_fixed(dat_FLH[3,7:ncol(dat_FLH)], "@", 5))
names(height_meta)[2:6]=c("data.type", "place", "country", "marine region", "stage")

dat_FLH<-dat_FLH[-c(2,3),] # leaves reference row in there
which(is.na(dat_FLH[,1]))
dat_FLH<-dat_FLH[-which(is.na(dat_FLH[,1])),] # remove NA row normally.. at end


#flight speed sheet
dat_FSD<-read_xlsx('C:/Users/mmil0049/Downloads/procellariiform_OWF_review_FORMATTED (12).xlsx', sheet='flight.speed', skip=1) # skip top 'checking' row

speed_meta<-data.frame(ref=paste(dat_FSD[1,7:ncol(dat_FSD)]), str_split_fixed(dat_FSD[3,7:ncol(dat_FSD)], "@", 5))
names(speed_meta)[2:6]=c("data.type", "place", "country", "marine region", "stage")

dat_FSD<-dat_FSD[-c(2,3),] # leaves reference row in there

#### ***  *** ####

#### *** Flight height review *** ####

#Funtion: n studies, split per confidence class for meta
n_conf<-function(x){
  t1<-str_split_fixed(x[7:ncol(perc_height)], "@", 3)%>%as.data.frame()
  t2<-count(t1, V2)
  t3<-data.frame(n_H=0, n_M=0, n_L=0)
  if(length(t2[t2$V2=='H',]$n)>0){t3$n_H<-t2[t2$V2=='H',]$n}
  if(length(t2[t2$V2=='M',]$n)>0){t3$n_M<-t2[t2$V2=='M',]$n}
  if(length(t2[t2$V2=='L',]$n)>0){t3$n_L<-t2[t2$V2=='L',]$n}
  return(t3)}

## PERC RSZ ##

perc_height<-dat_FLH%>%dplyr::select(c(1:6, starts_with('perc'))) # selects first 6 cols with sp info then finds variable header word
names(perc_height)<-perc_height[1,];perc_height<-perc_height[-1,] # removes selection of variable name row...and sets with reference name

# calculate weighted mean based on three categories. Pool for RSZ height (>10, >20, >30)
wt_mn<-function(x){
  t1<-str_split_fixed(x[7:ncol(perc_height)], "@", 3)%>%as.data.frame()
  t1$V2<-str_replace_all(t1$V2,c(L="0.33", M="0.66", H="1"))
  t1<-t1 %>% mutate_if(is.character,as.numeric)
  return(data.frame(wt_ave=weighted.mean(t1$V1, t1$V2, na.rm=T),
                    min=min(t1$V1, na.rm=T), max=max(t1$V1, na.rm=T)))}

perc_height_out<-data.frame(varib="percRSZ", `Common name`=perc_height$`Common name`,
                   do.call("rbind", apply(perc_height, 1, FUN=wt_mn)),
                   do.call("rbind", apply(perc_height, 1, FUN=n_conf)))


  
## FLIGHT HEIGHT ##

fl_height<-dat_FLH%>%dplyr::select(c(1:6, starts_with('mean')))
names(fl_height)<-fl_height[1,];fl_height<-fl_height[-1,]

wt_mn<-function(x){
  t1<-str_split_fixed(x[7:ncol(fl_height)], "@", 2)%>%as.data.frame()
  t1$V2<-str_replace_all(t1$V2,c(L="0.33", M="0.66", H="1"))
  t1<-t1 %>% mutate_if(is.character,as.numeric)
  return(data.frame(wt_ave=weighted.mean(t1$V1, t1$V2, na.rm=T),
                    min=min(t1$V1, na.rm=T), max=max(t1$V1, na.rm=T)))}

fl_height_out<-data.frame(varib="height", `Common name`=fl_height$`Common name`,
                   do.call("rbind", apply(fl_height, 1, FUN=wt_mn)),
                   do.call("rbind", apply(fl_height, 1, FUN=n_conf)))

#combine results

ht_res_out<-rbind(perc_height_out, fl_height_out)
ht_res_out<-ht_res_out%>%filter(!is.na(wt_ave))

ht_res_out$min<-ifelse(ht_res_out$min==ht_res_out$wt_ave, NA, ht_res_out$min)
ht_res_out$max<-ifelse(ht_res_out$max==ht_res_out$wt_ave, NA, ht_res_out$max)

#combine studies for appendix
app_p<-perc_height[,c(2, 7:16)]%>%pivot_longer(!"Common name", names_to='study', values_to = 'perc_rsz', values_drop_na = T)
app_p<-data.frame(varib="percRSZ", app_p, str_split_fixed(app_p$perc_rsz, "@", 3))

app_f<-fl_height[,c(2, 7:13)]%>%pivot_longer(!"Common name", names_to='study', values_to = 'fl_ht', values_drop_na = T)
app_f<-data.frame(varib="height", app_f, str_split_fixed(app_f$fl_ht, "@", 2), X3=NA)

app_height_out<-rbind(app_p[c("varib", "Common.name", "study", "X1", "X2", "X3")],
                      app_f[c("varib", "Common.name", "study", "X1", "X2", "X3")])


app_height_out<-app_height_out%>%arrange("varib", "study")

app_height_out<-left_join(app_height_out, height_meta, by=c("study"="ref"), multiple='first') # join in meta data

## WRITE OUT TABLES ##
#write_xlsx(app_height_out, "analyses/height_ready.xlsx")

# add extra attribs to resutls
ht_res_out<-left_join(ht_res_out, 
                      app_height_out%>%group_by(varib, Common.name)%>%summarise(stage=paste(unique(stage),
                      collapse=", "), region=paste(unique(`marine region`), collapse=", ")),
                      by=c("varib", "Common.name"))
#write_xlsx(ht_res_out, "outputs/height_results.xlsx")

# Anecdotal/ Max height to be added manually

#OLD
#write_xlsx(height_meta, "outputs/height_meta.xlsx")
#write_xlsx(perc_height, "outputs/height_perc.xlsx")
#write_xlsx(fl_height, "outputs/height_absl.xlsx")

#### ---- Plots  ---- ####

#Plots PERC RSZ

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

#Plots - FLIGHT HIEGHT

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
#### ---- Plots End  ---- ####

#### ***  *** ####

#### *** Flight speed review *** ####


#### *** prep data and cast to long format *** #### 

# function 1 splitting data
meta_split<-function(x)
{
  t1<-unlist(str_split(x, "@"))
  if(length(t1)==5)
  {
    t2<-as.data.frame(as.list(c('Combined', t1)), col.names=(c('V1', "V2", "V3", "V4", "V5", "V6")))
  }else{
    t2<-do.call("rbind", (split(t1, ceiling(1:length(t1)/6))))%>%as.data.frame()
  }
  return(t2)
}

## FLIGHT SPEED ##

fl_speed<-dat_FSD%>%dplyr::select(c(1:6, starts_with('flight')))
names(fl_speed)<-fl_speed[1,];fl_speed<-fl_speed[-1,]
 
long_speed<-NULL
for(i in 1:nrow(fl_speed))
{
  fl_speed_temp<-fl_speed[,7:ncol(fl_speed)]
  sel_sp<-fl_speed_temp[i,which(!is.na(fl_speed_temp[i,]))]
  if(length(sel_sp)==0){next} # skip sp with no data
  temp<-do.call("rbind", apply(sel_sp, 2, FUN=meta_split))
  temp<-data.frame(sp=fl_speed[i,]$`Common name`,
                   study=paste0(unlist(str_split(row.names(temp), "\\)"))[seq(1, ((nrow(temp)*2)-1), 2)], ")"), temp)
  long_speed<-rbind(long_speed, temp)
}
 
row.names(long_speed)<-NULL 
  

## MAX SPEED ##

max_speed<-dat_FSD%>%dplyr::select(c(1:6, starts_with('max')))
names(max_speed)<-max_speed[1,];max_speed<-max_speed[-1,]

long_max<-NULL
for(i in 1:nrow(max_speed))
{
  max_speed_temp<-max_speed[,7:ncol(max_speed)]
  sel_sp<-max_speed_temp[i,which(!is.na(max_speed_temp[i,]))]
  if(length(sel_sp)==0){next} # skip sp with no data
  temp<-do.call("rbind", apply(sel_sp, 2, FUN=meta_split))
  temp<-data.frame(sp=max_speed[i,]$`Common name`,
                   study=paste0(unlist(str_split(row.names(temp), "\\)"))[seq(1, ((nrow(temp)*2)-1), 2)], ")"), temp)
  long_max<-rbind(long_max, temp)
}

row.names(long_max)<-NULL 

# set missing "99s" to zero SDs for Max speed
long_max[long_max$V3==99,]$V3<-0 

## TRIP SPEED ##

trip_speed<-dat_FSD%>%dplyr::select(c(1:6, starts_with('trip')))
names(trip_speed)<-trip_speed[1,];trip_speed<-trip_speed[-1,]

long_trip<-NULL
for(i in 1:nrow(trip_speed))
{
  trip_speed_temp<-trip_speed[,7:ncol(trip_speed)]
  sel_sp<-trip_speed_temp[i,which(!is.na(trip_speed_temp[i,]))]
  if(length(sel_sp)==0){next} # skip sp with no data
  temp<-do.call("rbind", apply(sel_sp, 2, FUN=meta_split))
  temp<-data.frame(sp=trip_speed[i,]$`Common name`,
                   study=paste0(unlist(str_split(row.names(temp), "\\)"))[seq(1, ((nrow(temp)*2)-1), 2)], ")"), temp)
  long_trip<-rbind(long_trip, temp)
}

row.names(long_trip)<-NULL 

## combine all speed data

all_speed<-rbind(data.frame(varib="max", long_max),data.frame(varib="trip", long_trip),
      data.frame(varib="speed", long_speed))

#export dataset to check sample sizes for calculating mean and sd from median and min/max
#write_xlsx(all_speed[all_speed$V3=="NA",],"analyses/median_ss_checking.xlsx")


# Now impute missing means and SDs from median, min and max
# function using estmeansd package, using MLN method 
MLN_mnsd<-function(x){
  qe1<-mln.mean.sd(min.val = as.numeric(x["V4"]), med.val = as.numeric(x["V2"]),
                  max.val = as.numeric(x["V5"]), n = as.numeric(x["med_min_max_SS"]))
  rbind(qe1$est.mean, qe1$est.sd)}

# read in median/min/max sample size sheet 
med_mi_ma<-read_excel("analyses/median_ss_checking.xlsx")

med_mi_ma[c("V2", "V3")]<- round(t(apply(med_mi_ma, 1, FUN=MLN_mnsd)),2) # apply function and replace median and NA SD vals with calculatd vals

#recombine datasets and export

speed_ready<-rbind(med_mi_ma[c("varib" ,"sp","study","V1", "V2", "V3", "V6")],
      all_speed[all_speed$V3!="NA",c("varib" ,"sp","study","V1", "V2", "V3", "V6")])

names(speed_ready)[4]<-"subset"
names(speed_ready)[5]<-"mean"
names(speed_ready)[6]<-"sd"
names(speed_ready)[7]<-"n"

speed_ready$mean<-as.numeric(speed_ready$mean)
speed_ready$sd<-as.numeric(speed_ready$sd)
speed_ready$n<-as.numeric(speed_ready$n)

speed_ready<-speed_ready%>%arrange("varib", "study")

speed_ready<-left_join(speed_ready, speed_meta, by=c("study"="ref"), multiple='first') # join in meta data

# Imputing missing SDs for Max speed
# split out max from main dataset

sp_rd_1<-speed_ready[speed_ready$varib!="max",]
sp_rd_2<-speed_ready[speed_ready$varib=="max",]

ds_mean_sd<-mean(sp_rd_2[sp_rd_2$sd>0,]$sd, na.rm=T)# get average sd acroass whole max dataset
#3.2665

sp_rd_3<-NULL
for(i in unique(sp_rd_2$sp))
{
  l_temp<-sp_rd_2[sp_rd_2$sp==i,]
  if(prod(l_temp$sd)!=0){sp_rd_3<-rbind(sp_rd_3, l_temp)
  next()}
  if(sum(l_temp$sd)==0){
    l_temp$sd<-ds_mean_sd
    sp_rd_3<-rbind(sp_rd_3, l_temp)}else{
      l_temp[l_temp$sd==0,]$sd<-mean(l_temp[l_temp$sd>0,]$sd, na.rm=T)  
      sp_rd_3<-rbind(sp_rd_3, l_temp)}
}
 
speed_ready<-rbind(sp_rd_1, sp_rd_3) 

#write_xlsx(speed_ready,"analyses/speed_ready.xlsx")
      
#### ***  *** ####

#### *** Run meta-analysis: mixed models *** #### 

# run per species
# use random effects model making each study@stage a separate slab BUT using study as the random effect grouping level.

#escalc(measure='MNLN', mi=mean, sdi=sd, ni=n, data=sp_var, slab=study) useful to check assumptions

# read in prepped data
speed_ready<-read_excel("analyses/speed_ready.xlsx")

speed_meta_out<-NULL
for(i in unique(speed_ready$varib))
{
  var1<-speed_ready[speed_ready$varib==i,]
  for(j in unique(var1$sp))
  {
    out_temp<-NULL
    sp_var<-var1[var1$sp==j,]
    if(nrow(sp_var)==1){
      out_temp<-data.frame(sp_var[1,1:2],sp_var[1,5], LCI=NA, UCI=NA, sp_var[1,6:7],
                           n_studies= length(unique(sp_var$study)), stage=paste(unique(sp_var$stage), collapse=", "),
                           region=paste(unique(sp_var$`marine region`), collapse=", "))}else{
    
    m.mean <- metamean(n = n,
                       mean = mean,
                       sd = sd,
                       studlab = paste(study, subset, sep="-"),
                       cluster=study,
                       data = sp_var,
                       sm = "MLN", # use log transformation instead. Probably, advisable when using non-negative data
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML",
                       title = paste(i, "Scores"))
    
    #print(summary(m.mean))
    #print(paste(i, j))
    #print(forest(m.mean))
    #readline("")
    
    out_temp<-data.frame(sp_var[1,1:2],mean=exp(m.mean$TE.random), LCI=exp(m.mean$lower.random), UCI=exp(m.mean$upper.random), sd=NA, n=sum(m.mean$n), 
                         n_studies= length(unique(sp_var$study)), stage=paste(unique(sp_var$stage), collapse=", "), region=paste(unique(sp_var$`marine region`), collapse=", "))
    }
    speed_meta_out<-rbind(speed_meta_out, out_temp)
  }
}
#warnings() fine - just for single studies entered into a re model

# write out results

#write_xlsx(speed_meta_out, "outputs/speed_results.xlsx")

#### ***  *** ####

