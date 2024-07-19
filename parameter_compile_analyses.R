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
dat_FLH<-read_xlsx('C:/Users/mmil0049/Downloads/procellariiform_OWF_review_FORMATTED (23).xlsx', sheet='flight.height', skip=1) # skip top 'checking' row

height_meta<-data.frame(ref=paste(dat_FLH[1,7:ncol(dat_FLH)]), str_split_fixed(dat_FLH[3,7:ncol(dat_FLH)], "@", 5))
names(height_meta)[2:6]=c("data.type", "place", "country", "marine region", "stage")

dat_FLH<-dat_FLH[-c(2,3),] # leaves reference row in there
which(is.na(dat_FLH[,1]))
dat_FLH<-dat_FLH[-which(is.na(dat_FLH[,1])),] # remove NA row normally.. at end


#flight speed sheet
dat_FSD<-read_xlsx('C:/Users/mmil0049/Downloads/procellariiform_OWF_review_FORMATTED (23).xlsx', sheet='flight.speed', skip=1) # skip top 'checking' row

speed_meta<-data.frame(ref=paste(dat_FSD[1,7:ncol(dat_FSD)]), str_split_fixed(dat_FSD[3,7:ncol(dat_FSD)], "@", 5))
names(speed_meta)[2:6]=c("data.type", "place", "country", "marine region", "stage")

dat_FSD<-dat_FSD[-c(2,3),] # leaves reference row in there

#NAF sheet
dat_NAF<-read_xlsx('C:/Users/mmil0049/Downloads/procellariiform_OWF_review_FORMATTED (23).xlsx', sheet='nocturnal.activity', skip=1) # skip top 'checking' row

NAF_meta<-data.frame(ref=paste(dat_NAF[1,7:ncol(dat_NAF)]), str_split_fixed(dat_NAF[3,7:ncol(dat_NAF)], "@", 5))
names(NAF_meta)[2:6]=c("data.type", "place", "country", "marine region", "stage")

dat_NAF<-dat_NAF[-c(2,3),] # leaves reference row in there

#### *** ####

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

# add extra attribs to resutls

ht_res_out<-left_join(ht_res_out, 
                      app_height_out%>%group_by(varib, Common.name)%>%summarise(stage=paste(unique(stage),
                      collapse=", "), region=paste(unique(`marine region`), collapse=", ")),
                      by=c("varib", "Common.name"))

#anecdotal Max height, added manually

## WRITE OUT TABLES ##
#write_xlsx(app_height_out, "analyses/height_ready.xlsx")
#write_xlsx(ht_res_out, "outputs/height_results.xlsx")

#### ***  *** ####


#### *** Nocturnal Activity Factor review *** ####

#Bonnet-Lebrun will need a tweak to region/location metadata.
# need to think if we can integrate 2 lit review studies into formal meta-analysis

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

# function 2 splitting data
meta_split_2<-function(x)
{
  t1<-unlist(str_split(x, "@"))
  t2<-do.call("rbind", (split(t1, ceiling(1:length(t1)/4))))%>%as.data.frame()
  return(t2)
}

## TIME ON WATER ##

t_on_water<-dat_NAF%>%dplyr::select(c(1:6, starts_with('timeon')))
names(t_on_water)<-t_on_water[1,];t_on_water<-t_on_water[-1,]

long_t_water<-NULL
for(i in 1:nrow(t_on_water))
{
  fl_speed_temp<-t_on_water[,7:ncol(t_on_water)]
  sel_sp<-fl_speed_temp[i,which(!is.na(fl_speed_temp[i,]))]
  if(length(sel_sp)==0){next} # skip sp with no data
  temp<-do.call("rbind", apply(sel_sp, 2, FUN=meta_split))
  temp<-data.frame(sp=t_on_water[i,]$`Common name`,
                   study=paste0(unlist(str_split(row.names(temp), "\\)"))[seq(1, ((nrow(temp)*2)-1), 2)], ")"), temp)
  long_t_water<-rbind(long_t_water, temp)
}

row.names(long_t_water)<-NULL 

long_t_water$V2<-100-as.numeric(long_t_water$V2) # reverse so that it now represents time flying
# Same thing for min max but flip columns as now LCI is UCI
temp1<-long_t_water$V4
long_t_water$V4<-100-as.numeric(long_t_water$V5)
long_t_water$V5<-100-as.numeric(temp1)

## TIME FLYING ##

t_fly<-dat_NAF%>%dplyr::select(c(1:6, starts_with('timefly')))
names(t_fly)<-t_fly[1,];t_fly<-t_fly[-1,]

long_t_fly<-NULL
for(i in 1:nrow(t_fly))
{
  fl_speed_temp<-t_fly[,7:ncol(t_fly)]
  sel_sp<-fl_speed_temp[i,which(!is.na(fl_speed_temp[i,]))]
  if(length(sel_sp)==0){next} # skip sp with no data
  temp<-do.call("rbind", apply(sel_sp, 2, FUN=meta_split))
  temp<-data.frame(sp=t_fly[i,]$`Common name`,
                   study=paste0(unlist(str_split(row.names(temp), "\\)"))[seq(1, ((nrow(temp)*2)-1), 2)], ")"), temp)
  long_t_fly<-rbind(long_t_fly, temp)
}

row.names(long_t_fly)<-NULL 

## NFI ##

NFI_t<-dat_NAF%>%dplyr::select(c(1:6, starts_with('NFI')))
names(NFI_t)<-NFI_t[1,];NFI_t<-NFI_t[-1,]

long_nfi<-NULL
for(i in 1:nrow(NFI_t))
{
  fl_speed_temp<-NFI_t[,7:ncol(NFI_t)]
  sel_sp<-fl_speed_temp[i,which(!is.na(fl_speed_temp[i,]))]
  if(length(sel_sp)==0){next} # skip sp with no data
  temp<-do.call("rbind", apply(sel_sp, 2, FUN=meta_split))
  temp<-data.frame(sp=NFI_t[i,]$`Common name`,
                   study=paste0(unlist(str_split(row.names(temp), "\\)"))[seq(1, ((nrow(temp)*2)-1), 2)], ")"), temp)
  long_nfi<-rbind(long_nfi, temp)
}

row.names(long_nfi)<-NULL 

## Lit review ##

lit_nf<-dat_NAF%>%dplyr::select(c(1:6, starts_with('Review')))
names(lit_nf)<-lit_nf[1,];lit_nf<-lit_nf[-1,]

long_nlit<-NULL
for(i in 1:nrow(lit_nf))
{
  fl_speed_temp<-lit_nf[,7:ncol(lit_nf)]
  sel_sp<-fl_speed_temp[i,which(!is.na(fl_speed_temp[i,]))]
  if(length(sel_sp)==0){next} # skip sp with no data
  temp<-do.call("rbind", apply(sel_sp, 2, FUN=meta_split_2))
  temp<-data.frame(sp=lit_nf[i,]$`Common name`,
                   study=paste0(unlist(str_split(row.names(temp), "\\)"))[seq(1, ((nrow(temp)*2)-1), 2)], ")"), temp)
  long_nlit<-rbind(long_nlit, temp)
}

row.names(long_nlit)<-NULL 

#Calc mean and sd from min max and reformt to bind wit other datasets
long_nlit<-data.frame(long_nlit[,1:3],  V2=apply(long_nlit, 1, FUN=function(x){mean(c(as.numeric(x["V2"]), as.numeric(x["V3"])))}),
                      V3=apply(long_nlit, 1, FUN=function(x){sd(c(as.numeric(x["V2"]), as.numeric(x["V3"])))}), 
                      V4=long_nlit$V2, V5=long_nlit$V3, V6=long_nlit$V4)

## combine all NAF data

all_naf<-rbind(data.frame(varib="t_fly", long_t_water),data.frame(varib="t_fly", long_t_fly),
                 data.frame(varib="nfi", long_nfi), data.frame(varib="nlit", long_nlit))


# Now impute missing means and SDs from median, min and max
# function using estmeansd package, using MLN method 
MLN_mnsd<-function(x){
  qe1<-mln.mean.sd(min.val = as.numeric(x["V4"]), med.val = as.numeric(x["V2"]),
                   max.val = as.numeric(x["V5"]), n = as.numeric(x["med_min_max_SS"]))
  rbind(qe1$est.mean, qe1$est.sd)}

# read in median/min/max sample size sheet 
med_mi_ma<-read_excel("analyses/median_ss_checking.xlsx")%>%as.data.frame()

med_mi_ma[med_mi_ma$varib=="t_fly",c("V2", "V3")]<- round(t(apply(med_mi_ma%>%filter(varib=="t_fly"), 1, FUN=MLN_mnsd)),2) # apply function and replace median and NA SD vals with calculatd vals

med_mi_ma[med_mi_ma$varib=="nfi",c("V2", "V4", "V5")]<-med_mi_ma[med_mi_ma$varib=="nfi",c("V2", "V4", "V5")]+1# hack for NFI to make vals positive
med_mi_ma[med_mi_ma$varib=="nfi",c("V2", "V3")]<- round(t(apply(med_mi_ma%>%filter(varib=="nfi"), 1, FUN=MLN_mnsd)),2) # apply function and replace median and NA SD vals with calculatd vals
med_mi_ma[med_mi_ma$varib=="nfi",c("V2")]<- med_mi_ma[med_mi_ma$varib=="nfi",c("V2")]-1 # then reverse for mean val to rescale 

#recombine datasets

naf_ready<-rbind(med_mi_ma[med_mi_ma$varib%in%c("nfi", "t_fly"), c("varib" ,"sp","study","V1", "V2", "V3", "V6")],
                   all_naf[all_naf$V3!="NA",c("varib" ,"sp","study","V1", "V2", "V3", "V6")])

names(naf_ready)[4]<-"subset"
names(naf_ready)[5]<-"mean"
names(naf_ready)[6]<-"sd"
names(naf_ready)[7]<-"n"

naf_ready$mean<-as.numeric(naf_ready$mean)
naf_ready$sd<-as.numeric(naf_ready$sd)
# naf_ready$n<-as.numeric(naf_ready$n) still have chars in there from lit

naf_ready<-naf_ready%>%arrange("varib", "study", "sp", "subset")

naf_ready<-left_join(naf_ready, NAF_meta, by=c("study"="ref"), multiple='first') # join in meta data

#write_xlsx(naf_ready,"analyses/nocturnal_activity_ready.xlsx") # export pre NFI version for appendix?

# convert to NFI

naf_4_nfi<-naf_ready%>%filter(varib!="nfi")

naf_4_nfi$split<-NA
naf_4_nfi$dn<-NA

#day
naf_4_nfi$split<-ifelse(1:nrow(naf_4_nfi)%in%grep("day",naf_4_nfi$subset  , ignore.case=T),
                           gsub("day", "", naf_4_nfi$subset  , ignore.case=T), naf_4_nfi$split)
naf_4_nfi$dn<-ifelse(1:nrow(naf_4_nfi)%in%grep("day",naf_4_nfi$subset  , ignore.case=T), "day", naf_4_nfi$dn)
#night
naf_4_nfi$split<-ifelse(1:nrow(naf_4_nfi)%in%grep("night",naf_4_nfi$subset  , ignore.case=T),
                           gsub("night", "", naf_4_nfi$subset  , ignore.case=T), naf_4_nfi$split)
naf_4_nfi$dn<-ifelse(1:nrow(naf_4_nfi)%in%grep("night",naf_4_nfi$subset  , ignore.case=T), "night", naf_4_nfi$dn)


# loop to calculate NFI and correctly propagate errors #https://www.geol.lsu.edu/jlorenzo/geophysics/uncertainties/Uncertaintiespart2.html

nfi_conv<-NULL
for(i in unique(naf_4_nfi$sp))
{
  sp1<-naf_4_nfi[naf_4_nfi$sp==i,]
  
  for(j in unique(sp1$study))
  {
    stp1<-sp1[sp1$study==j,]
    
    for(k in unique(stp1$split))
    {
      sub1<-stp1[stp1$split==k,]
      #2 part calc: 1) Z1=%FN − %FD ; 2) Z2= Z1/max (%FN , %FD)
      if(sub1[sub1$dn=="night",]$mean==sub1[sub1$dn=="day",]$mean){sub1[sub1$dn=="day",]$mean<-sub1[sub1$dn=="day",]$mean+0.00001} # add tiny constant if exactly equal
      
      Z1=sub1[sub1$dn=="night",]$mean-sub1[sub1$dn=="day",]$mean
      dZ1=sqrt((sub1[sub1$dn=="night",]$sd^2)+(sub1[sub1$dn=="day",]$sd^2))
      
      Z2=Z1/max(sub1$mean)
      dZ2= sqrt(((dZ1/Z1)^2) + ((sub1[which.max(sub1$mean),]$sd/sub1[which.max(sub1$mean),]$mean)^2))*abs(Z2) 
      
      out<-sub1[1,]
      out$mean=Z2
      out$sd=dZ2
      
      nfi_conv<-rbind(nfi_conv, out)
    }
  }
}

nfi_conv$subset<-nfi_conv$split
nfi_conv$varib<-"nfi"
nfi_conv$split<-NULL
nfi_conv$dn<-NULL

nfi_ready<-rbind(naf_ready%>%filter(varib=="nfi"), nfi_conv)

nfi_ready<-nfi_ready%>%arrange("varib", "study", "sp")

#write_xlsx(nfi_ready,"analyses/NFI_ready.xlsx")

#### ***  *** ####

#### *** Run NFI meta-analysis: mixed models *** #### 

# run per species
# use random effects model making each study@stage a separate slab BUT using study as the random effect grouping level.

#es1<-escalc(measure='OR', yi=m2, vi=(se2^2)*as.numeric(n), data=sp_var, slab=study) #useful to check assumptions
#es1$ID<-1:nrow(es1)
#res <- rma.mv(yi, vi, data=es1, random=~1|study/ID, slab=paste(study, subset, sep=", "), method="REML" )
#predict(res, transf=fun1)

# read in prepped data
nfi_ready<-read_excel("analyses/NFI_ready.xlsx")

# edit to set hacked 'almost zero' vals back to zero
nfi_ready[nfi_ready$mean>-0.000001 & nfi_ready$mean<0,]$mean<-0


nfi_meta_out<-NULL

for(j in unique(nfi_ready$sp))
{
  out_temp<-NULL
  sp_var<-nfi_ready[nfi_ready$sp==j,]
  if(nrow(sp_var)==1){
    out_temp<-data.frame(sp_var[1,1:2],sp_var[1,5], LCI=NA, UCI=NA, sp_var[1,6:7],
                         n_studies= length(unique(sp_var$study)), stage=paste(unique(sp_var$stage), collapse=", "),
                         region=paste(unique(sp_var$`marine region`), collapse=", "))}else{
                           
                           # tweak to lit review data to impute n
                           if(NA%in%(as.numeric(sp_var$n))){
                             sp_var$n<-ifelse(sp_var$n=="M", median(as.numeric(sp_var$n), na.rm=T), sp_var$n)  
                             sp_var$n<-ifelse(sp_var$n=="L", min(as.numeric(sp_var$n), na.rm=T), sp_var$n)  
                           }
                           
                           sp_var$m2<-qlogis((sp_var$mean+1)/2) # apply correction to rescale -1:1 NFI to 0:1 proportion, then take logit 
                           sp_var$se2<-(sp_var$sd/2)/(sqrt(as.numeric(sp_var$n))) # correction to sd doesn't need +1 as already non negative 
                           
                           m.mean <- metagen(TE = m2,
                                              seTE = se2,
                                              studlab = paste(study, subset, sep="-"),
                                              cluster=study,
                                              data = sp_var,
                                              sm = "OR",
                                              fixed = FALSE,
                                              random = TRUE,
                                              method.tau = "ML",
                                              title = paste(j, "Scores"), backtransf = F)
                           
                           #print(summary(m.mean))
                           #print(paste(i, j))
                           #print(forest(m.mean))
                           #readline("")
                           
                           out_temp<-data.frame(sp_var[1,1:2],mean=(plogis(m.mean$TE.random)*2)-1,
                                                LCI=(plogis(m.mean$lower.random)*2)-1,
                                                UCI=(plogis(m.mean$upper.random)*2)-1,
                                                sd=NA,
                                                n=sum(as.numeric(sp_var$n)), 
                                                n_studies= length(unique(sp_var$study)), stage=paste(unique(sp_var$stage), collapse=", "), region=paste(unique(sp_var$`marine region`), collapse=", "))
                           
                           # calculate SDs from CIs and n https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
                           out_temp$sd<-sqrt(m.mean$k)*(out_temp$UCI- out_temp$LCI)/3.92
                           }
  nfi_meta_out<-rbind(nfi_meta_out, out_temp)
}


#warnings() fine - just for single studies entered into a re model

# write out results

#write_xlsx(nfi_meta_out, "outputs/NFI_results.xlsx")
#### ***  *** ####

#### *** Flight speed review *** ####

#### ***  *** ####
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
med_mi_ma<-read_excel("analyses/median_ss_checking.xlsx")%>%as.data.frame()

med_mi_ma[med_mi_ma$varib%in%c("speed", "trip", "max"),c("V2", "V3")]<- round(t(apply(med_mi_ma[med_mi_ma$varib%in%c("speed", "trip", "max"),], 1, FUN=MLN_mnsd)),2) # apply function and replace median and NA SD vals with calculatd vals

#recombine datasets and export

speed_ready<-rbind(med_mi_ma[med_mi_ma$varib%in%c("speed", "trip", "max"),c("varib" ,"sp","study","V1", "V2", "V3", "V6")],
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
#### *** Run Speed meta-analysis: mixed models *** #### 

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
    
    out_temp<-data.frame(sp_var[1,1:2],mean=exp(m.mean$TE.random), LCI=exp(m.mean$lower.random), UCI=exp(m.mean$upper.random),
                         sd=NA, n=sum(m.mean$n), 
                         n_studies= length(unique(sp_var$study)), stage=paste(unique(sp_var$stage), collapse=", "),
                         region=paste(unique(sp_var$`marine region`), collapse=", "))
    
    # calculate SDs from CIs and n https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
        out_temp$sd<-sqrt(m.mean$k)*(out_temp$UCI- out_temp$LCI)/3.92
    }
    speed_meta_out<-rbind(speed_meta_out, out_temp)
  }
}
#warnings() fine - just for single studies entered into a re model

# write out results

#write_xlsx(speed_meta_out, "outputs/speed_results.xlsx")

#### ***  *** ####

nfi_ready<-read_xlsx("analyses/NFI_ready.xlsx")
height_ready<-read_xlsx("analyses/height_ready.xlsx")
speed_ready<-read_xlsx("analyses/speed_ready.xlsx")


#### *** Make first review summary table *** #### 

# join each to extended flight group
nfi_ready<-read_xlsx("analyses/NFI_ready.xlsx")
height_ready<-read_xlsx("analyses/height_ready.xlsx")
speed_ready<-read_xlsx("analyses/speed_ready.xlsx")

flg<-read_xlsx("data/procellariiform_flight_groups.xlsx")

speed_ready<-left_join(speed_ready, flg[,c(2,8)], by=join_by("sp"==`Common name`))
nfi_ready<-left_join(nfi_ready, flg[,c(2,8)], by=join_by("sp"==`Common name`))
height_ready<-left_join(height_ready, flg[,c(2,8)], by=join_by("Common.name"==`Common name`))
names(height_ready)[2]<-"sp"

tab1_dat<-rbind(speed_ready%>%select(varib,sp, study, data.type,`Extended flight group`)%>%mutate(id="speed"),
                height_ready%>%select(varib,sp, study, data.type,`Extended flight group`)%>%mutate(id="height"),
                nfi_ready%>%select(varib,sp, study, data.type,`Extended flight group`)%>%mutate(id="nfi"))


tab1_dat$data.group<-"Vessel-based"

tab1_dat[tab1_dat$data.type%in% c("GPS" ,   "PTT",    "GPS-PTT"  ,   "GPS and PTT" ,                   
 "PPT"  ,  "GPS & PTT" ,     "Barometric pressure sensor" ,"Geolocator" , "GPS/Geolocator",                   
 "Wet-dry logger"  ,   "Wet-dry loggers" ,  "Activity loggers",   "GLS" ,                           
   "Immersion loggers"  ,    "Temperature logger" ,"TDR" ),]$data.group<-"Bio-logger"

tab1_dat[tab1_dat$data.type=="Literature review",]$data.group<-"Literature review"

tab1_dat[tab1_dat$data.type%in% c("Land based ornithodolite"  , "land based radar"  ,"Infrared binoculars and markers",
                                  "Aerial photogrammetry"   ,  "Ornithodolite from headland" ),]$data.group<-"Aerial/land-based"

tab1_dat<-tab1_dat %>%
  pivot_wider(names_from = data.group, values_from=data.group, values_fn =~1, values_fill = 0)
  
tab1_sum_1<-tab1_dat%>%group_by(varib, `Extended flight group`)%>%
  summarise(n_sp=length(unique(sp)), n_study=length(unique(study)))
            
tab1_sum_2<-tab1_dat%>%group_by(varib, `Extended flight group`)%>%distinct(study,.keep_all = T)%>%ungroup()%>%
  group_by(varib, `Extended flight group`)%>%
  summarise(`Vessel-based`=sum(`Vessel-based`), `Bio-logger`=sum(`Bio-logger`),
            `Aerial/land-based`=sum(`Aerial/land-based`),`Literature review`=sum(`Literature review`))

tab1_sum_3<-left_join(tab1_sum_1, tab1_sum_2, by=join_by("varib", `Extended flight group`))

tab1_sum_3<-left_join(tab1_sum_3, table(flg$`Extended flight group`)%>%data.frame(), by=join_by(`Extended flight group`=='Var1'))
#tab1_sum_3$sp_perc<-ceiling(tab1_sum_3$n_sp/tab1_sum_3$Freq*100) # not used
#tab1_sum_3$caption<-paste0(tab1_sum_3$n_sp, " (", tab1_sum_3$sp_perc, "%)")
# export table
tab1_out<-tab1_sum_3%>%select(varib, `Extended flight group`, n_sp, Freq)%>%pivot_wider(names_from = varib, values_from=n_sp, values_fill = NA)
tab1_out$`Extended flight group`<-factor(tab1_out$`Extended flight group`, levels=
                                       c("Great albatrosses", "Sooty albatrosses", "Small albatrosses", "Giant petrels", "Fulmars", "Procellaria petrels", 
                                         "Large gadfly petrels", "Small gadfly petrels", "Calonectris shearwaters", "Surface feeding shearwaters", "Diving shearwaters", 
                                         "Manx type shearwaters", "Prions", "Diving petrels", "Oceanodroma", "Frigate petrels", "Oceanites"))
tab1_out<-tab1_out%>%arrange(`Extended flight group`)
#write_xlsx(tab1_out, "outputs/review_summary_table_subparameters.xlsx") # join in excel to flg mean+sd from main table

# write out pie charts
for(i in 1:nrow(tab1_sum_3))
{
  r1<-tab1_sum_3[i,]
  r2<-r1%>%select(5:8)%>%pivot_longer(!varib, names_to = 'data.type', values_to='count')
  r2<-filter(r2, count>0)
  
  p1<-ggplot(r2, aes(x = "", y = count, fill = data.type)) +
    geom_col(color = "black", size=0.1) +
    geom_text(aes(label = count),
              position = position_stack(vjust = 0.5), size=2.5) +
    scale_fill_manual(values = c( `Vessel-based` = "#00BFC4", `Bio-logger` = "#F8766D",
                                  `Literature review` = "#C77CFF", `Aerial/land-based`="#7CAE00"))+
    coord_polar(theta = "y") +
    theme_void()+  theme(legend.position = "none")
  
  if(nrow(r2)==1){
  p1<-ggplot(r2, aes(x = "", y = count, fill = data.type)) +
    geom_col() +
    geom_text(aes(label = count),
              position = position_stack(vjust = 1), size=2.5) +
    scale_fill_manual(values = c( `Vessel-based` = "#00BFC4", `Bio-logger` = "#F8766D",
                                  `Literature review` = "#C77CFF", `Aerial/land-based`="#7CAE00"))+
    coord_polar(theta = "y") +
    theme_void()+  theme(legend.position = "none")}
  
  ggsave(paste0("outputs/review_summary_table_subparameter_plots/", paste(r1[c(2,1)], collapse="_"), ".png"), 
         p1, width = 0.8, height = 0.8, units = "cm")
}

# Per parameter totals for paper
length(unique(tab1_dat$sp)) # n sp overall

n_sps<-tab1_dat%>%group_by(id, sp)%>%summarise_all(first)%>%as.data.frame()
t1<-table(n_sps$sp)%>%as.data.frame()%>%arrange(Freq)
nrow(t1[t1$Freq>1,]);nrow(t1[t1$Freq>2,]) # n with 2+ and 3+ parameters

# n study types overall
tab1_dat%>%group_by(study)%>%summarise_all(first)%>%ungroup()%>%summarise_if(is.numeric,sum) # Pennyciuk 1982 = land and vessel based

# percentages per data group
t1_d<-tab1_dat%>%group_by(varib, study)%>%summarise_all(first)%>%ungroup%>%group_by(varib)%>%summarise_if(is.numeric,sum)
t1_d$sum=rowSums(t1_d[,2:5])
round(t1_d[,2:5]/t1_d$sum*100)

# n species per flight group
tab1_sum_3%>%group_by(varib)%>%summarise(med=median(n_study), mn=mean(n_study), min=min(n_study), max=max(n_study))
#


#### ***  *** ####

#### *** Flight group means calculated via meta analyses approach *** #### 

#read in results data
speed_ready<-read_excel("analyses/speed_ready.xlsx")
nfi_ready<-read_excel("analyses/NFI_ready.xlsx")

# tiny tweak to Yelkouan Trip speed: impute sd from other two species
speed_ready[speed_ready$varib=='trip'& speed_ready$sp=='Yelkouan Shearwater',]$sd<-mean(c(3.28, 0.298))
# edit to set hacked 'almost zero' vals back to zero
nfi_ready[nfi_ready$mean>-0.000001 & nfi_ready$mean<0,]$mean<-0

# join each to extended flight group
flg<-read_xlsx("data/procellariiform_flight_groups.xlsx")
speed_ready<-left_join(speed_ready, flg[,c(2,8)], by=join_by("sp"==`Common name`))
nfi_ready<-left_join(nfi_ready, flg[,c(2,8)], by=join_by("sp"==`Common name`))

# Speed model

speed_meta_out<-NULL
for(i in unique(speed_ready$varib))
{
  var1<-speed_ready[speed_ready$varib==i,]
  for(j in unique(var1$`Extended flight group`))
  {
    out_temp<-NULL
    sp_var<-var1[var1$`Extended flight group`==j,]
    if(nrow(sp_var)==1){
      out_temp<-data.frame(sp_var[1,c(1,13)], mean=sp_var$mean, LCI=NA, UCI=NA, sd=NA,
                           n=sp_var$n,n_sp=1, 
                           n_studies= 1, stage=paste(unique(sp_var$stage), collapse=", "),
                           region=paste(unique(sp_var$`marine region`), collapse=", "))
                           }else{
 
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
                             
                             
                             out_temp<-data.frame(sp_var[1,c(1,13)],mean=exp(m.mean$TE.random), LCI=exp(m.mean$lower.random), UCI=exp(m.mean$upper.random),
                                                  sd=NA, n=sum(m.mean$n), n_sp=length(unique(sp_var$sp)),
                                                  n_studies= length(unique(sp_var$study)), stage=paste(unique(sp_var$stage), collapse=", "),
                                                  region=paste(unique(sp_var$`marine region`), collapse=", "))
                             
                             # calculate SDs from CIs and n https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
                             out_temp$sd<-sqrt(m.mean$k)*(out_temp$UCI- out_temp$LCI)/3.92
  }
    speed_meta_out<-rbind(speed_meta_out, out_temp)
  }
}


# NFI model

nfi_meta_out<-NULL
for(j in unique(nfi_ready$`Extended flight group`))
{
  out_temp<-NULL
  sp_var<-nfi_ready[nfi_ready$`Extended flight group`==j,]
  if(nrow(sp_var)==1){
    out_temp<-data.frame(sp_var[1,c(1,13)], mean=sp_var$mean, LCI=NA, UCI=NA, sd=NA,
                         n=sp_var$n,n_sp=1, 
                         n_studies= 1, stage=paste(unique(sp_var$stage), collapse=", "),
                         region=paste(unique(sp_var$`marine region`), collapse=", "))
                        }else{
    
                           # tweak to lit review data to impute n
                           if(NA%in%(as.numeric(sp_var$n))){
                             sp_var$n<-ifelse(sp_var$n=="M", median(as.numeric(sp_var$n), na.rm=T), sp_var$n)  
                             sp_var$n<-ifelse(sp_var$n=="L", min(as.numeric(sp_var$n), na.rm=T), sp_var$n)  
                           }
                           
                           sp_var$m2<-qlogis((sp_var$mean+1)/2) # apply correction to rescale -1:1 NFI to 0:1 proportion, then take logit 
                           sp_var$se2<-(sp_var$sd/2)/(sqrt(as.numeric(sp_var$n))) # correction to sd doesn't need +1 as already non negative 
                           
                           m.mean <- metagen(TE = m2,
                                             seTE = se2,
                                             studlab = paste(study, subset, sep="-"),
                                             cluster=study,
                                             data = sp_var,
                                             sm = "OR",
                                             fixed = FALSE,
                                             random = TRUE,
                                             method.tau = "ML",
                                             title = paste(j, "Scores"), backtransf = F)

                           out_temp<-data.frame(sp_var[1,c(1, 13)],mean=(plogis(m.mean$TE.random)*2)-1,
                                                LCI=(plogis(m.mean$lower.random)*2)-1,
                                                UCI=(plogis(m.mean$upper.random)*2)-1,
                                                sd=NA,
                                                n=sum(as.numeric(sp_var$n)), n_sp=length(unique(sp_var$sp)),
                                                n_studies= length(unique(sp_var$study)), stage=paste(unique(sp_var$stage), collapse=", "), region=paste(unique(sp_var$`marine region`), collapse=", "))
                           
                           # calculate SDs from CIs and n https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
                           out_temp$sd<-sqrt(m.mean$k)*(out_temp$UCI- out_temp$LCI)/3.92
                      }
  nfi_meta_out<-rbind(nfi_meta_out, out_temp)
}

fg_metaz<-rbind(speed_meta_out, nfi_meta_out)
fg_metaz[fg_metaz$n_studies==1 &fg_metaz$n_sp==1,][c('LCI', 'UCI', 'sd')]<-NA

#write_xlsx(fg_metaz, "outputs/fg_meta_means.xlsx")
#### ***  *** ####

#### *** Combine results tables into main paper table *** #### 

#read in results data
speed_meta_out<-read_xlsx("outputs/speed_results.xlsx")
ht_res_out<-read_xlsx("outputs/height_results.xlsx")
nfi_meta_out<-read_xlsx("outputs/NFI_results.xlsx")

# join each to extended flight group

flg<-read_xlsx("data/procellariiform_flight_groups.xlsx")
speed_meta_out<-left_join(speed_meta_out, flg[,c(2,8)], by=join_by("sp"==`Common name`))
nfi_meta_out<-left_join(nfi_meta_out, flg[,c(2,8)], by=join_by("sp"==`Common name`))
ht_res_out<-left_join(ht_res_out, flg[,c(2,8)], by=join_by("Common.name"==`Common name`))

# Anecdotal/ Max HEIGHT to be added manually

# pivot data

piv_speed<-speed_meta_out%>%pivot_wider(id_cols=c(`Extended flight group`, sp), names_from=varib,
                             values_from=c(mean, LCI, UCI, sd, n, n_studies, stage, region), names_vary = "slowest")

piv_nfi<-nfi_meta_out%>%pivot_wider(id_cols=c(`Extended flight group`, sp), names_from=varib,
                                        values_from=c(mean, LCI, UCI, sd, n, n_studies, stage, region), names_vary = "slowest")

piv_height<-ht_res_out%>%pivot_wider(id_cols=c(`Extended flight group`, Common.name), names_from=varib,
                                        values_from=c(wt_ave, min, max, n_H, n_M, n_L, stage, region), names_vary = "slowest")

# and combine
piv_res<-full_join(piv_speed, piv_height, by=join_by(sp==Common.name, `Extended flight group`))
piv_res<-full_join(piv_res, piv_nfi, by=join_by(sp, `Extended flight group`))

#calc mean and sd per flight group and bind back in then re-order
flg_mn<-piv_res%>%group_by(`Extended flight group`)%>%summarise_all(.fun=function(x){if(is.numeric(x)){mean(x, na.rm = TRUE)}else{NA}})
flg_sd<-piv_res%>%group_by(`Extended flight group`)%>%summarise_all(.fun=function(x){if(is.numeric(x)){sd(x, na.rm = TRUE)}else{NA}})

flg_minmax<-piv_res%>%group_by(`Extended flight group`)%>%summarise(p1_mi1=min(min_percRSZ , na.rm = TRUE),p1_mi2=min(wt_ave_percRSZ , na.rm = TRUE), 
                                                                    p1_mx1=max(max_percRSZ , na.rm = TRUE),p1_mx2=max(wt_ave_percRSZ , na.rm = TRUE),
                                                                    p2_mi1=min(min_height , na.rm = TRUE),p2_mi2=min(wt_ave_height , na.rm = TRUE), 
                                                                    p2_mx1=max(max_height , na.rm = TRUE),p2_mx2=max(wt_ave_height , na.rm = TRUE))

flg_minmax$p1_min<-apply(cbind(flg_minmax$p1_mi1, flg_minmax$p1_mi2),1, min)
flg_minmax$p1_max<-apply(cbind(flg_minmax$p1_mx1, flg_minmax$p1_mx2),1, max)
flg_minmax$p2_min<-apply(cbind(flg_minmax$p2_mi1, flg_minmax$p2_mi2),1, min)
flg_minmax$p2_max<-apply(cbind(flg_minmax$p2_mx1, flg_minmax$p2_mx2),1, max)


flg_mn$min_percRSZ<-flg_minmax$p1_min
flg_mn$max_percRSZ<-flg_minmax$p1_max
flg_mn$min_height<-flg_minmax$p2_min
flg_mn$max_height<-flg_minmax$p2_max

flg_mn$sp<-"ZZ_mean"
flg_sd$sp<-"ZZ_sd"

# read in fg group means and SDs from meta analyses and replace 'summarise calc-ed' values
fg_metaz<-read_xlsx("outputs/fg_meta_means.xlsx")

fg_metaz<-fg_metaz%>%pivot_wider(id_cols=c(`Extended flight group`), names_from=varib,
                       values_from=c(mean, LCI, UCI, sd, n, n_studies, stage, region), names_vary = "slowest")%>%
  arrange(`Extended flight group`)

flg_mn$mean_speed<-fg_metaz$mean_speed
flg_mn$LCI_speed<-fg_metaz$LCI_speed
flg_mn$UCI_speed<-fg_metaz$UCI_speed
flg_sd$mean_speed<-fg_metaz$sd_speed

flg_mn$mean_trip<-fg_metaz$mean_trip
flg_mn$LCI_trip<-fg_metaz$LCI_trip
flg_mn$UCI_trip<-fg_metaz$UCI_trip
flg_sd$mean_trip<-fg_metaz$sd_trip

flg_mn$mean_max<-fg_metaz$mean_max
flg_mn$LCI_max<-fg_metaz$LCI_max
flg_mn$UCI_max<-fg_metaz$UCI_max
flg_sd$mean_max<-fg_metaz$sd_max

flg_mn$mean_nfi<-fg_metaz$mean_nfi
flg_mn$LCI_nfi<-fg_metaz$LCI_nfi
flg_mn$UCI_nfi<-fg_metaz$UCI_nfi
flg_sd$mean_nfi<-fg_metaz$sd_nfi
# end replacement bit 

piv_res<-rbind(piv_res, flg_mn, flg_sd)
piv_res<-piv_res%>%arrange(`Extended flight group`, sp)

# now concatenate columns into sensible number. Code breedstages/regions. Leave some tricky ones to do manually.
# make two tables, one near raw of piv_res + anecdotal/max Height for supp; one, trimmed piv_res for main table

# main table
tab1<-piv_res

#code regions
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "North Atlantic", replacement = "NAt"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "South Atlantic", replacement = "SAt"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Mediterranean", replacement = "Med"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Antarctic", replacement = "Ant"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Eastern Pacific", replacement = "EPa"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Western Pacific", replacement = "WPa"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "North Pacific", replacement = "NPa"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Unknown", replacement = "Unk"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "North Sea", replacement = "Nth"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Caribbean Sea and Gulf of Mexico", replacement = "Crb"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Indian", replacement = "Ind"))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Ocean", replacement = ""))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Oceans", replacement = ""))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Sea", replacement = ""))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "and", replacement = ","))
tab1<-tab1 %>% mutate(across(c(region_speed, region_trip, region_max, region_percRSZ, region_height, region_nfi), gsub, pattern = "Global", replacement = "All"))

# code stages
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Incubation", replacement = "Inc", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Unknown", replacement = "Unk", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Brooding", replacement = "BrG", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Brood-guard", replacement = "BrG", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Brood guard", replacement = "BrG", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "chick guard", replacement = "BrG", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "chick-brooding", replacement = "BrG", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Chick-rearing", replacement = "Chk", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Chick rearing", replacement = "Chk", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Post-guard", replacement = "Chk", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "migration and wintering (total non-breeding)", replacement = "Nbr", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Non-breeding", replacement = "NBr", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Entire breeding", replacement = "Brd", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "breeding season", replacement = "Brd", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "breeding", replacement = "Brd", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Migration", replacement = "Mig", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Wintering", replacement = "Wnt", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Winter", replacement = "Wnt", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Winter", replacement = "Wnt", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "All stages", replacement = "All", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "Annual cycle", replacement = "All", ignore.case=T))
tab1<-tab1 %>% mutate(across(c(stage_speed, stage_trip, stage_max, stage_percRSZ, stage_height, stage_nfi), gsub, pattern = "and", replacement = ",", ignore.case=T))

#format group mean and sd then concatenate
tab1[tab1$sp=='ZZ_mean',]$sd_speed<-tab1[tab1$sp=='ZZ_sd',]$mean_speed
tab1[tab1$sp=='ZZ_mean',]$sd_trip<-tab1[tab1$sp=='ZZ_sd',]$mean_trip
tab1[tab1$sp=='ZZ_mean',]$sd_max<-tab1[tab1$sp=='ZZ_sd',]$mean_max
tab1[tab1$sp=='ZZ_mean',]$sd_nfi<-tab1[tab1$sp=='ZZ_sd',]$mean_nfi

tab1$mean_speed<-paste0(round(tab1$mean_speed, 1), "±", round(tab1$sd_speed, 1))
tab1$mean_trip<-paste0(round(tab1$mean_trip, 1), "±", round(tab1$sd_trip, 1))
tab1$mean_max<-paste0(round(tab1$mean_max, 1), "±", round(tab1$sd_max, 1))
tab1$wt_ave_percRSZ<-paste0(round(tab1$wt_ave_percRSZ,1), "(", round(tab1$min_percRSZ, 1), "-", round(tab1$max_percRSZ, 1), ")")
tab1$wt_ave_height<-paste0(round(tab1$wt_ave_height,1), "(", round(tab1$min_height, 1), "-", round(tab1$max_height, 1), ")")
tab1$mean_nfi<-paste0(round(tab1$mean_nfi, 2), "±", round(tab1$sd_nfi, 2))

#tidy
tab1<-tab1 %>% mutate(across(everything(), gsub, pattern = "(NA-NA)", replacement = "", fixed=T))
tab1<-tab1 %>% mutate(across(everything(), gsub, pattern = "NA±NA", replacement = "", fixed=T))
tab1<-tab1 %>% mutate(across(everything(), gsub, pattern = "NaN±NA", replacement = "", fixed=T))
tab1<-tab1 %>% mutate(across(everything(), gsub, pattern = "NA±NaN", replacement = "", fixed=T))
tab1<-tab1 %>% mutate(across(everything(), gsub, pattern = "NaN±NaN", replacement = "", fixed=T))

#arrange by fight group
tab1$`Extended flight group`<-factor(tab1$`Extended flight group`, levels=
                                       c("Great albatrosses", "Sooty albatrosses", "Small albatrosses", "Giant petrels", "Fulmars", "Procellaria petrels", 
                                         "Large gadfly petrels", "Small gadfly petrels", "Calonectris shearwaters", "Surface feeding shearwaters", "Diving shearwaters", 
                                         "Manx type shearwaters", "Prions", "Diving petrels", "Oceanodroma", "Frigate petrels", "Oceanites"))

tab1<-tab1%>%arrange(`Extended flight group`, sp)

#clean up flight group summary rows
tab1<-tab1%>%filter(sp!="ZZ_sd")
tab1[tab1$sp=="ZZ_mean", which(!names(tab1)%in%c("mean_speed", "mean_trip", "mean_max", "wt_ave_percRSZ", "wt_ave_height", "mean_nfi", "sp", "Extended flight group"))]<-""
tab1[tab1$sp=="ZZ_mean",]$sp<-tab1[tab1$sp=="ZZ_mean",]$`Extended flight group`

#format H, M, L reporting
for(i in 1:nrow(tab1))
{ t1<-NULL
  t2<-NULL
  
  if(!is.na(tab1[i,]$n_H_height)& tab1[i,]$n_H_height>0){t1<-c(t1, paste0("H", tab1[i,]$n_H_height))}
  if(!is.na(tab1[i,]$n_M_height)& tab1[i,]$n_M_height>0){t1<-c(t1, paste0("M", tab1[i,]$n_M_height))}
  if(!is.na(tab1[i,]$n_L_height)& tab1[i,]$n_L_height>0){t1<-c(t1, paste0("L", tab1[i,]$n_L_height))}
  if(is.null(t1)){t1<-""}
  
  tab1[i,]$max_height<-paste(t1, collapse=",")
  
  if(!is.na(tab1[i,]$n_H_percRSZ)&tab1[i,]$n_H_percRSZ>0){t2<-c(t2, paste0("H", tab1[i,]$n_H_percRSZ))}
  if(!is.na(tab1[i,]$n_M_percRSZ)&tab1[i,]$n_M_percRSZ>0){t2<-c(t2, paste0("M", tab1[i,]$n_M_percRSZ))}
  if(!is.na(tab1[i,]$n_L_percRSZ)&tab1[i,]$n_L_percRSZ>0){t2<-c(t2, paste0("L", tab1[i,]$n_L_percRSZ))}
  if(is.null(t2)){t2<-""}
  
  tab1[i,]$max_percRSZ<-paste(t2, collapse=",")
  }

#format n birds, studies reporting

tab1$n_studies_speed<-paste0(tab1$n_studies_speed, "(",tab1$n_speed, ")" )
tab1$n_studies_trip<-paste0(tab1$n_studies_trip, "(",tab1$n_trip, ")" )
tab1$n_studies_max<-paste0(tab1$n_studies_max, "(",tab1$n_max, ")" )
tab1$n_studies_nfi<-paste0(tab1$n_studies_nfi, "(",tab1$n_nfi, ")" )
#final na removal
tab1<-tab1 %>% mutate(across(everything(), gsub, pattern = "NA(NA)", replacement = "", fixed=T))
tab1<-tab1 %>% mutate(across(everything(), gsub, pattern = "()", replacement = "", fixed=T))
tab1<-tab1 %>% mutate(across(everything(), gsub, pattern = "±NA", replacement = "", fixed=T))
tab1<-tab1 %>% mutate(across(everything(), gsub, pattern = "NaN(Inf--Inf)", replacement = "", fixed=T))

tab1<-tab1%>%select(!starts_with(c("LCI", "UCI", "sd", "n_sp", "n_tr", "n_ma" , "n_nf", "min_", "n_H", "n_M", "n_L")))

#write_xlsx(tab1, "outputs/main_table.xlsx")

#### ***  *** ####

#### ---- Plots  ---- ####

# Main Fig1

#read in results data and remake main table in earlier format
speed_meta_out<-read_xlsx("outputs/speed_results.xlsx")
ht_res_out<-read_xlsx("outputs/height_results.xlsx")
nfi_meta_out<-read_xlsx("outputs/NFI_results.xlsx")

nfi_ready<-read_xlsx("analyses/NFI_ready.xlsx")
height_ready<-read_xlsx("analyses/height_ready.xlsx")
speed_ready<-read_xlsx("analyses/speed_ready.xlsx")

# join each to extended flight group

flg<-read_xlsx("data/procellariiform_flight_groups.xlsx")
speed_meta_out<-left_join(speed_meta_out, flg[,c(2,8)], by=join_by("sp"==`Common name`))
nfi_meta_out<-left_join(nfi_meta_out, flg[,c(2,8)], by=join_by("sp"==`Common name`))
ht_res_out<-left_join(ht_res_out, flg[,c(2,8)], by=join_by("Common.name"==`Common name`))

speed_ready<-left_join(speed_ready, flg[,c(2,8)], by=join_by("sp"==`Common name`))
nfi_ready<-left_join(nfi_ready, flg[,c(2,8)], by=join_by("sp"==`Common name`))
height_ready<-left_join(height_ready, flg[,c(2,8)], by=join_by("Common.name"==`Common name`))

# pivot data

piv_speed<-speed_meta_out%>%pivot_wider(id_cols=c(`Extended flight group`, sp), names_from=varib,
                                        values_from=c(mean, LCI, UCI, sd, n, n_studies, stage, region), names_vary = "slowest")

piv_nfi<-nfi_meta_out%>%pivot_wider(id_cols=c(`Extended flight group`, sp), names_from=varib,
                                    values_from=c(mean, LCI, UCI, sd, n, n_studies, stage, region), names_vary = "slowest")

piv_height<-ht_res_out%>%pivot_wider(id_cols=c(`Extended flight group`, Common.name), names_from=varib,
                                     values_from=c(wt_ave, min, max, n_H, n_M, n_L, stage, region), names_vary = "slowest")

# and combine
piv_res<-full_join(piv_speed, piv_height, by=join_by(sp==Common.name, `Extended flight group`))
piv_res<-full_join(piv_res, piv_nfi, by=join_by(sp, `Extended flight group`))

# read in fg group means and SDs from meta analyses and replace 'summarise calc-ed' values
fg_metaz<-read_xlsx("outputs/fg_meta_means.xlsx")

flg_mn<-fg_metaz%>%pivot_wider(id_cols=c(`Extended flight group`), names_from=varib,
                                 values_from=c(mean, LCI, UCI, sd, n, n_studies, stage, region), names_vary = "slowest")%>%
  arrange(`Extended flight group`)

# original calc just for RSZ and flight height
flg_mn_rsz<-piv_res%>%group_by(`Extended flight group`)%>%summarise_all(.fun=function(x){if(is.numeric(x)){mean(x, na.rm = TRUE)}else{NA}})

# make plots for %RSZ, speed and NFI

# ** NFI **
#Species + genus averaged plot
#order by decreasing risk
flg_mn$`Extended flight group`<-factor(flg_mn$`Extended flight group`, levels=flg_mn[order(flg_mn$mean_nfi, decreasing =T),] $`Extended flight group`)
piv_nfi$`Extended flight group`<-factor(piv_nfi$`Extended flight group`, levels=levels( flg_mn$`Extended flight group`))
nfi_ready$`Extended flight group`<-factor(nfi_ready$`Extended flight group`, levels=levels( flg_mn$`Extended flight group`))

nfi_p<-ggplot()+
  geom_hline(yintercept=0, size=0.5)+
  geom_jitter(data=nfi_ready%>%filter(!is.na(mean)), aes(x=`Extended flight group`, y=mean ), height=0, width=0.15, alpha=0.2, size=2)+
  geom_jitter(data=piv_nfi%>%filter(!is.na(mean_nfi)), aes(x=`Extended flight group`, y=mean_nfi ), height=0, width=0.05, alpha=0.6, size=2, colour='blue')+
  geom_point(data=flg_mn%>%filter(!is.na(mean_nfi)), aes(x=`Extended flight group`, y=mean_nfi), size=4, colour='blue')+
  geom_point(data=flg_mn%>%filter(!is.na(mean_nfi)), aes(x=`Extended flight group`, y=mean_nfi), size=4, colour='black', shape=1)+
  scale_y_continuous(breaks=seq(-1, 1, 0.2), limits=c(-1, 1))+
  labs(y="Night Flight Index")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme_bw()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        axis.title.x = element_blank())

#Species plot for appendix
#order by decreasing risk

piv_nfi$sp<-factor(piv_nfi$sp, levels=piv_nfi[order(piv_nfi$mean_nfi, decreasing =T),]$sp)

ke_sp<-piv_nfi[piv_nfi$n_studies_nfi==1,]$sp[which(piv_nfi[piv_nfi$n_studies_nfi==1,]$sp%in%nfi_ready[nfi_ready$study=='Kelsey et al (2018)',]$sp)]
rw_sp<-piv_nfi[piv_nfi$n_studies_nfi==1,]$sp[which(piv_nfi[piv_nfi$n_studies_nfi==1,]$sp%in%nfi_ready[nfi_ready$study=='Robinson-Willmot et al (2013)',]$sp)]


nfi_p_appen<-ggplot()+
  geom_hline(yintercept=0, size=0.5)+
  geom_point(data=piv_nfi, aes(x=sp, y=mean_nfi, colour=factor(n_studies_nfi)))+
  geom_errorbar(data=piv_nfi%>%filter(n_studies_nfi>1), aes(x=sp, ymax=UCI_nfi, ymin=LCI_nfi), col='black', width=0.8)+
  geom_errorbar(data=piv_nfi%>%filter(!is.na(LCI_nfi) & n_studies_nfi==1), aes(x=sp, ymax=UCI_nfi, ymin=LCI_nfi), col='darkgrey', width=0.8)+
  geom_errorbar(data=piv_nfi%>%filter(is.na(LCI_nfi)), aes(x=sp, ymax=mean_nfi+sd_nfi, ymin=mean_nfi-sd_nfi), col='darkgrey', width=0.8)+
  
  geom_text(data=piv_nfi%>%filter(sp%in%ke_sp), aes(x=sp, y=mean_nfi+sd_nfi+0.1, label="C"), size=2.5,colour='darkgrey')+
  geom_text(data=piv_nfi%>%filter(sp%in%rw_sp), aes(x=sp, y=mean_nfi+sd_nfi+0.1, label="D"), size=2.5, colour='darkgrey')+
  
  geom_point(data=piv_nfi, aes(x=sp, y=mean_nfi, colour=factor(n_studies_nfi)))+
  coord_flip(ylim=c(-1, 1))+
  labs(y="Night Flight Index", colour='n studies')+
 theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"), 
        axis.title.y = element_blank(),
        legend.position=c(.15,.2),
                          legend.background = element_blank(),
                          legend.box.background = element_blank(),
                          legend.key = element_blank(),
                      legend.title = element_text(size = 8), 
                       legend.text = element_text(size = 8))

# ** SPEED **
#Species + genus averaged plot
#order by decreasing risk
flg_mn$`Extended flight group`<-factor(flg_mn$`Extended flight group`, levels=flg_mn[order(flg_mn$mean_speed, decreasing =T),] $`Extended flight group`)
piv_speed$`Extended flight group`<-factor(piv_speed$`Extended flight group`, levels=levels( flg_mn$`Extended flight group`))
speed_ready$`Extended flight group`<-factor(speed_ready$`Extended flight group`, levels=levels( flg_mn$`Extended flight group`))

speed_p<-ggplot()+
  geom_jitter(data=speed_ready%>%filter(varib=="trip"&!is.na(mean)), aes(x=`Extended flight group`, y=mean, colour='trip'), height=0, width=0.15, alpha=0.2, size=2)+
  geom_jitter(data=piv_speed%>%filter(!is.na(mean_trip)), aes(x=`Extended flight group`, y=mean_trip, colour='trip' ), height=0, width=0.05, alpha=0.6, size=2)+
  geom_jitter(data=speed_ready%>%filter(varib=="max"&!is.na(mean)), aes(x=`Extended flight group`, y=mean, colour='max' ), height=0, width=0.15, alpha=0.2, size=2)+
  geom_jitter(data=piv_speed%>%filter(!is.na(mean_max)), aes(x=`Extended flight group`, y=mean_max , colour='max' ), height=0, width=0.05, alpha=0.6, size=2)+
  geom_jitter(data=speed_ready%>%filter(varib=="speed"&!is.na(mean)), aes(x=`Extended flight group`, y=mean, colour='speed' ), height=0, width=0.15, alpha=0.2, size=2)+
  geom_jitter(data=piv_speed%>%filter(!is.na(mean_speed)), aes(x=`Extended flight group`, y=mean_speed , colour='speed'), height=0, width=0.05, alpha=0.6, size=2)+
  geom_point(data=flg_mn%>%filter(!is.na(mean_trip)), aes(x=`Extended flight group`, y=mean_trip, colour='trip'), size=4)+
  geom_point(data=flg_mn%>%filter(!is.na(mean_trip)), aes(x=`Extended flight group`, y=mean_trip), size=4, colour='black', shape=1)+
  geom_point(data=flg_mn%>%filter(!is.na(mean_max)), aes(x=`Extended flight group`, y=mean_max, colour='max' ), size=4)+
  geom_point(data=flg_mn%>%filter(!is.na(mean_max)), aes(x=`Extended flight group`, y=mean_max), size=4, colour='black', shape=1)+
  geom_point(data=flg_mn%>%filter(!is.na(mean_speed)), aes(x=`Extended flight group`, y=mean_speed, colour='speed'), size=4)+
  geom_point(data=flg_mn%>%filter(!is.na(mean_speed)), aes(x=`Extended flight group`, y=mean_speed), size=4, colour='black', shape=1)+
  labs(y="Speed (m/s)")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2), drop=F)+theme_bw()+
  scale_color_manual(name = "Group",
                     values = c( "speed" = "blue", "max" = "darkred", "trip" = "orange"),
                     labels = c( "Maximum speed","Flight speed", "Trip speed"))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        axis.title.x = element_blank(), legend.position=c(.9,.74),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank())

#Species plot for appendix --Speed
#order by decreasing risk
piv_speed$sp<-factor(piv_speed$sp, levels=piv_speed[order(piv_speed$mean_speed, decreasing =T),]$sp)
sp_ai_sp<-piv_speed[piv_speed$n_studies_speed==1,]$sp[which(piv_speed[piv_speed$n_studies_speed==1,]$sp%in%speed_ready[speed_ready$study=='Spear & Ainley (1997)',]$sp)]
al_sp<-piv_speed[piv_speed$n_studies_speed==1,]$sp[which(piv_speed[piv_speed$n_studies_speed==1,]$sp%in%speed_ready[speed_ready$study=='Alerstam et al (1993)',]$sp)]
pn_sp<-piv_speed[piv_speed$n_studies_speed==1,]$sp[which(piv_speed[piv_speed$n_studies_speed==1,]$sp%in%speed_ready[speed_ready$study=='Pennycuik (1982)',]$sp)]


speed_p_appen<-ggplot()+
  geom_point(data=piv_speed%>%filter(!is.na(mean_speed)), aes(x=sp, y=mean_speed , colour=factor(n_studies_speed)), size=2)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(mean_speed)& n_studies_speed>1), aes(x=sp, ymax=UCI_speed, ymin=LCI_speed), col='black', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(mean_speed)&!is.na(LCI_speed) & n_studies_speed==1), aes(x=sp, ymax=UCI_speed, ymin=LCI_speed), col='darkgrey', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(mean_speed)&is.na(LCI_speed)), aes(x=sp, ymax=mean_speed+sd_speed, ymin=mean_speed-sd_speed), col='darkgrey', width=0.8)+
  geom_point(data=piv_speed%>%filter(!is.na(mean_speed)), aes(x=sp, y=mean_speed, colour=factor(n_studies_speed)), size=2)+
  geom_text(data=piv_speed%>%filter(sp%in%sp_ai_sp), aes(x=sp, y=UCI_speed+1, label="B"), size=2.5,colour='darkgrey')+
  geom_text(data=piv_speed%>%filter(sp%in%al_sp), aes(x=sp, y=mean_speed+sd_speed+1, label="A"), size=2.5, colour='darkgrey')+
  geom_point(data=piv_speed%>%filter(sp%in%pn_sp), aes(x=sp, y=mean_speed+sd_speed+1), size=1, shape=10)+ # none
  labs(y="Flight speed (m/s)", colour='n studies')+
  theme_bw()+guides(colour=guide_legend(ncol=2))+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(),
        legend.position=c(.1,.15),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank())

#Species plot for appendix --Max
#order by decreasing risk
piv_speed$sp<-factor(piv_speed$sp, levels=piv_speed[order(piv_speed$mean_max, decreasing =T),]$sp)

speed_p_appen<-ggplot()+
  geom_point(data=piv_speed%>%filter(!is.na(mean_max)), aes(x=sp, y=mean_max , colour=factor(n_studies_max)), size=2)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(mean_max)& n_studies_max>1), aes(x=sp, ymax=UCI_max, ymin=LCI_max), col='black', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(mean_max)&!is.na(LCI_max) & n_studies_max==1), aes(x=sp, ymax=UCI_max, ymin=LCI_max), col='darkgrey', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(mean_max)&is.na(LCI_max)), aes(x=sp, ymax=mean_max+sd_max, ymin=mean_max-sd_max), col='darkgrey', width=0.8)+
  geom_point(data=piv_speed%>%filter(!is.na(mean_max)), aes(x=sp, y=mean_max, colour=factor(n_studies_max)), size=2)+
  labs(y="Maximum speed (m/s)", colour='n studies')+
  theme_bw()+guides(colour=guide_legend(ncol=1))+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(),
        legend.position=c(.9,.75),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank())

#Species plot for appendix --trip
#order by decreasing risk
piv_speed$sp<-factor(piv_speed$sp, levels=piv_speed[order(piv_speed$mean_trip, decreasing =T),]$sp)

speed_p_appen<-ggplot()+
  geom_point(data=piv_speed%>%filter(!is.na(mean_trip)), aes(x=sp, y=mean_trip , colour=factor(n_studies_trip)), size=2)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(mean_trip)& n_studies_trip>1), aes(x=sp, ymax=UCI_trip, ymin=LCI_trip), col='black', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(mean_trip)&!is.na(LCI_trip) & n_studies_trip==1), aes(x=sp, ymax=UCI_trip, ymin=LCI_trip), col='darkgrey', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(mean_trip)& is.na(LCI_trip)), aes(x=sp, ymax=mean_trip+sd_trip, ymin=mean_trip-sd_trip), col='darkgrey', width=0.8)+
  geom_point(data=piv_speed%>%filter(!is.na(mean_trip)), aes(x=sp, y=mean_trip, colour=factor(n_studies_trip)), size=2)+
  labs(y="Whole trip speed (m/s)", colour='n studies')+
  theme_bw()+guides(colour=guide_legend(ncol=1))+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(),
        legend.position=c(.9,.75),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank())

#Species plot for appendix --ALL
#order by decreasing risk

piv_speed$sp<-factor(piv_speed$sp, levels=piv_speed[order(piv_speed$mean_speed, decreasing =T),]$sp)
sp_ai_sp<-piv_speed[piv_speed$n_studies_speed==1,]$sp[which(piv_speed[piv_speed$n_studies_speed==1,]$sp%in%speed_ready[speed_ready$study=='Spear & Ainley (1997)',]$sp)]
al_sp<-piv_speed[piv_speed$n_studies_speed==1,]$sp[which(piv_speed[piv_speed$n_studies_speed==1,]$sp%in%speed_ready[speed_ready$study=='Alerstam et al (1993)',]$sp)]
pn_sp<-piv_speed[piv_speed$n_studies_speed==1,]$sp[which(piv_speed[piv_speed$n_studies_speed==1,]$sp%in%speed_ready[speed_ready$study=='Pennycuik (1982)',]$sp)]

speed_p_appen<-ggplot()+
 
  geom_point(data=piv_speed, aes(x=sp, y=mean_trip, colour='trip' ), alpha=0.6, size=2)+
  geom_errorbar(data=piv_speed%>%filter(n_studies_trip>1), aes(x=sp, ymax=UCI_trip, ymin=LCI_trip), col='black', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(LCI_trip) & n_studies_trip==1), aes(x=sp, ymax=UCI_trip, ymin=LCI_trip), col='darkgrey', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(is.na(LCI_trip)), aes(x=sp, ymax=mean_trip+sd_trip, ymin=mean_trip-sd_trip), col='darkgrey', width=0.8)+
 
  geom_point(data=piv_speed, aes(x=sp, y=mean_max , colour='max' ), alpha=0.6, size=2)+
  geom_errorbar(data=piv_speed%>%filter(n_studies_max>1), aes(x=sp, ymax=UCI_max, ymin=LCI_max), col='black', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(LCI_max) & n_studies_max==1), aes(x=sp, ymax=UCI_max, ymin=LCI_max), col='darkgrey', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(is.na(LCI_max)), aes(x=sp, ymax=mean_max+sd_max, ymin=mean_max-sd_max), col='darkgrey', width=0.8)+

  geom_point(data=piv_speed, aes(x=sp, y=mean_speed , colour='speed'), alpha=0.6, size=2)+
  geom_errorbar(data=piv_speed%>%filter(n_studies_speed>1), aes(x=sp, ymax=UCI_speed, ymin=LCI_speed), col='black', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(!is.na(LCI_speed) & n_studies_speed==1), aes(x=sp, ymax=UCI_speed, ymin=LCI_speed), col='darkgrey', width=0.8)+
  geom_errorbar(data=piv_speed%>%filter(is.na(LCI_speed)), aes(x=sp, ymax=mean_speed+sd_speed, ymin=mean_speed-sd_speed), col='darkgrey', width=0.8)+
  
  geom_text(data=piv_speed%>%filter(sp%in%sp_ai_sp), aes(x=sp, y=UCI_speed+1, label="B"), size=2.5,colour='darkgrey')+
  geom_text(data=piv_speed%>%filter(sp%in%al_sp), aes(x=sp, y=mean_speed+sd_speed+1, label="A"), size=2.5, colour='darkgrey')+
  
  geom_point(data=piv_speed, aes(x=sp, y=mean_trip, colour='trip' ), alpha=0.6, size=2)+
  geom_point(data=piv_speed, aes(x=sp, y=mean_max , colour='max' ), alpha=0.6, size=2)+
  geom_point(data=piv_speed, aes(x=sp, y=mean_speed , colour='speed'), alpha=0.6, size=2)+
  coord_flip(ylim=c(0, 30))+
  labs(y="Speed (m/s)")+theme_bw()+
  scale_color_manual(name = "Group",
                     values = c( "speed" = "blue", "max" = "darkred", "trip" = "orange"),
                     labels = c( "Max speed","Flight speed", "Trip speed"))+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"), 
        axis.title.y = element_blank(),
        legend.position=c(.78,.68),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8))

# ** % RSZ **
#Species + genus averaged plot
#order by decreasing risk
flg_mn_rsz$`Extended flight group`<-factor(flg_mn_rsz$`Extended flight group`, levels=flg_mn_rsz[order(flg_mn_rsz$wt_ave_percRSZ, decreasing =T),] $`Extended flight group`)
piv_height$`Extended flight group`<-factor(piv_height$`Extended flight group`, levels=levels( flg_mn_rsz$`Extended flight group`))
height_ready$`Extended flight group`<-factor(height_ready$`Extended flight group`, levels=levels( flg_mn_rsz$`Extended flight group`))
height_ready$X3<-factor(height_ready$X3, levels=c('H', 'M', 'L'))

rsz_p<-ggplot()+
  geom_jitter(data=height_ready%>%filter(varib=="percRSZ"& !is.na(X1)), aes(x=`Extended flight group`, y=as.numeric(X1), colour=X2 ), height=0, width=0.2, alpha=0.4, size=2)+
  geom_point(data=flg_mn_rsz%>%filter(!is.na(wt_ave_percRSZ)), aes(x=`Extended flight group`, y=wt_ave_percRSZ), size=4, colour='black', shape=1)+
  labs(y="Time in Rotor Swept Zone (%)")+
  scale_color_manual(name = "Study accuracy",
                     breaks = c( 'H', 'M', 'L') ,
                     values = c( "H" = "blue", "M" = "orange", "L" = "darkred"),
                     labels = c( "High", "Medium", "Low"))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme_bw()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        axis.title.x = element_blank(), legend.position=c(.9,0.74),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank())

#Species plot for appendix --RSZ
rsz_sp<-height_ready%>%filter(varib=="percRSZ"& !is.na(X1))
sp_mn<-rsz_sp%>%gr(Common.name)%>%summarise(mn_x1=mean(as.numeric(X1)))
rsz_sp$Common.name<-factor(rsz_sp$Common.name, levels=sp_mn[order(sp_mn$mn_x1, decreasing=T),]$Common.name)

ggplot()+
  geom_point(data=rsz_sp, aes(x=Common.name, y=as.numeric(X1), colour=X2), alpha=0.6)+
  labs(y="Time in Rotor Swept Zone (%)")+
  scale_color_manual(name = "Study accuracy",
                     breaks = c( 'H', 'M', 'L') ,
                     values = c( "H" = "blue", "M" = "orange", "L" = "darkred"),
                     labels = c( "High", "Medium", "Low"))+
  theme_bw()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(), legend.position=c(.9,0.74),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank())


#make Main fig 1

library(patchwork)
speed_p/nfi_p/rsz_p

speed_p_appen|nfi_p_appen

# make Main fig 2 - flight height

piv_height<-piv_height%>%filter(!is.na(wt_ave_height))
height_ready<-height_ready%>%filter(!is.na(wt_ave_height))
piv_height$Common.name<-factor(piv_height$Common.name, levels=piv_height[order(piv_height$wt_ave_height, decreasing =T),]$Common.name)
height_ready$Common.name<-factor(height_ready$Common.name, levels=levels( piv_height$Common.name))
height_ready$X3<-factor(height_ready$X3, levels=c('H', 'M', 'L'))

ggplot()+
  geom_point(data=height_ready%>%filter(varib=="height"& !is.na(X1)), aes(x=Common.name, y=as.numeric(X1), 
            colour=paste0(study, " (", X2, ")") ), size=2)+
  geom_point(data=piv_height%>%filter(!is.na(wt_ave_height)), aes(x=Common.name, y=wt_ave_height), size=4, colour='black', shape=1)+
  labs(y="Mean flight height (m)", colour='Study (quality)')+
  theme_bw()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(), legend.position=c(.87,0.68),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank())

# Main Fig 3
library(scatterpie)

# original calc just for RSZ, calc mean and min max per FG
flg_mn_rsz<-piv_res%>%group_by(`Extended flight group`)%>%summarise_all(.fun=function(x){if(is.numeric(x)){mean(x, na.rm = TRUE)}else{NA}})

flg_minmax<-piv_res%>%group_by(`Extended flight group`)%>%summarise(p1_mi1=min(min_percRSZ , na.rm = TRUE),p1_mi2=min(wt_ave_percRSZ , na.rm = TRUE), 
                                                                    p1_mx1=max(max_percRSZ , na.rm = TRUE),p1_mx2=max(wt_ave_percRSZ , na.rm = TRUE))

flg_minmax$p1_min<-apply(cbind(flg_minmax$p1_mi1, flg_minmax$p1_mi2),1, min)
flg_minmax$p1_max<-apply(cbind(flg_minmax$p1_mx1, flg_minmax$p1_mx2),1, max)

flg_mn_rsz$min_percRSZ<-flg_minmax$p1_min
flg_mn_rsz$max_percRSZ<-flg_minmax$p1_max

# join rsz results to flg_mn
flg_mn<-left_join(flg_mn, flg_mn_rsz%>%select(`Extended flight group`, wt_ave_percRSZ,
                                              min_percRSZ, max_percRSZ), by=join_by(`Extended flight group`))

# Make counts of species within FG per NFI class
piv_nfi$nfi_class<-cut(piv_nfi$mean_nfi, c(-1,-0.1, 0.1, 1))
nfi_piedat<-piv_nfi%>%select(1,3,11)%>%pivot_wider(id_cols=`Extended flight group`,
                                                  names_from=nfi_class, values_from=mean_nfi, values_fn=length, values_fill = 0)
# join pie dat and seperate flg rsz dataframen(flg_mn, nfi_piedat, by=join_by(`Extended flight group`))
flg_mn<-left_join(flg_mn, nfi_piedat, by=join_by(`Extended flight group`))

#geom_errorbar(aes(ymin=min_percRSZ, ymax=max_percRSZ, x=mean_speed), alpha=0.5)+
#  geom_errorbarh(aes(xmin=mean_speed-sd_speed, xmax=mean_speed+sd_speed, y=wt_ave_percRSZ), alpha=0.5)+

ggplot(data=flg_mn)+
  
  geom_scatterpie(aes(x=mean_speed, y=wt_ave_percRSZ), data=flg_mn,
                  cols= c("(-1,-0.1]", "(-0.1,0.1]","(0.1,1]"),colour=NA, pie_scale = 3) +
  geom_point(data=flg_mn[flg_mn$`Extended flight group`=='Frigate petrels',],
                           aes(x=mean_speed, y=wt_ave_percRSZ), shape=1, size=7) +
  coord_equal()+
  scale_fill_manual(breaks = c("(-1,-0.1]", "(-0.1,0.1]","(0.1,1]") ,
                     values = c( "(-1,-0.1]" = "yellow", "(-0.1,0.1]" = "lightgrey", "(0.1,1]" = "black"),
                     labels = c( "Diurnal", "Both", "Nocturnal"))+
  geom_text_repel(aes(x=mean_speed, y=wt_ave_percRSZ, label= `Extended flight group`),
                  size=3, nudge_y=1, nudge_x=0.25)+theme_bw()+
  labs(y="Mean time in Rotor Swept Zone (%)", x="Mean flight speed (m/s)",
       fill="NFI class")+
  theme(        legend.position = c(0.3, 0.92), 
                legend.text = element_text(size=10),
                legend.background = element_blank(),
                legend.box.background = element_blank(),
                legend.key = element_blank())

#### ---- Plots End  ---- ####


# Stats and RSZ turbine height appendix fig 
# Species plot to present reasoning for pooling RSZ heights: basically lots of error. Could test with stats if needed
# runs from above code to make main fig 1

# test to see if %time in RSZ increases with lower minimum tip height
library(lme4)
library(performance)
library(ggResidpanel)
library(lmerTest)

#first models excluding small studies < 3 species
m1<-lmer(as.numeric(X1)~as.numeric(X3)+(1|study)+(1|`Extended flight group`), data=height_ready%>%filter(study%in%c(names(which(table(height_ready$study)>2)))))
check_model(m1)


m2<-glmer(cbind(ceiling(as.numeric(X1)), 100-ceiling(as.numeric(X1)))~as.numeric(X3)+(1|study)+(1|`Extended flight group`),
          data=height_ready%>%filter(study%in%c(names(which(table(height_ready$study)>2)))), family='binomial')

#refit with full dataset (all studies)

m3<-lmer(as.numeric(X1)~as.numeric(X3)+(1|study)+(1|`Extended flight group`), data=height_ready)


m4<-glmer(cbind(ceiling(as.numeric(X1)), 100-ceiling(as.numeric(X1)))~as.numeric(X3)+(1|study)+(1|`Extended flight group`),
          data=height_ready, family='binomial')


resid_compare(list(m1, m3, m2,m4))

library(lmerTest)
anova(m1)
anova(m3) # use this
#Type III Analysis of Variance Table with Satterthwaite's method
#               Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
#as.numeric(X3) 11.761  11.761     1 6.6688   0.253 0.6312

library(car)
Anova(m2)
Anova(m4)

# none significant


# make sup fig plot
height_ready$Common.name<-factor(height_ready$Common.name, levels=unique(height_ready[order(height_ready$`Extended flight group`),] $Common.name))
height_ready$X2<-factor(height_ready$X2, levels=c("L", "M", "H"))

# one STSH outlier at 50 removed
ggplot()+
  geom_point(data=height_ready%>%filter(varib=="percRSZ" & X1<26 & !is.na(X1)), aes(x=Common.name, y=as.numeric(X1), colour=X3, size=X2), shape=1)+
  labs(y="Percent time in Rotor Swept Zone")+theme_bw()+
  scale_y_continuous(breaks=seq(0, 25, 2))+
  scale_x_discrete(drop=F)+
  scale_size_discrete(name = "Study\nconfidence")+
  scale_colour_discrete(name = "Minimum height\nof RSZ (m)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# test flight speeds differ between trip, speed and max
speed_meta_out<-read_xlsx("outputs/speed_results.xlsx")

m1<-lmer(mean~varib+(1|sp),data=speed_meta_out)
check_model(m1)
anova(m1)
#Type III Analysis of Variance Table with Satterthwaite's method
#      Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
#varib 1807.3  903.66     2  95.5  102.29 < 2.2e-16 ***

library(emmeans)
emmeans(m1, "varib")
emmeans(m1, "varib")%>%pairs()

# test whether survey data produces faster trips than biologger
speed1<-read_xlsx("analyses/speed_ready.xlsx")
speed1$data.group<-"Vessel-based"

speed1[speed1$data.type%in% c("GPS" ,   "PTT",    "GPS-PTT"  ,   "GPS and PTT" ,                   
                                  "PPT"  ,  "GPS & PTT" ,     "Barometric pressure sensor" ,"Geolocator" , "GPS/Geolocator",                   
                                  "Wet-dry logger"  ,   "Wet-dry loggers" ,  "Activity loggers",   "GLS" ,                           
                                  "Immersion loggers"  ,    "Temperature logger" ,"TDR" ),]$data.group<-"Bio-logger"

speed1[speed1$data.type=="Literature review",]$data.group<-"Literature review"

speed1[speed1$data.type%in% c("Land based ornithodolite"  , "land based radar"  ,"Infrared binoculars and markers",
                                  "Aerial photogrammetry"   ,  "Ornithodolite from headland" ),]$data.group<-"Aerial/land-based"
speed1<-filter(speed1, varib=='speed')
speed1<-filter(speed1, data.group!='Aerial/land-based')#remove few

m1<-lmer(mean~data.group+(1|sp)+(1|study),data=speed1)
check_model(m1)
anova(m1)
#Type III Analysis of Variance Table with Satterthwaite's method
#Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)   
#data.group 72.737  72.737     1 14.01  11.774 0.004048 **

library(emmeans)
emmeans(m1, "data.group")
emmeans(m1, "data.group")%>%pairs()


# TEMP

## Anecdotal ##

# function 2 splitting data
meta_split_2<-function(x)
{
  t1<-unlist(str_split(x, "@"))
  t2<-do.call("rbind", (split(t1, ceiling(1:length(t1)/2))))%>%as.data.frame()
  return(t2)
}


lit_nf<-dat_NAF%>%dplyr::select(c(1:6, starts_with('Anec')))
names(lit_nf)<-lit_nf[1,];lit_nf<-lit_nf[-1,]

long_nlit<-NULL
for(i in 1:nrow(lit_nf))
{
  fl_speed_temp<-lit_nf[,7:ncol(lit_nf)]
  sel_sp<-fl_speed_temp[i,which(!is.na(fl_speed_temp[i,]))]
  if(length(sel_sp)==0){next} # skip sp with no data
  temp<-do.call("rbind", apply(sel_sp, 2, FUN=meta_split_2))
  temp<-data.frame(sp=lit_nf[i,]$`Common name`,
                   study=paste0(unlist(str_split(row.names(temp), "\\)"))[seq(1, ((nrow(temp)*2)-1), 2)], ")"), temp)
  long_nlit<-rbind(long_nlit, temp)
}

long_nlit<-left_join(long_nlit, NAF_meta, by=c("study"="ref"))

write_xlsx(long_nlit, "analyses/temp_anecdotalnaf.xlsx")

lit_nf<-dat_FSD%>%dplyr::select(c(1:6, starts_with('Anec')))
names(lit_nf)<-lit_nf[1,];lit_nf<-lit_nf[-1,]

long_nlit<-NULL
for(i in 1:nrow(lit_nf))
{
  fl_speed_temp<-lit_nf[,7:ncol(lit_nf)]
  sel_sp<-fl_speed_temp[i,which(!is.na(fl_speed_temp[i,]))]
  if(length(sel_sp)==0){next} # skip sp with no data
  temp<-do.call("rbind", apply(sel_sp, 2, FUN=meta_split_2))
  temp<-data.frame(sp=lit_nf[i,]$`Common name`,
                   study=paste0(unlist(str_split(row.names(temp), "\\)"))[seq(1, ((nrow(temp)*2)-1), 2)], ")"), temp)
  long_nlit<-rbind(long_nlit, temp)
}

long_nlit<-left_join(long_nlit, speed_meta, by=c("study"="ref"))

write_xlsx(long_nlit, "analyses/temp_anecdotalspeed.xlsx")

# trial 3D plot, hmm hard to see whats going on

library(plot3D)

# greyish background for the boxtype (bty = "g") 
scatter3D(x=flg_mn$mean_speed, y=flg_mn$mean_nfi, z=flg_mn$wt_ave_percRSZ,
           bty = "g",phi=40, theta=35,
          pch = 20, cex = 2, ticktype = "detailed")

# add text
text3D(x=flg_mn$mean_speed, y=flg_mn$mean_nfi, z=flg_mn$wt_ave_percRSZ,
       colkey = FALSE, add = TRUE, 
       labels = flg_mn$`Extended flight group`, col = c("black"))

