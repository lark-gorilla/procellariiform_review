library(ggplot2)
library(readxl)
library(dplyr)

# Use Ainley et al. (2015) GLMM coefficients to work out probabililty of birds flying above 10m  


#Fixed effect coefs
Intercept=                    -1.5120    
Wind.speed=                   0.1794    
Headwind =                    -0.7670    
Tailwind=                      -0.3481 
California.Current=           -1.6391 
ETP =                         -3.2725 
Peru.Current  =               -3.1522  
Wind.speed_INT_Headwind =    -0.1049     
Wind.speed_INT_Tailwind =    -0.0099

# Random effect coefs
rand_effz<-read_xlsx("C:/Users/mmil0049/Documents/projects/01 southern seabird OWF review/analyses/Spear_GLMM/Ainley et al (2015) Seabird flight behaviour and height in response to wind strength GLMM coef.xlsx", sheet=2)

rand_effz_table<-expand.grid(Group=rand_effz$Group, wind=1:35)

rand_effz_table<-left_join(rand_effz_table, rand_effz, by="Group")%>%arrange(Group)

ave_intercept<-mean(c(Intercept,
       (Intercept+Headwind),
       (Intercept+Tailwind),
       (Intercept+California.Current),
       (Intercept+California.Current+Headwind),
       (Intercept+California.Current+Tailwind),
       (Intercept+ETP),
       (Intercept+ETP+Headwind),
       (Intercept+ETP+Tailwind),
       (Intercept+Peru.Current),
       (Intercept+Peru.Current+Headwind),
       (Intercept+Peru.Current+Tailwind)))

   
rand_effz_table$dev_from_av<-ave_intercept+rand_effz_table$Intercept+
  ((Wind.speed+Wind.speed_INT_Headwind+rand_effz_table$Slope)*rand_effz_table$wind) # Fig 5 assumes in Headwind HEADWIND!

ggplot(data=rand_effz_table)+ geom_line(aes(x=wind, y=dev_from_av, colour=Group))+
  scale_y_continuous(breaks=seq(-10, 8))

ggplot(data=rand_effz_table%>%filter(Group%in%c(
   "Diving shearwaters","Giant petrels","Frigate petrels" ,  "Fulmars"  ,   "Large gadfly petrels" ,      
    "Manx-type shearwaters" ,   "Oceanites"   ,   "Oceanodroma", "Prions"  ,  "Small albatrosses"  ,
   "Small gadfly petrels","Surface-feeding shearwaters" )))+ geom_line(aes(x=wind, y=dev_from_av, colour=Group))+
  scale_y_continuous(breaks=seq(-10, 8))
# think the above plot is pretty much Fig 5

# now create predictions for database
rand_effz_table

# previously using more intuitive region intercept based on average region intercepts only from where birds were observed but results
# were too impacted by region so just taking mean over all regions.
rand_effz_table$region_intercept<-mean(c(Intercept,(Intercept+California.Current),(Intercept+ETP),(Intercept+Peru.Current)))
                                      
rand_effz_final1<-rand_effz_table
rand_effz_final2<-rand_effz_table
rand_effz_final3<-rand_effz_table

rand_effz_final1$pred<-plogis(rand_effz_final1$region_intercept+rand_effz_final1$Intercept+
                               ((Wind.speed+rand_effz_final1$Slope)*rand_effz_final1$wind))
rand_effz_final1$wind_dir="Crosswind"

rand_effz_final2$pred<-plogis(rand_effz_final2$region_intercept+Headwind+rand_effz_final2$Intercept+
                                ((Wind.speed+Wind.speed_INT_Headwind+rand_effz_final2$Slope)*rand_effz_final2$wind))
rand_effz_final2$wind_dir="Headwind"

rand_effz_final3$pred<-plogis(rand_effz_final3$region_intercept+Tailwind+rand_effz_final3$Intercept+
                                ((Wind.speed+Wind.speed_INT_Tailwind+rand_effz_final3$Slope)*rand_effz_final3$wind))
rand_effz_final3$wind_dir="Tailwind"

rand_effz_final<-rbind(rand_effz_final1, rand_effz_final2, rand_effz_final3)

ggplot(data=rand_effz_final%>%filter(wind_dir=="Crosswind"))+ geom_line(aes(x=wind, y=pred, colour=Group))

ggplot(data=rand_effz_final)+ geom_line(aes(x=wind, y=pred, colour=wind_dir), size=1)+facet_wrap(~Group)+
geom_hline(aes(yintercept =0.5), linetype=3)+xlab("Wind speed m/s")+ylab("Probability of flying above 10m")+theme_bw()

# extract results

out_dat<-rand_effz_final%>%group_by(Group)%>%summarise(mean_pred=mean(pred), sd_pred=sd(pred))

library(writexl)
  
write_xlsx(out_dat, "C:/Users/mmil0049/OneDrive - Monash University/projects/01 southern seabird OWF review/analyses/Spear_GLMM/probs_mean0-30_alt.xlsx")  


#UNDERSTANDING SHIT
library(lme4)

sanity<-read_xlsx("C:/Users/mmil0049/Documents/projects/01 southern seabird OWF review/analyses/Spear_GLMM/Ainley et al (2015) Seabird flight behaviour and height in response to wind strength GLMM coef.xlsx", sheet=3)

mod<-glmer(above10~wind*wind_dir+(wind|Group), family="binomial", data=sanity)

sanity$pred_fixed<-predict(mod, sanity, re.form=NA, type="response")
sanity$pred<-fitted(mod)

fixef(mod)
ranef(mod)

head(sanity)

# Headwind
plogis(-20.161413 + (1.053571*15)) # fixed
plogis(-20.161413+ 9.553187 + ((1.053571-0.3951276)*15)) #gull

#Tailwind
head(sanity, 20)
plogis(-20.161413-227.045982 + ((1.053571+23.548432)*10)) #fixed
plogis(-20.161413-227.045982+9.553187 + ((1.053571+23.548432-0.3951276)*10))#gull

plogis(rand_effz_final3$region_intercept+Tailwind+rand_effz_final3$Intercept+
         ((Wind.speed+Wind.speed_INT_Tailwind+rand_effz_final3$Slope)*rand_effz_final3$wind))
       
       