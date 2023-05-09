xlib = c("dplyr","stringr","reshape2","tidyr","here","ggplot2",
         "data.table","lubridate","jsonlite","R.utils")
lapply(xlib, require, character.only=T) ; rm(xlib)

#########################################################################################
########################## place scenario data here #####################################
#########################################################################################



######################## Read and set boa directory ##############################################
# args <- commandArgs(trailingOnly=TRUE)
# trial_dir <- args[1] #boa output directory

trial_dir <-c("C:\\Users\\kujawa.21\\source\\repos\\SWAT_BOA_0215\\TxtInOut - Final params") #for testing
####################### copy model into boa directory ###########################################
sub_path <- file.path(trial_dir, "TxtInOut")

dir.create(sub_path)

copyDirectory(here::here("TxtInOut"), sub_path, recursive=TRUE)


######################### read in BOA params - json #############################################
Params <- read_json(paste0(trial_dir,"/parameters.json"))

# write new params based on outputs from boa
p_stl_res1<-Params[[1]] #Can use range 5-20 to start
p_stl_res2<-Params[[2]]
p_stl_res3<-Params[[3]]

#n_stl_res1<-5.5
#n_stl_res2<-5.5
#n_stl_res3<-5.5

sed_stl_res1<-Params[[4]] #Can use range 10-50
sed_stl_res2<-Params[[5]]
sed_stl_res3<-Params[[6]]

############### READ IN NUTRIENT PARAMETER FILE ##########################
nutFilePath<-paste0(trial_dir,"/TxtInOut",'/nutrients.res')
tmp <- file(nutFilePath)
open(tmp, "r") #read

#read past headerline and save to rewrite the file
topOfFile<-readLines(tmp, n = 2) 

#read file 
data1<-readLines(tmp, n = -1)


close(tmp)
headers<-c("name", "mid_start", "mid_end",  "mid_n_stl", "n_stl", "mid_p_stl",  "p_stl",  "chla_co",
           "secchi_co", "theta_n",  "theta_p", "n_min_stl", "p_min_stl")

#read by spacing 
DF<-strsplit(data1,split=" ")
DF<-lapply(DF, function(z){ z[z != ""]}) 
DF<-data.frame(do.call(rbind, DF),stringsAsFactors = FALSE) #unlist
colnames(DF)<-headers



################# CHANGE RES PARAMS ######################################
#this is crashing in boa, maybe try just using row and col index 2/20
#DF$p_stl[DF$name == "nutres1"]     <- sprintf(p_stl_res1, fmt = '%#.5f')
#DF$p_stl[DF$name == "nutres2"]     <- sprintf(p_stl_res2, fmt = '%#.5f')
#DF$p_stl[DF$name == "nutres3"]     <- sprintf(p_stl_res3, fmt = '%#.5f')

DF[1,7]    <- sprintf(p_stl_res1, fmt = '%#.5f')
DF[2,7]     <- sprintf(p_stl_res2, fmt = '%#.5f')
DF[3,7]     <- sprintf(p_stl_res3, fmt = '%#.5f')

# DF$n_stl[DF$name == "nutres1"]     <-sprintf(n_stl_res1, fmt = '%#.5f')
# DF$n_stl[DF$name == "nutres2"]     <-sprintf(n_stl_res2, fmt = '%#.5f')
# DF$n_stl[DF$name == "nutres3"]     <-sprintf(n_stl_res3, fmt = '%#.5f')


################ REWRITE NEW RES PARAMS #################################


# convert table to characters and strip of whitespace
DF[] <- lapply(DF, as.character)
DF <- lapply(DF, str_trim) # keep the first column with the correct spacing


spaceOutput<-function(data,nspaces){
  
  newData<-paste0(str_dup(" ",(nspaces-nchar(data))),data)
  return(newData)
  
}

spaceOutput_spacesecond<-function(data,nspaces){
  
  newData<-paste0(data,str_dup(" ",(nspaces-nchar(data))))
  return(newData)
  
}



#space the first two columns appropriately
DF[[1]]<-spaceOutput(DF[[1]],7) 
DF[[2]]<-spaceOutput(DF[[2]],19)
DF[[3]]<-spaceOutput(DF[[3]],10)

#all other columns have 14 spaces
for (i in c(4:length(DF))){
  
  DF[[i]] <- spaceOutput(DF[[i]],14)
  
  
}






file.remove(nutFilePath)

sink(nutFilePath, type=c("output"), append = T)

write(c(topOfFile),nutFilePath,sep = "\n",append=T)

write(c(paste0(DF[[1]],DF[[2]],DF[[3]],DF[[4]],DF[[5]],DF[[6]],DF[[7]],DF[[8]],DF[[9]],DF[[10]],
               DF[[11]],DF[[12]],DF[[13]])),nutFilePath,sep="\n",append=T)


sink()

############### READ IN SEDIMENT PARAMETER FILE ##########################
sedFilePath<-paste0(trial_dir,"/TxtInOut",'/sediment.res')
tmp <- file(sedFilePath)
open(tmp, "r") #read

#read past headerline and save to rewrite the file
topOfFile<-readLines(tmp, n = 2) 

#read file 
data1<-readLines(tmp, n = -1)


close(tmp)
headers<-c("name", "sed_amt",   "d50",  "carbon", "bd", "sed_stl", "stl_vel")

#read by spacing 
DF<-strsplit(data1,split=" ")
DF<-lapply(DF, function(z){ z[z != ""]}) 
DF<-data.frame(do.call(rbind, DF),stringsAsFactors = FALSE) #unlist
colnames(DF)<-headers



################# CHANGE RES PARAMS ######################################

#DF$stl_vel[DF$name == "sedres1"]     <- sprintf(sed_stl_res1, fmt = '%#.5f') #indexing like this is crashing boa, try row col
#DF$stl_vel[DF$name == "sedres2"]     <- sprintf(sed_stl_res2, fmt = '%#.5f')
#DF$stl_vel[DF$name == "sedres3"]     <- sprintf(sed_stl_res3, fmt = '%#.5f')

DF[1,7]     <- sprintf(sed_stl_res1, fmt = '%#.5f')
DF[2,7]     <- sprintf(sed_stl_res2, fmt = '%#.5f')
DF[3,7]     <- sprintf(sed_stl_res3, fmt = '%#.5f')

################ REWRITE NEW RES PARAMS #################################


# convert table to characters and strip of whitespace
DF[] <- lapply(DF, as.character)
DF <- lapply(DF, str_trim) # keep the first column with the correct spacing





#space the first two columns appropriately
DF[[1]]<-spaceOutput(DF[[1]],7) 
DF[[2]]<-spaceOutput(DF[[2]],23) 

#all other columns have 14 spaces
for (i in c(3:length(DF))){
  
  DF[[i]] <- spaceOutput(DF[[i]],14)
  
  
}


file.remove(sedFilePath)

sink(sedFilePath, type=c("output"), append = T)

write(c(topOfFile),sedFilePath,sep = "\n",append=T)

write(c(paste0(DF[[1]],DF[[2]],DF[[3]],DF[[4]],DF[[5]],DF[[6]],DF[[7]])),sedFilePath,sep="\n",append=T)


sink()



######################## Run SWAT ######################################################
# run SWAT
TxtInOutPath<-paste0(trial_dir,"/TxtInOut")
setwd(TxtInOutPath)

x<-system('SWATPlus_60.5.5.exe',ignore.stdout = F, ignore.stderr = F) #run SWAT

setwd('..')
#if SWAT crashed tell BOA, otherwise continue on to read SWAT outputs  
if (x == 157 | x == 72 | x == 38) {
  print("\n\n\n\n\n\n")
  print(x)

  trial_status <- "FAILED"
  
  out_data <- list(
    TrialStatus=unbox(trial_status),
    error=x
  )
  json_data <- toJSON(out_data, pretty = TRUE)
  write(json_data,"output.json")
  
}





######################### read in observed data  #######################################
# reprocess obs data from cores and changes format
# Paccum_obs<-read.csv(here::here("obs","Paccum.csv")) #observed data
# Accretion_obs<-read.csv(here::here("obs","accretion.csv")) #observed data

obs<-read.csv(here::here("obs","BOA_calibration_data.csv"))

Paccum_obs<-obs %>% 
  filter(grepl('Paccum',variable))

Accretion_obs <-obs %>% 
  filter(grepl('Accretion',variable)) %>% 
  rename( "sed_rate_g_m2" = "value")


################## Data for BOA ########################################################
BOA<-data.frame(c("res1","res2","res3"))
BOA$sed<-NA
BOA$P<-NA

colnames(BOA)<-c("res","sed_accum","p_accum")

 

################# Read in yearly estuary data ##########################################


simDF<-headers<-c("jday", "mon",   "day",    "yr", "unit",   "gis_id",   "name",  "area", "precip", "evap",   "seep", "flo_stor",
                  "sed_stor", "orgn_stor", "sedp_stor", "no3_stor", "solp_stor", "chla_stor", "nh3_stor", "no2_stor", "cbod_stor",
                  "dox_stor", "san_stor", "sil_stor", "cla_stor", "sag_stor", "lag_stor", "grv_stor",   "null","setl_stor","setlp_stor", "flo_in", "sed_in",
                  "orgn_in",  "sedp_in",  "no3_in",   "solp_in",  "chla_in",  "nh3_in", "no2_in", "cbod_in",  "dox_in", "san_in",
                  "sil_in",   "cla_in",   "sag_in",   "lag_in",     "grv_in",   "null", "setl_in", "setlp_in", "flo_out",  "sed_out", "orgn_out",
                  "sedp_out", "no3_out",  "solp_out",   "chla_out",   "nh3_out",  "no2_out",  "cbod_out", "dox_out",  "san_out",
                  "sil_out",  "cla_out",  "sag_out",      "lag_out",        "grv_out",           "null", "setl_out","setlp_out")




################## Read in hru output ##########################################



tmp <- file(paste0(TxtInOutPath,'/reservoir_day.txt'))
open(tmp, "r") #read

#read past headerlines
readLines(tmp, n = 3) 



###### read in simulated data columns #########

data<-readLines(tmp, n = -1)
close(tmp)
DF<-strsplit(data,split=" ")
DF<-lapply(DF, function(z){ z[z != ""]}) 
DF<-data.frame(do.call(rbind, DF)) #unlist
colnames(DF)<-headers

DF[,c(1:6,8:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:6,8:(ncol(DF)-1))]))           # convert to numerics

#### P accum rate ###################
#area is in ha - convert to m2
dir.create(paste0(trial_dir,"/Graphs"))
GraphPath<-paste0(trial_dir,"/Graphs")
setwd(GraphPath)

DF$Paccum<-DF$setlp_stor*1000/(DF$area*10000) #kg--> g/m2
DF$Sedaccum<-DF$setl_stor*10^6/(DF$area*10000) #tons -->g/m2

DF1<-select(DF,c("yr","day","mon","name","area","Paccum"))



#################################### P ACCUM ##########################################################

#################################### res 1 ##############################################################
res_num<-"res1"
res_title<-"open water/deep"



res1<-DF1[DF1$name==res_num, ]

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=sum(Paccum,na.rm=T))
colnames(res1_sedaccum)<-c("year","value") #value is g/m2/y


res1_sedaccum$var<-"sim"

obs_res1<-Paccum_obs[Paccum_obs$res==res_num,]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","value")],res1_sedaccum[,c("var","value")])
sed_rate$res<-res_num

BOA$p_accum[BOA$res==res_num]<- abs(mean(sed_rate$value[sed_rate$var=="sim"],
                                          na.rm=T)-mean(sed_rate$value[sed_rate$var=="obs"],na.rm=T))



res1_paccum<-ggplot(sed_rate,aes(x=res_num,y=value,group=var,fill=var))+geom_boxplot()+ggtitle(res_title)+ylab(bquote('P accumulation ' (~g ~m^-2 ~y^-1)))+xlab("")+
scale_fill_manual(name="", labels=c("obs" = " Observed - Sediment cores", "sim" = "Simulated - SWAT+"),values=c('white','grey'))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=6, color="red", fill="red",aes(group=var),position=position_dodge(0.75))+
  scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.5,by=0.5))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),legend.key=element_rect(fill="white"),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position='bottom')

ggsave(paste0(res_num,"_paccum.png"),last_plot(),height=150,width=150,units="mm")
#################################### res 2 ##############################################################
res_num<-"res2"
res_title<-"cattail/shallow"

res1<-DF1[DF1$name==res_num, ]

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=sum(Paccum,na.rm=T))
colnames(res1_sedaccum)<-c("year","value") #value is g/m2/y


res1_sedaccum$var<-"sim"

obs_res1<-Paccum_obs[Paccum_obs$res==res_num,]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","value")],res1_sedaccum[,c("var","value")])
sed_rate$res<-res_num

BOA$p_accum[BOA$res==res_num]<- abs(mean(sed_rate$value[sed_rate$var=="sim"],
                                         na.rm=T)-mean(sed_rate$value[sed_rate$var=="obs"],na.rm=T))



res2_paccum<-ggplot(sed_rate,aes(x=res_num,y=value,group=var,fill=var))+geom_boxplot()+ggtitle(res_title)+ylab(bquote('P accumulation ' (~g ~m^-2 ~y^-1)))+xlab("")+
  scale_fill_manual(name="", labels=c("obs" = " Observed - Sediment cores", "sim" = "Simulated - SWAT+"),values=c('white','grey'))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=6, color="red", fill="red",aes(group=var),position=position_dodge(0.75))+
  scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.5,by=0.5))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),legend.key=element_rect(fill="white"),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position='bottom')

ggsave(paste0(res_num,"_paccum.png"),last_plot(),height=150,width=150,units="mm")

#################################### res 3 ##############################################################
res_num<-"res3"
res_title<-"leaf/intermediate"

res1<-DF1[DF1$name==res_num, ]

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=sum(Paccum,na.rm=T))
colnames(res1_sedaccum)<-c("year","value") #value is g/m2/y


res1_sedaccum$var<-"sim"

obs_res1<-Paccum_obs[Paccum_obs$res==res_num,]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","value")],res1_sedaccum[,c("var","value")])
sed_rate$res<-res_num

BOA$p_accum[BOA$res==res_num]<- abs(mean(sed_rate$value[sed_rate$var=="sim"],
                                         na.rm=T)-mean(sed_rate$value[sed_rate$var=="obs"],na.rm=T))



res3_paccum<-ggplot(sed_rate,aes(x=res_num,y=value,group=var,fill=var))+geom_boxplot()+ggtitle(res_title)+ylab(bquote('P accumulation ' (~g ~m^-2 ~y^-1)))+xlab("")+
  scale_fill_manual(name="", labels=c("obs" = " Observed - Sediment cores", "sim" = "Simulated - SWAT+"),values=c('white','grey'))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=6, color="red", fill="red",aes(group=var),position=position_dodge(0.75))+
  scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.5,by=0.5))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),legend.key=element_rect(fill="white"),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position='bottom')

ggsave(paste0(res_num,"_paccum.png"),last_plot(),height=150,width=150,units="mm")


########################################## SEDIMENT ####################################################
#### accretion rate ###################
#area is in ha - convert to m2
#sediment settling = sed in - sed out + storage day of - storage day before
#lag sed stor 


DF<-select(DF,c("yr","day","mon","name","area","Sedaccum"))

#################################### sed accum ##########################################################

#################################### res 1 ##############################################################
res_num<-"res1"
res_title<-"deep/open water"

res1<-DF[DF$name==res_num, ]

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=sum(Sedaccum,na.rm=T))
colnames(res1_sedaccum)<-c("year","sed_rate_g_m2") 

res1_sedaccum$var<-"sim"

obs_res1<-Accretion_obs[Accretion_obs$res==res_num,]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","sed_rate_g_m2")],res1_sedaccum[,c("var","sed_rate_g_m2")])
sed_rate$res<-res_num

BOA$sed_accum[BOA$res==res_num]<- abs(mean(sed_rate$sed_rate_g_m2[sed_rate$var=="sim"],
                                          na.rm=T)-mean(sed_rate$sed_rate_g_m2[sed_rate$var=="obs"],na.rm=T))



res1_sedaccum_plot<-ggplot(sed_rate,aes(x=res_num,y=sed_rate_g_m2,group=var,fill=var))+geom_boxplot()+ggtitle(res_title)+ylab(bquote('Sediment accretion ' (~g ~m^-2 ~y^-1)))+xlab("")+
  scale_fill_manual(name="", labels=c("obs" = " Observed - Sediment cores", "sim" = "Simulated - SWAT+"),values=c('white','grey'))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=6, color="red", fill="red",aes(group=var),position=position_dodge(0.75))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),legend.key=element_rect(fill="white"),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position='bottom')

ggsave(paste0(res_num,"_sedaccum.png"),last_plot(),height=150,width=150,units="mm")
#################################### res 2 ##############################################################
res_num<-"res2"
res_title<-"cattail/shallow"

res1<-DF[DF$name==res_num, ]

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=sum(Sedaccum,na.rm=T))
colnames(res1_sedaccum)<-c("year","sed_rate_g_m2") 

res1_sedaccum$var<-"sim"

obs_res1<-Accretion_obs[Accretion_obs$res==res_num,]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","sed_rate_g_m2")],res1_sedaccum[,c("var","sed_rate_g_m2")])
sed_rate$res<-res_num

BOA$sed_accum[BOA$res==res_num]<- abs(mean(sed_rate$sed_rate_g_m2[sed_rate$var=="sim"],
                                           na.rm=T)-mean(sed_rate$sed_rate_g_m2[sed_rate$var=="obs"],na.rm=T))


res2_sedaccum_plot<-ggplot(sed_rate,aes(x=res_num,y=sed_rate_g_m2,group=var,fill=var))+geom_boxplot()+ggtitle(res_title)+ylab(bquote('Sediment accumulation ' (~g ~m^-2 ~y^-1)))+xlab("")+
  scale_fill_manual(name="", labels=c("obs" = " Observed - Sediment cores", "sim" = "Simulated - SWAT+"),values=c('white','grey'))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=6, color="red", fill="red",aes(group=var),position=position_dodge(0.75))+
  geom_text(label=param,x=res_num,y=y_text)+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),legend.key=element_rect(fill="white"),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position='bottom')

ggsave(paste0(res_num,"_sedaccum.png"),last_plot(),height=150,width=150,units="mm")
#################################### res 3 ##############################################################
res_num<-"res3"
res_title<-"floating leaf/intermediate"

res1<-DF[DF$name==res_num, ]

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=sum(Sedaccum,na.rm=T))
colnames(res1_sedaccum)<-c("year","sed_rate_g_m2") 

res1_sedaccum$var<-"sim"

obs_res1<-Accretion_obs[Accretion_obs$res==res_num,]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","sed_rate_g_m2")],res1_sedaccum[,c("var","sed_rate_g_m2")])
sed_rate$res<-res_num

BOA$sed_accum[BOA$res==res_num]<- abs(mean(sed_rate$sed_rate_g_m2[sed_rate$var=="sim"],
                                           na.rm=T)-mean(sed_rate$sed_rate_g_m2[sed_rate$var=="obs"],na.rm=T))

y_text<-max(sed_rate$sed_rate_g_m2)-5

res3_sedaccum_plot<-ggplot(sed_rate,aes(x=res_num,y=sed_rate_g_m2,group=var,fill=var))+geom_boxplot()+ggtitle(res_title)+ylab(bquote('Sediment accumulation ' (~g ~m^-2 ~y^-1)))+xlab("")+
  scale_fill_manual(name="", labels=c("obs" = " Observed - Sediment cores", "sim" = "Simulated - SWAT+"),values=c('white','grey'))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=6, color="red", fill="red",aes(group=var),position=position_dodge(0.75))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),legend.key=element_rect(fill="white"),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position = 'bottom')

ggsave(paste0(res_num,"_sedaccum.png"),last_plot(),height=150,width=150,units="mm")

############# combined plot ###############################

figure<-ggarrange(res1_paccum + rremove("xlab")+  geom_text(label=paste("p_stl = ",round(p_stl_res1,2)),x=res_num,y=0), #deep--int--shallow
          res3_paccum + rremove("xlab")+ ylab("")+  geom_text(label=paste("p_stl = ",round(p_stl_res3,2)),x=res_num,y=0),
          res2_paccum+ rremove("xlab")+ ylab("")+  geom_text(label=paste("p_stl = ",round(p_stl_res2,2)),x=res_num,y=0),

          res1_sedaccum_plot+ rremove("xlab")+ggtitle("")+  geom_text(label=paste("sed_stl = ",round(sed_stl_res1,2)),x=res_num,y=0)+
            scale_y_continuous(limits=c(0,2250),breaks=seq(0,2250,by=500)), # add scale here so if plots get cut off you can still see them in individual plots
          
          res3_sedaccum_plot+ rremove("xlab")+ ylab("")+ggtitle("")+  geom_text(label=paste("sed_stl = ",round(sed_stl_res3,2)),x=res_num,y=0)+
            scale_y_continuous(limits=c(0,2250),breaks=seq(0,2250,by=500)),
          
          res2_sedaccum_plot+ rremove("xlab")+ ylab("")+ggtitle("")+  geom_text(label=paste("sed_stl = ",round(sed_stl_res2,2)),x=res_num,y=0)+
            scale_y_continuous(limits=c(0,2250),breaks=seq(0,2250,by=500)),

          nrow=2,ncol=3,common.legend = T,align='hv')

ggsave('Final_plot.png',figure,height=190,width=300,units="mm")
############# write json output file ######################
setwd(trial_dir)

out_data <- list(
  res1_sedaccum=list(
    a=BOA$sed_accum[BOA$res=="res1"]
  ),
  res2_sedaccum=list(
    a=BOA$sed_accum[BOA$res=="res2"]
  ),
  res3_sedaccum=list(
    a=BOA$sed_accum[BOA$res=="res3"]
  ),
  res1_paccum=list(
    a=BOA$p_accum[BOA$res=="res1"]
  ),
  res2_paccum=list(
    a=BOA$p_accum[BOA$res=="res2"]
  ),
  res3_paccum=list(
    a=BOA$p_accum[BOA$res=="res3"]
  )
  
)
json_data <- toJSON(out_data,pretty=T)
setwd(trial_dir)
write(json_data,"output.json")