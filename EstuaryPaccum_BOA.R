write(c('here'),'location.txt',sep = "\n",append=T) #test

rm(list=ls())
xlib = c("dplyr","splitstackshape","stringr","rlang","reshape2","tidyr","here","rstudioapi","hydroGOF","ggplot2","scales","grid","ggpubr","cowplot",
         "hydroGOF","tibble","gg.gap","data.table","qdapRegex","lubridate","rjson","jsonlite","R.utils")
lapply(xlib, require, character.only=T) ; rm(xlib)

#########################################################################################
########################## place scenario data here #####################################
#########################################################################################
setwd(dirname(getActiveDocumentContext()$path))  # set wd to current folder script is saved in


######################## Read and set boa directory ##############################################
args <- commandArgs(trailingOnly=TRUE)
trial_dir <- args[1] #boa output directory

#trial_dir <-c("D:/swat_boa/boa_runs_20230210T153723/000000") #for testing
####################### copy model into boa directory ###########################################


dir.create(trial_dir,"/TxtInOut")
setwd('../..')
copyDirectory(here("TxtInOut"), paste0(trial_dir,"/TxtInOut"), recursive=TRUE)


######################### read in BOA params - json #############################################
Params <- read_json(paste0(trial_dir,"/parameters.json"))



# I assume here is where you might pass back and forth data from BOA?
# write new params based on outputs
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

tmp <- file(here(trial_dir,"TxtInOut",'nutrients.res'))
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
DF<-data.frame(do.call(rbind, DF)) #unlist
colnames(DF)<-headers



################# CHANGE RES PARAMS ######################################

DF$p_stl[DF$name == "nutres1"]     <- sprintf(p_stl_res1, fmt = '%#.5f')
DF$p_stl[DF$name == "nutres2"]     <- sprintf(p_stl_res2, fmt = '%#.5f')
DF$p_stl[DF$name == "nutres3"]     <- sprintf(p_stl_res3, fmt = '%#.5f')

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






file.remove(here(trial_dir,"TxtInOut",'nutrients.res'))

sink(here(trial_dir,"TxtInOut",'nutrients.res'), type=c("output"), append = T)

write(c(topOfFile),here(trial_dir,"TxtInOut",'nutrients.res'),sep = "\n",append=T)

write(c(paste0(DF[[1]],DF[[2]],DF[[3]],DF[[4]],DF[[5]],DF[[6]],DF[[7]],DF[[8]],DF[[9]],DF[[10]],
               DF[[11]],DF[[12]],DF[[13]])),here(trial_dir,"TxtInOut",'nutrients.res'),sep="\n",append=T)


sink()

############### READ IN SEDIMENT PARAMETER FILE ##########################

tmp <- file(here(trial_dir,"TxtInOut",'sediment.res'))
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
DF<-data.frame(do.call(rbind, DF)) #unlist
colnames(DF)<-headers



################# CHANGE RES PARAMS ######################################

DF$stl_vel[DF$name == "sedres1"]     <- sprintf(sed_stl_res1, fmt = '%#.5f')
DF$stl_vel[DF$name == "sedres2"]     <- sprintf(sed_stl_res2, fmt = '%#.5f')
DF$stl_vel[DF$name == "sedres3"]     <- sprintf(sed_stl_res3, fmt = '%#.5f')



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


file.remove(here(trial_dir,"TxtInOut",'sediment.res'))

sink(here(trial_dir,"TxtInOut",'sediment.res'), type=c("output"), append = T)

write(c(topOfFile),here(trial_dir,"TxtInOut",'sediment.res'),sep = "\n",append=T)

write(c(paste0(DF[[1]],DF[[2]],DF[[3]],DF[[4]],DF[[5]],DF[[6]],DF[[7]])),here(trial_dir,"TxtInOut",'sediment.res'),sep="\n",append=T)


sink()



######################## Run SWAT ######################################################
# run SWAT
setwd(here(trial_dir,"TxtInOut"))
x<-system('SWATPlus_60.5.5.exe') #run SWAT

#if SWAT crashed tell BOA, otherwise continue on to read SWAT outputs  
if (x == 157 | x == 72) {
  trial_status <- "FAILED"
  
  out_data <- list(
    TrialStatus=unbox(trial_status)
  )
  json_data <- toJSON(out_data, pretty = TRUE)
  write(json_data, "set_trial_status_from_wrapper.json")
  
}

########################### copy textinout to folder ##################################


########################################################################################
######################### read in observed data  #######################################
########################################################################################


Paccum_obs<-read.csv(here("obs","Paccum.csv")) #observed data
Accretion_obs<-read.csv(here("obs","accretion.csv")) #observed data

################## Data for BOA ########################################################
BOA<-data.frame(c("res1","res2","res3"))
BOA$sed<-NA
BOA$P<-NA

colnames(BOA)<-c("res","sed_accum","p_accum")

 

################# Read in yearly estuary data ##########################################


simDF<-headers<-c("jday", "mon",   "day",    "yr", "unit",   "gis_id",   "name",  "area", "precip", "evap",   "seep", "flo_stor",
                  "sed_stor", "orgn_stor", "sedp_stor", "no3_stor", "solp_stor", "chla_stor", "nh3_stor", "no2_stor", "cbod_stor",
                  "dox_stor", "san_stor", "sil_stor", "cla_stor", "sag_stor", "lag_stor", "grv_stor",   "null","setl_stor", "flo_in", "sed_in",
                  "orgn_in",  "sedp_in",  "no3_in",   "solp_in",  "chla_in",  "nh3_in", "no2_in", "cbod_in",  "dox_in", "san_in",
                  "sil_in",   "cla_in",   "sag_in",   "lag_in",     "grv_in",   "null", "setl_in",  "flo_out",  "sed_out", "orgn_out",
                  "sedp_out", "no3_out",  "solp_out",   "chla_out",   "nh3_out",  "no2_out",  "cbod_out", "dox_out",  "san_out",
                  "sil_out",  "cla_out",  "sag_out",      "lag_out",        "grv_out",           "null", "setl_out")




################################################################################
################## Read in hru output ##########################################
################################################################################



tmp <- file(here(trial_dir,"TxtInOut",'reservoir_day.txt'))
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

DF$Paccum<-DF$sedp_stor+DF$solp_stor

DF1<-select(DF,c("yr","day","mon","name","area","Paccum"))

#################################### P accum ##########################################################

#################################### res 1 ##############################################################
res1<-DF1[DF1$name=="res1", ]
res1<-rbind(res1,NA)
res1$Paccum_lag<-c(NA,head(res1$Paccum,-1))

res1$change_in_storage<-res1$Paccum-res1$Paccum_lag # positive = sediment storage inc., negative = decrease
#res1$inminout<-res1$sed_in-res1$sed_out

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=sum(change_in_storage,na.rm=T))
colnames(res1_sedaccum)<-c("year","deltaStorage_kg")


res1_sedaccum$var<-"sim"
names(res1_sedaccum)[names(res1_sedaccum) == "deltaStorage_kg"] <- "value_kg_y"

obs_res1<-Paccum_obs[Paccum_obs$depth=="deep",]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","value_kg_y")],res1_sedaccum[,c("var","value_kg_y")])
sed_rate$res<-"res1"

BOA$p_accum[BOA$res=="res1"]<- abs(mean(sed_rate$value_kg_y[sed_rate$var=="sim"],
                                          na.rm=T)-mean(sed_rate$value_kg_y[sed_rate$var=="obs"],na.rm=T))

ggplot(sed_rate,aes(y=value_kg_y,group=var,color=var))+geom_boxplot()+ggtitle("res1 - open water/deep")

#################################### res 2 ##############################################################
res1<-DF1[DF1$name=="res2", ]
res1<-rbind(res1,NA)
res1$Paccum_lag<-c(NA,head(res1$Paccum,-1))

res1$change_in_storage<-res1$Paccum-res1$Paccum_lag # positive = sediment storage inc., negative = decrease
#res1$inminout<-res1$sed_in-res1$sed_out

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=sum(change_in_storage,na.rm=T))
colnames(res1_sedaccum)<-c("year","deltaStorage_kg")


res1_sedaccum$var<-"sim"
names(res1_sedaccum)[names(res1_sedaccum) == "deltaStorage_kg"] <- "value_kg_y"

obs_res1<-Paccum_obs[Paccum_obs$depth=="intermediate",]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","value_kg_y")],res1_sedaccum[,c("var","value_kg_y")])
sed_rate$res<-"res1"

BOA$p_accum[BOA$res=="res2"]<- abs(mean(sed_rate$value_kg_y[sed_rate$var=="sim"],
                                        na.rm=T)-mean(sed_rate$value_kg_y[sed_rate$var=="obs"],na.rm=T))

ggplot(sed_rate,aes(y=value_kg_y,group=var,color=var))+geom_boxplot()+ggtitle("res2 - intermediate")


#################################### res 3 ##############################################################
res1<-DF1[DF1$name=="res3", ]
res1<-rbind(res1,NA)
res1$Paccum_lag<-c(NA,head(res1$Paccum,-1))

res1$change_in_storage<-res1$Paccum-res1$Paccum_lag # positive = sediment storage inc., negative = decrease
#res1$inminout<-res1$sed_in-res1$sed_out

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=sum(change_in_storage,na.rm=T))
colnames(res1_sedaccum)<-c("year","deltaStorage_kg")


res1_sedaccum$var<-"sim"
names(res1_sedaccum)[names(res1_sedaccum) == "deltaStorage_kg"] <- "value_kg_y"

obs_res1<-Paccum_obs[Paccum_obs$depth=="shallow",]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","value_kg_y")],res1_sedaccum[,c("var","value_kg_y")])
sed_rate$res<-"res1"

BOA$p_accum[BOA$res=="res3"]<- abs(mean(sed_rate$value_kg_y[sed_rate$var=="sim"],
                                        na.rm=T)-mean(sed_rate$value_kg_y[sed_rate$var=="obs"],na.rm=T))

ggplot(sed_rate,aes(y=value_kg_y,group=var,color=var))+geom_boxplot()+ggtitle("res3 - shallow")



########################################## SEDIMENT ####################################################
#### accretion rate ###################
#area is in ha - convert to m2
#sediment settling = sed in - sed out + storage day of - storage day before
#lag sed stor 


DF<-select(DF,c("yr","day","mon","name","area","sed_stor","sed_in","sed_out","setl_stor"))

#################################### sed accum ##########################################################

#################################### res 1 ##############################################################
res1<-DF[DF$name=="res1", ]
res1<-rbind(res1,NA)
res1$sed_stor_lag<-c(NA,head(res1$sed_stor,-1))

res1$change_in_storage<-res1$sed_stor-res1$sed_stor_lag # positive = sediment storage inc., negative = decrease
res1$inminout<-res1$sed_in-res1$sed_out

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=mean(change_in_storage,na.rm=T))
colnames(res1_sedaccum)<-c("year","deltaStorage_ton")

res1_sedinminout<-res1 %>%
  group_by(yr) %>%
  summarize(value=mean(change_in_storage,na.rm=T)) #these are the same

res1_area<-res1 %>%
  group_by(yr) %>%
  summarize(value=mean(area,na.rm=T))
colnames(res1_area)<-c("year","area_ha")

res1_accum<-left_join(res1_sedaccum,res1_area,by=c("year"))

res1_accum$sed_rate_g_m2<-res1_accum$deltaStorage_ton*100/res1_accum$area_ha
res1_accum$var<-"sim"
#names(obs_res1)[names(obs_res1) == "sed_rate_g_m2"] <- "sed_rate_g_m2"

obs_res1<-Accretion_obs[Accretion_obs$depth=="deep",]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","sed_rate_g_m2")],res1_accum[,c("var","sed_rate_g_m2")])
sed_rate$res<-"res1"

BOA$sed_accum[BOA$res=="res1"]<- abs(mean(sed_rate$sed_rate_g_m2[sed_rate$var=="sim"],
                                          na.rm=T)-mean(sed_rate$sed_rate_g_m2[sed_rate$var=="obs"],na.rm=T))

#################################### res 2 ##############################################################
res1<-DF[DF$name=="res2", ]
res1<-rbind(res1,NA)
res1$sed_stor_lag<-c(NA,head(res1$sed_stor,-1))

res1$change_in_storage<-res1$sed_stor-res1$sed_stor_lag # positive = sediment storage inc., negative = decrease
res1$inminout<-res1$sed_in-res1$sed_out

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=mean(change_in_storage,na.rm=T))
colnames(res1_sedaccum)<-c("year","deltaStorage_ton")

res1_sedinminout<-res1 %>%
  group_by(yr) %>%
  summarize(value=mean(change_in_storage,na.rm=T)) #these are the same

res1_area<-res1 %>%
  group_by(yr) %>%
  summarize(value=mean(area,na.rm=T))
colnames(res1_area)<-c("year","area_ha")

res1_accum<-left_join(res1_sedaccum,res1_area,by=c("year"))

res1_accum$sed_rate_g_m2<-res1_accum$deltaStorage_ton*100/res1_accum$area_ha
res1_accum$var<-"sim"
#names(obs_res1)[names(obs_res1) == "sed_rate_g_m2"] <- "sed_rate_g_m2"

obs_res1<-Accretion_obs[Accretion_obs$depth=="deep",]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","sed_rate_g_m2")],res1_accum[,c("var","sed_rate_g_m2")])
sed_rate$res<-"res2"

BOA$sed_accum[BOA$res=="res2"]<- abs(mean(sed_rate$sed_rate_g_m2[sed_rate$var=="sim"],
                                          na.rm=T)-mean(sed_rate$sed_rate_g_m2[sed_rate$var=="obs"],na.rm=T))

#################################### res 3 ##############################################################
res1<-DF[DF$name=="res3", ]
res1<-rbind(res1,NA)
res1$sed_stor_lag<-c(NA,head(res1$sed_stor,-1))

res1$change_in_storage<-res1$sed_stor-res1$sed_stor_lag # positive = sediment storage inc., negative = decrease
res1$inminout<-res1$sed_in-res1$sed_out

res1_sedaccum<-res1 %>%
  group_by(yr) %>%
  summarize(value=mean(change_in_storage,na.rm=T))
colnames(res1_sedaccum)<-c("year","deltaStorage_ton")

res1_sedinminout<-res1 %>%
  group_by(yr) %>%
  summarize(value=mean(change_in_storage,na.rm=T)) #these are the same

res1_area<-res1 %>%
  group_by(yr) %>%
  summarize(value=mean(area,na.rm=T))
colnames(res1_area)<-c("year","area_ha")

res1_accum<-left_join(res1_sedaccum,res1_area,by=c("year"))

res1_accum$sed_rate_g_m2<-res1_accum$deltaStorage_ton*100/res1_accum$area_ha
res1_accum$var<-"sim"
#names(obs_res1)[names(obs_res1) == "sed_rate_g_m2"] <- "sed_rate_g_m2"

obs_res1<-Accretion_obs[Accretion_obs$depth=="deep",]
obs_res1$var<-"obs"

sed_rate<-rbind(obs_res1[,c("var","sed_rate_g_m2")],res1_accum[,c("var","sed_rate_g_m2")])
sed_rate$res<-"res3"

BOA$sed_accum[BOA$res=="res3"]<- abs(mean(sed_rate$sed_rate_g_m2[sed_rate$var=="sim"],
                                          na.rm=T)-mean(sed_rate$sed_rate_g_m2[sed_rate$var=="obs"],na.rm=T))


############# write json output file ######################
out_data <- list(
  res1_sedaccum=list(
    a=BOA$sed_accum[BOA$res=="res1"]
  ),
  res2_sedaccum=list(
    a=BOA$sed_accum[BOA$res=="res2"]
  )
)
json_data <- toJSON(out_data,pretty=T)
setwd(trial_dir)
write(json_data,"output.json")