library(tidyr)
library(tidyverse)
library(ggplot2)
library(survival)
library(survminer)
library(ranger)
library(ggpubr)
library(ggsci)
library(readxl)
library(showtext)
library(coxme)
library(lubridate)
library(pollen)
setwd("C:\\Users\\lblan\\OneDrive\\Escritorio\\PrimerCap\\Submission\\Datasets")
clima <- read_excel("clima_field.xlsx",
                    col_types = c("date",
                                  "numeric", "numeric", "numeric", "numeric",
                                  "numeric", "numeric", "text"))

#We name the locations
clima$location <- as.factor(clima$location)

#Divide the dataframe in two, per location (avoiding repetitions of dates and 
#mixing temperatures of different places when doing calculations)
clima_urban <- filter(clima, location == "Urban")
clima_veg <- filter(clima, location == "Vegetated")

#check diff in each dataframe to find if there's a number different to one
diff(clima_urban$start_date)
diff(clima_veg$start_date)

#GROWING DEGREE DAYS

#Urban
clima_urban<- clima_urban[-c(1:17),]
clima_urban <- clima_urban %>% 
  mutate(gdd = gdd(tmax = maxtemperature, tmin = mintemperature, tbase = 10,
                   tbase_max = 30)) %>% 
  mutate(daily_acc_gdd = c(NA, diff(gdd)))
ggplot(aes(x =start_date, y= daily_acc_gdd), data = clima_urban) + geom_line() #daily changes in the gdd in the urban area

#Peri_Urban
clima_veg<- clima_veg[-c(1:35),]
clima_veg <- clima_veg %>% 
  mutate(gdd = gdd(tmax = maxtemperature, tmin = mintemperature, tbase = 10,
                   tbase_max = 30)) %>% 
  mutate(daily_acc_gdd = c(NA, diff(gdd)))
ggplot(aes(x =start_date, y= daily_acc_gdd), data = clima_veg) + geom_line() #daily changes in the gdd in the periurban area

#data mosquito survival
data_analysis <-
  read_excel("data_analysis_cens.xlsx",
             col_types = c("numeric", "date","numeric", "date",
                           "numeric", "numeric", "numeric", "text"))
data_analysis$location <- as.factor(data_analysis$location)

df_urban <- filter(data_analysis, location == "Urban")
df_veg <- filter(data_analysis, location == "Vegetated")

clima <- rbind(clima_veg, clima_urban)
datos_peri <- inner_join(df_veg, clima_veg, 
                         by = c("start_date", "location"), all= TRUE) 

datos_urban <- inner_join(df_urban, clima_urban, 
                          by = c("start_date", "location"), all= TRUE)

datos_field<- rbind(datos_peri, datos_urban) #dataframe WITHOUT duplicated rows



#LET'S DUPLICATE ROWS! #we need for each day lived of the mosquito one row, as weather conditions changed everyday from the capture until the death
new_df <- datos_field[0, ] 

id <- seq_len(nrow(datos_field)) 

datos_field$start_date <- as.Date(datos_field$start_date, format = '%Y-%m-%d')
clima$start_date <- as.character(as.Date(clima$start_date, format = '%Y-%m-%d'))

new_df <- do.call('rbind', lapply(seq_len(nrow(datos_field)), function(id){
  tmp <- do.call('rbind', replicate(datos_field$total_lived[id],
                                    datos_field[id, ], simplify = FALSE))
  tmp$start_date <- format(seq(tmp$start_date[1], by = 'day',
                               length.out = nrow(tmp)), '%Y-%m-%d')
  tmp
}))

vars <- c('maxtemperature', 'meantemperature', 
          'mintemperature', 'maxrh', 'minrh', 'meanrh', 'gdd', 'daily_acc_gdd')
for(d in unique(clima$start_date)){
  new_df[new_df$start_date == d & new_df$location == "Vegetated", vars] <-
    clima[clima$start_date == d & clima$location =="Vegetated", vars]
  new_df[new_df$start_date == d & new_df$location == "Urban", vars] <-
    clima[clima$start_date == d & clima$location == "Urban", vars]
}

#Now, as we have time-dependent variables, we have to add the columns with 1 and
#change the censored!

#Here we want, for the individuals which were not censored, have censored = 0 in each row in which mosquitoes
#were still alive, and censored = 1 when the event (death) occurs, that is the last row of the mosquito.

#We split the dataframe filtering (then we will join it again) the mosquitoes censored to avoid putting a "1" in their last row
init = 1
dataframe_new <- filter(new_df, censored == 1)
dataframe_censoredno<- filter(new_df, censored == 0)

dataframe_new$censored = 0
for (i in seq(1:max(dataframe_new$id_mosquito))){
  subdf <- filter(dataframe_new, id_mosquito == i)
  r = nrow(subdf)
  max_date = max(subdf$start_date)
  dataframe_new$censored[which(dataframe_new$start_date == max_date & dataframe_new$id_mosquito == i )] = 1
}

new_df_of <- rbind(dataframe_new, dataframe_censoredno) #adding again the censored
new_df_of$time <- 1  #adding the "ones"

#######GDD by id_mosquito###############

#Here we want to calculate the growing degree days per mosquito, the "heat" accumulation that every mosquito
#suffers from the moment of capture to the death
result <- c()
for(i in unique(new_df_of$id_mosquito)){
  result <- c(result, gdd(new_df_of$maxtemperature[new_df_of$id_mosquito == i],
                          new_df_of$mintemperature[new_df_of$id_mosquito == i],
                          10, 30))
}
plot(result[1:40]) #we assure here that we have accumulations for each mosquito; 
#mosquito 1 lived 34 days, and then starts the second mosquito with his heat accumulation


#adding it to the dataframe
new_df_of$gdd_id <- result

##PHOTOPERIOD
library(meteor)
photoperiod <- photoperiod(152:337,  41.6833)
photoperiod <- as.data.frame(photoperiod)
values = seq(from = as.Date("2021-06-01"), to = as.Date("2021-12-03"), 
             by = 'day')
photoperiod$dates <- values
photoperiod$dates <- as.character(as.Date(photoperiod$dates, 
                                          format = '%Y-%m-%d'))


new_df_of$start_date <- as.character(as.Date(new_df_of$start_date, 
                                             format = '%Y-%m-%d'))

str(photoperiod)
new_df_of[ , 'photoperiod'] <- 0 # new column called photoperiod
vars2 <- c('photoperiod')
for(d in unique(photoperiod$dates)){
  new_df_of[new_df_of$start_date == d,vars2] <-
    photoperiod[photoperiod$dates == d, vars2]
}

range(new_df_of$photoperiod) #check that photoperiod has a correct range, no NAs there
#renaming the method of capture
new_df_of <- new_df_of%>%
  rename(method = "hl/bg")
new_df_of$id_mosquito <- as.factor(new_df_of$id_mosquito)

new_df_of$method <- factor(new_df_of$method, 
                           levels = c("1", "2"), 
                           labels = c("HLC", "BG"))

new_df_of$dayofy<- yday(new_df_of$start_date) #calculated doy
str(new_df_of$id_mosquito)
which(is.na(new_df_of)) #14 NAs in the 15.155 rows
new_df_of <- na.omit(new_df_of) #removing them to avoid problems

###MODELS###
model_ph<- coxme(Surv(time, censored) ~ photoperiod +
                   (1|location)+ (1|method) + (1|id_mosquito),
                 data= new_df_of)
summary(model_ph)


model_gddid<- coxme(Surv(time, censored) ~ gdd_id +
                   (1|location)+ (1|method) + (1|id_mosquito),
                 data= new_df_of)
summary(model_gddid)

modelgddph<- coxme(Surv(time, censored) ~  gdd_id + photoperiod +
                   (1|location)+ (1|method) + (1|id_mosquito),
                 data= new_df_of)
summary(modelgddph)

model_max<- coxme(Surv(time, censored) ~  maxtemperature + maxrh +
                   (1|location)+ (1|method) + (1|id_mosquito),
                 data= new_df_of)

summary(model_max)

model_min<- coxme(Surv(time, censored) ~  mintemperature + minrh +
                   (1|location)+ (1|method) + (1|id_mosquito),
                 data= new_df_of)

summary(model_min)

model_phgddmax<- coxme(Surv(time, censored) ~  gdd_id + photoperiod + maxrh +
                     (1|location)+ (1|method) + (1|id_mosquito),
                   data= new_df_of)
summary(model_phgddmax)
