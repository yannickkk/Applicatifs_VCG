#------------------------------------TITRE--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  03/09/2021
#  Description: copier le geojson de l'appli suivi_adulte de l'année en cours et lancer le script
# Documentation:
#
#
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------
mypackages<-c("shiny", "shinyjs","plotly","reshape","dplyr","tidyverse","tidyr","markdown","leaflet","leaflet.minicharts","units","maptools","leafsync","sodium","rdrop2","lubridate", "magrittr", "sf","RPostgreSQL","data.table")
for (p in mypackages){
if(!require(p, character.only = TRUE)){
install.packages(p)
library(p, character.only = TRUE)
}
}
#-----------------------------------------------------------------------------
#-------------------------- connection aux bases de donnees ------------------
#source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R")
#source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R"))
#source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_CEFS/con_localhost_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/con_serveur_dbgardouch.R")
#-------------------------- chargement de mes fonctions ----------------------
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")
#####auto install packages

mypackages<-c("googledrive", "foreign", "RPostgreSQL","uuid","lubridate","rgdal","sf", "dplyr", "as_tibble")
for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

#-----------------------------------------------------------------------------
#-------------------------- connection aux bases de donnees ------------------
#source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R")
#source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R"))
source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
# source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/con_serveur_dbgardouch.R")
#-------------------------- chargement de mes fonctions ----------------------
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")

#setwd("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Programmes/R/google_drive/DATA")
setwd("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_repro/DonneesBrutes/2021")

dat<-st_read("suivi_adultes.geojson")

dat<-data.frame(dat)

dat<-rbind(dat[grep("f_",dat[,"Animal"]),], dat[grep("fy_",dat[,"Animal"]),])
dat<- rbind(dat[is.na(dat$Cause_fin_suivi),],dat[which(dat$Cause_fin_suivi =='Panne collier'),])

dat<-dat[,c("Memoire","Type_collier","Animal","Collier","Boitier","Hors_service","ani_id","nom", "geometry")]
                
dat$repro_1<-NA
dat$repro_1_date<-NA
dat$repro_1_rq<-NA
dat$repro_2<-NA
dat$repro_2_date<-NA
dat$repro_2_rq<-NA
dat$repro_3<-NA
dat$repro_3_date<-NA
dat$repro_3_rq<-NA
dat$repro<-NA
dat$repro_remarque<-NA


bac<-st_as_sf(dat, crs = 2154)

st_write(bac, dsn = "C:/Users/ychaval/Documents/BD_tools/GitHub/Applicatifs_VCG/App_succes_repro/succes_repro.geojson", layer = "succes_repro", driver = "GeoJSON")

