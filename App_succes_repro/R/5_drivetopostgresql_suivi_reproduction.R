
#------------------------------------TITRE--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  14/10/2021
#  Version: 1.0.0
#  Description: intègre met à jour les données de reproduction annuelle des animaux à partir des données entrée dans l'application Qfield success_repro
#  Documentation:
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
mypackages<-c("googledrive", "foreign", "RPostgreSQL","uuid","lubridate","rgdal","sf","dplyr","geojsonio")
for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

annee_suivi <- 2021

source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
con<-serveur

if(!dir.exists(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_repro/DonneesBrutes/",annee_suivi,""))){
dir.create(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_repro/DonneesBrutes/",annee_suivi,""))
}

source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")

setwd(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_repro/DonneesBrutes/",annee_suivi,""))

drive_download(as_id(drive_find(pattern = "succes_repro.geojson")$id), overwrite = TRUE)

2

#####à changer dans le geojson"name": "urn:ogc:def:crs:EPSG::2154"
d<-geojsonio::geojson_read("succes_repro.geojson", what = "sp", options = "ENCODING=UTF-8")
#d<-geojsonsf::geojson_sf("succes_repro.geojson")
#st_crs(d) <- 2154

d<-st_as_sf(d)

d_coords <- unlist(st_geometry(d)) %>% 
  matrix(ncol=2,byrow=TRUE) %>% 
  as_tibble() %>% 
  setNames(c("lon","lat"))

d<-bind_cols(d,d_coords)
write.csv2(as.data.frame(d),paste0("succes_repro_",annee_suivi,".csv"), row.names =F)


colnam<-dbGetQuery(con,paste0("
  SELECT column_name FROM information_schema.columns WHERE table_schema = 'public' AND table_name   = 't_repro_rep' and (column_default is null OR column_default !~* 'nextval');"
))[,1]

d<-utf8(as.data.frame(d))


for (i in (1:dim(d)[1])) {
  
  ani_id<- dbGetQuery(con,paste0("SELECT ani_id FROM t_animal_ani where ani_etiq in ",v2dbn(paste0("'",unlist(lapply(str_split(as.data.frame(d[i,"Animal"])[,1],"_"),"[[",2)),"'"))," "))
  #rep_cap_id<-dbGetQuery(con,paste0("SELECT cap_id FROM public.t_capture_cap where cap_ani_id = ",ani_id," and cap_annee_suivi = ",annee_suivi,""))
  rep_obs1<-as.numeric(d[i,"repro_1"])
  rep_obs1_date<-dmy(d[i,"repro_1_date"])
  rep_obs1_remarque<-d[i,"repro_1_rq"]
  rep_obs2<-as.numeric(d[i,"repro_2"])
  rep_obs2_date<-dmy(d[i,"repro_2_date"])
  rep_obs2_remarque<-d[i,"repro_2_rq"]
  rep_obs3<-as.numeric(d[i,"repro_3"])
  rep_obs3_date<-dmy(d[i,"repro_3_date"])
  rep_obs3_remarque<-d[i,"repro_3_rq"]
  rep_repro<- d[i,"repro"]
  rep_repro_remarque<-d[i,"repro_remarque"]
  rep_annee_repro<-annee_suivi

  val<-unlist(c(ani_id,rep_obs1,as.character(rep_obs1_date),rep_obs1_remarque,rep_obs2,as.character(rep_obs2_date),rep_obs2_remarque,rep_obs3,as.character(rep_obs3_date),rep_obs3_remarque,rep_repro,rep_repro_remarque, rep_annee_repro))
  if (length(na.omit(val)) == 1) {next}
  val <- gsub("NA","NULL", val)
  val <- gsub("'NULL'","NULL", val)
  val<-replace(val, is.na(val), "NULL")
  val<- str_replace_all(v2db(val),"'NULL'","NULL")

  dbSendQuery(con,paste0("
INSERT INTO public.t_repro_rep ",v2dbn(colnam)," values ",val," ON CONFLICT (rep_ani_id,rep_annee_repro ) DO UPDATE SET (rep_obs1,rep_obs1_date,rep_obs1_remarque,rep_obs2,rep_obs2_date,rep_obs2_remarque,rep_obs3,rep_obs3_date,rep_obs3_remarque,rep_repro,rep_repro_remarque)  =  (excluded.rep_obs1,excluded.rep_obs1_date,excluded.rep_obs1_remarque,excluded.rep_obs2,excluded.rep_obs2_date,excluded.rep_obs2_remarque,excluded.rep_obs3,excluded.rep_obs3_date,excluded.rep_obs3_remarque,excluded.rep_repro,excluded.rep_repro_remarque)
             "))
  }###balaye le sous jeu de données (pour un type de variable) et alimente la table correspondante
#####les WARNING() NAs introduced by coercion sont liés au as.numeric(NA) donc pas grave
