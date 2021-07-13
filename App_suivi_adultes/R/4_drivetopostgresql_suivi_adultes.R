#------------------------------------Intégration des données de suivi adultes--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  13/07/2021
#  Version: V2.0
#  Description: Par rapport à la version V1.0, cette version inclus la mise à jour de ani_date_fin_suivi au mercredi de la semaine de dernier contact et la maj de ani_date_fin_suivi = ani_date_mort pour les animaux morts
#  Documentation:
#
#
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------
mypackages<-c("googledrive", "foreign", "RPostgreSQL","uuid","lubridate","rgdal","sf","dplyr")
for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}
#-----------------------------------------------------------------------------
#-------------------------- connection aux bases de donnees ------------------
#source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R")
#source("C:/Users/ychaval/Documents/BD_CEFS/con_local_dbcefs.R")
source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/con_serveur_dbgardouch.R")
#-------------------------- chargement de mes fonctions ----------------------
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")

# dbGetQuery(con, "
# SELECT pg_terminate_backend(pg_stat_activity.pid)
#            FROM pg_stat_activity
#            WHERE pg_stat_activity.datname = 'db_chevreuils' -- ← change this to your DB
#            AND pid <> pg_backend_pid();
#            ")
# dbDisconnect(con)

annee_suivi <- 2020

####https://googledrive.tidyverse.org/reference/drive_download.html

setwd("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/DonneesBrutes")

drive_download(as_id(drive_find(pattern = "suivi_adultes.geojson")$id), overwrite = TRUE)

2

#####à changer dans le geojson"name": "urn:ogc:def:crs:EPSG::2154"

d<-geojsonsf::geojson_sf("suivi_adultes.geojson")
st_crs(d) <- 2154

d_coords <- unlist(st_geometry(d)) %>% 
  matrix(ncol=2,byrow=TRUE) %>% 
  as_tibble() %>% 
  setNames(c("lon","lat"))

d<-bind_cols(d,d_coords)
write.csv2(as.data.frame(d),paste0("suivi_adultes_",annee_suivi,".csv"), row.names =F)

d<-d[-which(is.na(d$ani_id)),]
d<-utf8(as.data.frame(d))


for (i in (1:dim(d)[1])) {
  
  
  ani_id<- d[i,"ani_id"]
  if (!is.na(d[i,"Cause_fin_suivi"]) & d[i,"Cause_fin_suivi"] == "Mort") {ani_mortalite <- TRUE;ani_mort_x <-d[i,"lon"];ani_mort_y<-d[i,"lat"]} else {ani_mortalite <- FALSE;ani_mort_x <- NA;ani_mort_y<-NA}
  ani_date_mort_text<-d[i,"Date_mort_text"]
  ani_date_mort<- ymd(d[i,"Date_mort"])
  ani_date_fin_suivi<-dmy(d[i,"Date_fin_suivi"])
  if (!is.na(d[i,"Cause_fin_suivi"]) & d[i,"Cause_fin_suivi"] == "Mort" & is.na(ani_date_mort) &  length(dmy(d[i,"Date_fin_suivi"])) != 0)    {ani_date_mort <- dmy(d[i,"Date_fin_suivi"])} else if (!is.na(d[i,"Cause_fin_suivi"]) & d[i,"Cause_fin_suivi"] == "Mort" & is.na(ani_date_mort) & is.na(ani_date_mort_text) ){ani_date_mort_text<-d[i,"Cause_fin_suivi"]}
  if (!is.na(d[i,"Cause_fin_suivi"]) & d[i,"Cause_fin_suivi"] == "Mort" & is.na(ani_date_mort)) {ani_date_mort<- as.Date(paste(annee_suivi, as.numeric(d[i,"Date_dernier_contact"])-1, 3, sep="-"), "%Y-%U-%u");ani_date_fin_suivi<- ani_date_mort;ani_date_mort_arrondi <-TRUE}
  ani_cause_mort<-d[i,"Cause_mort"]           
  ani_date_fin_suivi_text<-d[i,"Date_fin_suivi_text"] 
  ani_cause_fin_suivi<-d[i,"Cause_fin_suivi"]
  ani_cause_fin_remarque<-d[i,"Date_fin_suivi_text"]
  ani_cause_mort_classes<- d[i,"Cause_mort_classe"]    
  ani_poids_mort<-d[i,"Pds_mort"]  
  if (!is.na(ani_poids_mort)) {ani_poids_mort_na <- FALSE} else {ani_poids_mort_na <- TRUE}
  ani_necropsie<- ifelse(d[i,"Congel"] == 0,FALSE,TRUE)
  ani_lpa_mort<-d[i,"Lpa_mort"]
  if (is.na(ani_date_mort) & exists("ani_date_mort_arrondi")) {ani_remarque_suivi<- paste0("attention date de mort calculée automatiquement comme le milieu de la semaine de dernier contact ",d[i,"Remarque"],"")} else {ani_remarque_suivi<-d[i,"Remarque"]}
  if (!is.na(d[i,"Date_dernier_contact"])) {ani_dernier_contact<- paste0 ("dernier contact: année ",annee_suivi," semaine: ",d[i,"Date_dernier_contact"],"")} else {ani_dernier_contact<-NA}

dbSendQuery(con,enc2utf8(paste0("UPDATE public.t_animal_ani SET 
ani_mortalite= ",ani_mortalite,",
ani_mort_x= ",ifelse(is.na(ani_mort_x),'NULL::real',as.numeric(ani_mort_x)),",
ani_mort_y= ",ifelse(is.na(ani_mort_y),'NULL::real',as.numeric(ani_mort_y)),",
ani_date_mort_text= ",ifelse(is.na(ani_date_mort_text),'NULL::varchar',paste0("'",ani_date_mort_text,"'")),",
ani_date_mort= ",ifelse(is.na(ani_date_mort),'NULL::date',paste0("'",ani_date_mort,"'")),",
ani_cause_mort= ",ifelse(is.na(ani_cause_mort),'NULL::varchar',paste0("'",ani_cause_mort,"'")),",
ani_date_fin_suivi= ",ifelse(is.na(ani_date_fin_suivi),'NULL::varchar',paste0("'",ani_date_fin_suivi,"'")),",
ani_date_fin_suivi_text = ",ifelse(is.na(ani_date_fin_suivi_text),'NULL::varchar',paste0("'",ani_date_fin_suivi_text,"'")),",
ani_cause_mort_classes= ",ifelse(is.na(ani_cause_mort_classes),'NULL::varchar',paste0("'",ani_cause_mort_classes,"'")),",
ani_poids_mort= ",ifelse(is.na(ani_poids_mort),'NULL::real',as.numeric(ani_poids_mort)),",
ani_poids_mort_na= ",ani_poids_mort_na,",
ani_necropsie= ",ani_necropsie,",
ani_lpa_mort= ",ifelse(is.na(ani_poids_mort),'NULL::real',as.numeric(ani_lpa_mort)),",
ani_dernier_contact=",ifelse(is.na(ani_dernier_contact),'NULL::varchar',paste0("'",ani_dernier_contact,"'")),",
ani_cause_fin_suivi= ",ifelse(is.na(ani_cause_fin_suivi),'NULL::varchar',paste0("'",ani_cause_fin_suivi,"'")),",
ani_remarque_suivi= ",ifelse(is.na(ani_remarque_suivi),'NULL::varchar',paste0("'",ani_remarque_suivi,"'")),"
Where ani_id = ",ani_id,"")
                         )
            )
if (exists("ani_date_mort_arrondi")){dbSendQuery(con,enc2utf8(paste0("UPDATE public.t_animal_ani SET ani_date_mort_arrondi = TRUE Where ani_id = ",ani_id,"")));rm(ani_date_mort_arrondi)}


}

#######une fois les donnees integrees je reprends les dates de dernier contact pour calculer la date de fin de suivi a partir du numero de semaine.
sel<-utf8(dbGetQuery(serveur,"SELECT ani_id, ani_etiq, ani_sexe, ani_mortalite, ani_date_mort, ani_date_mort_arrondi, ani_date_mort_text, ani_poids_mort, ani_cause_mort, ani_poids_mort_na, ani_remarque, ani_mort_x, ani_mort_y, ani_inconnu, ani_name, ani_mere_observee, ani_mere_pedigree, ani_pere_pedigree, ani_fratrie, ani_fratrie_oct, ani_surv_faon_marque_oct, ani_agres_faon_marque, ani_cause_fin_suivi, ani_cause_fin_suivi_remarque, ani_date_fin_suivi, ani_cause_mort_classes, ani_necropsie, ani_crane, ani_lpa_mort, ani_remarque_suivi, ani_dernier_contact, ani_date_fin_suivi_text, ani_autopsie
FROM public.t_animal_ani where ani_dernier_contact is not null; "))

dat<- gsub(" semaine: ","~",gsub("dernier contact: année ","",sel$ani_dernier_contact[!is.na(sel$ani_dernier_contact)]))

datt<-NA
year <- unlist(lapply(str_split(dat,"~"),"[[",1))
week <- unlist(lapply(str_split(dat,"~"),"[[",2))
for (i in 1:length(year)){
  dd<-as.Date(paste(year[i], stringr::str_pad(week[i],width=2, pad="0"), "3", sep=""), "%Y%U%u")
  #datt<-append(datt,as.character(dd))  
  dbSendQuery(serveur,
              paste0("UPDATE public.t_animal_ani SET ani_date_fin_suivi = '",dd,"' where ani_etiq = '",sel$ani_etiq[i],"'; "))
}

######si une date de dernier contact est entree pour un individu mort, la ligne de commande au dessus va rendre approximative la date de fin de suivi je remets donc a jour la date de fin de suivi pour les animaux pour lesquels ani_mortalite = TRUE

dbSendQuery(serveur,
            paste0("UPDATE public.t_animal_ani SET ani_date_fin_suivi = ani_date_mort where ani_mortalite = TRUE; "))

