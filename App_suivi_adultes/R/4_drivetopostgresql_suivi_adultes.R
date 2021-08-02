#------------------------------------maj suivi_adulte--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  02/08/2021
#  Version: V0.2
#  Description: Ce script permet la mise à jour des informations de suivi des adultes.
#  Modification par rapport à la version précédente: Les informations de fin de suivi étaient jusqu'alors liées à l'individus, elles sont maintenant liées à l'association individu/collier de façon 
#  à avoir les dates de fin de suivi de chaque équipement porté par un animal (important en dynamique des pop pour censurer les individus non suivis) 
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
source("C:/Users/ychaval/Documents/BD_CEFS/con_local_dbcefs.R")
con<-local
#source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
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

annee_suivi <- 2021

####https://googledrive.tidyverse.org/reference/drive_download.html

setwd("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/DonneesBrutes")

#####à changer dans le geojson"name": "urn:ogc:def:crs:EPSG::2154"
dir.create(file.path("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/DonneesBrutes", annee_suivi), showWarnings = TRUE)
setwd(file.path("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/DonneesBrutes", annee_suivi))

drive_download(as_id(drive_find(pattern = "suivi_adultes.geojson")$id), overwrite = TRUE)

2



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
  ani_date_mort<- as.Date(d[i,"Date_mort"], "%d/%m/%Y")
  eqa_date_fin_suivi<-as.Date(d[i,"Date_fin_suivi"], "%d/%m/%Y")
  if (!is.na(d[i,"Cause_fin_suivi"]) & d[i,"Cause_fin_suivi"] == "Mort" & is.na(ani_date_mort) &  length(dmy(d[i,"Date_fin_suivi"])) != 0)    {ani_date_mort <- dmy(d[i,"Date_fin_suivi"])} else if (!is.na(d[i,"Cause_fin_suivi"]) & d[i,"Cause_fin_suivi"] == "Mort" & is.na(ani_date_mort) & is.na(ani_date_mort_text) ){ani_date_mort_text<-d[i,"Cause_fin_suivi"]}
  if (!is.na(d[i,"Cause_fin_suivi"]) & d[i,"Cause_fin_suivi"] == "Mort" & is.na(ani_date_mort)) {ani_date_mort<- as.Date(paste(annee_suivi, as.numeric(d[i,"Date_dernier_contact"])-1, 3, sep="-"), "%Y-%U-%u");eqa_date_fin_suivi<- ani_date_mort;ani_date_mort_arrondi <-TRUE}
  ani_cause_mort<-d[i,"Cause_mort"]           
  eqa_date_fin_suivi_text<-d[i,"Date_fin_suivi_text"] 
  eqa_cause_fin_suivi<-d[i,"Cause_fin_suivi"]
  ani_cause_fin_remarque<-d[i,"Date_fin_suivi_text"]
  ani_cause_mort_classes<- d[i,"Cause_mort_classe"]
  eqa_remarque_suivi<-d[i,"eqa_remarque_suivi"]
  ani_poids_mort<-gsub("kg","",wsr(tolower(d[i,"Pds_mort"])))  
  if (!is.na(ani_poids_mort)) {ani_poids_mort_na <- FALSE} else {ani_poids_mort_na <- TRUE}
  ani_necropsie<- ifelse(d[i,"Congel"] == 0,FALSE,TRUE)
  ani_lpa_mort<-d[i,"Lpa_mort"]
  if (is.na(ani_date_mort) & exists("ani_date_mort_arrondi")) {ani_remarque_suivi<- paste0("attention date de mort calculée automatiquement comme le milieu de la semaine de dernier contact ",d[i,"Remarque"],"")} else {ani_remarque_suivi<-d[i,"Remarque"]}
  if (!is.na(d[i,"Date_dernier_contact"])) {eqa_dernier_contact<- paste0 ("dernier contact: année ",annee_suivi," semaine: ",d[i,"Date_dernier_contact"],"")} else {eqa_dernier_contact<-NA}

dbSendQuery(con,enc2utf8(paste0("UPDATE public.t_animal_ani SET 
ani_mortalite= ",ani_mortalite,",
ani_mort_x= ",ifelse(is.na(ani_mort_x),'NULL::real',as.numeric(ani_mort_x)),",
ani_mort_y= ",ifelse(is.na(ani_mort_y),'NULL::real',as.numeric(ani_mort_y)),",
ani_date_mort_text= ",ifelse(is.na(ani_date_mort_text),'NULL::varchar',paste0("'",ani_date_mort_text,"'")),",
ani_date_mort= ",ifelse(is.na(ani_date_mort),'NULL::date',paste0("'",ani_date_mort,"'")),",
ani_cause_mort= ",ifelse(is.na(ani_cause_mort),'NULL::varchar',paste0("'",ani_cause_mort,"'")),",
ani_cause_mort_classes= ",ifelse(is.na(ani_cause_mort_classes),'NULL::varchar',paste0("'",ani_cause_mort_classes,"'")),",
ani_poids_mort= ",ifelse(is.na(ani_poids_mort),'NULL::real',as.numeric(ani_poids_mort)),",
ani_poids_mort_na= ",ani_poids_mort_na,",
ani_necropsie= ",ifelse(is.na(ani_necropsie),'FALSE::boolean','TRUE::boolean'),",
ani_lpa_mort= ",ifelse(is.na(ani_lpa_mort),'NULL::real',as.numeric(ani_lpa_mort)),"
Where ani_id = ",ani_id,"")
                         )
            )
if (exists("ani_date_mort_arrondi")){dbSendQuery(con,enc2utf8(paste0("UPDATE public.t_animal_ani SET ani_date_mort_arrondi = TRUE Where ani_id = ",ani_id,"")));rm(ani_date_mort_arrondi)}

if (!is.na(eqa_cause_fin_suivi)){
if (tolower(eqa_cause_fin_suivi) == "mort") {eqa_cause_fin_suivi <-"mort récupération du collier sur cadavre ou au sol avec signe de prédation"}
if (tolower(eqa_cause_fin_suivi) == "collier au sol") {eqa_cause_fin_suivi <-tolower(d[i,"Cause_fin_suivi"])}
if (tolower(eqa_cause_fin_suivi) == "drop off") {eqa_cause_fin_suivi <-tolower(d[i,"Cause_fin_suivi"])}
if (tolower(eqa_cause_fin_suivi) == "problème vhf") {eqa_cause_fin_suivi <-"problème VHF"}
}
  
dbSendQuery(con,enc2utf8(paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET 
eqa_date_fin_suivi= ",ifelse(is.na(eqa_date_fin_suivi),'NULL::date',paste0("'",eqa_date_fin_suivi,"'")),",
eqa_date_fin_suivi_text = ",ifelse(is.na(eqa_date_fin_suivi_text),'NULL::varchar',paste0("'",eqa_date_fin_suivi_text,"'")),",
eqa_dernier_contact=",ifelse(is.na(eqa_dernier_contact),'NULL::varchar',paste0("'",eqa_dernier_contact,"'")),",
eqa_cause_fin_suivi= ",ifelse(is.na(eqa_cause_fin_suivi)||is.null(eqa_cause_fin_suivi),'NULL::varchar',paste0("'",eqa_cause_fin_suivi,"'")),",
eqa_remarque_suivi= ",ifelse(is.na(eqa_remarque_suivi)||is.null(eqa_remarque_suivi),'NULL::varchar',paste0("'",eqa_remarque_suivi,"'")),"
WHERE eqa_ani_id = ",ani_id," AND  eqa_annee_suivi = ",annee_suivi," ")))
}

#######une fois les donnees integrees je reprends les dates de dernier contact pour calculer la date de fin de suivi a partir du numero de semaine.
sel<-utf8(dbGetQuery(con,"SELECT eqa_ani_id, eqa_eqt_id, eqa_dernier_contact
                     FROM public.tj_equipement_animal_eqt_ani_eqa where eqa_dernier_contact is not null; "))

dat<- gsub(" semaine: ","~",gsub("dernier contact: année ","",sel$eqa_dernier_contact[!is.na(sel$eqa_dernier_contact)]))

datt<-NA
year <- unlist(lapply(str_split(dat,"~"),"[[",1))
week <- unlist(lapply(str_split(dat,"~"),"[[",2))
for (i in 1:length(year)){
  dd<-as.Date(paste(year[i], stringr::str_pad(week[i],width=2, pad="0"), "3", sep=""), "%Y%U%u")
  #datt<-append(datt,as.character(dd)) 
  dbSendQuery(con,enc2utf8(paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin_suivi = '",dd,"',
                                  where eqa_ani_id = ",sel$eqa_ani_id[i]," and eqa_eqt_id = ",sel$eqa_eqt_id[i]," and eqa_date_fin_suivi is null; ")))

}

