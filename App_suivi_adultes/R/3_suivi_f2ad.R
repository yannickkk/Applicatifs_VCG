#------------------------------------suivi_faons_to_suivi_adultes--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  05/08/2021
#  Description: ce script permet de reccuperer les faons vivant en fin de suivi pour les intégrer dans l'appli suivi_adultes
#  Documentation:
#
#
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------
mypackages<-c("googledrive", "foreign","uuid","rgdal", "dplyr","lubridate", "sf","RPostgreSQL","data.table", "crayon")
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
#source("C:/Users/ychaval/Documents/BD_CEFS/con_localhost_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/con_serveur_dbgardouch.R")
#-------------------------- chargement de mes fonctions ----------------------
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")

setwd(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/suivi_adultes/",annee,""))
datad<-st_read("suivi_adultes.geojson")

datad<-as.data.frame(datad)

annee_suivi<-year(Sys.Date())

dat<-utf8(dbGetQuery(serveur,paste0("
select
ani_id, ani_etiq, ani_name, rfi_tag_code, cap_bague, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal, cap_annee_suivi, cap_date, ani_mere_observee, ani_mere_pedigree, ani_pere_pedigree, ani_fratrie, ani_fratrie_oct, ani_surv_faon_marque_oct, ani_agres_faon_marque, ani_sexe, cap_age_classe, cap_poids, cap_lpa, sit_nom_court, teq_nom_court, eqt_id_usuel, mar_libelle, mod_libelle, eqa_date_debut, eqa_date_fin, eqa_date_fin_suivi, eqa_date_fin_suivi_text, eqa_cause_fin_suivi, eqa_dernier_contact, ani_mortalite, ani_date_mort, ani_cause_mort, eqa_activite, eqa_probleme, eqa_date_fin_text, eqa_date_fin_arrondi, ani_date_mort_arrondi, ani_date_mort_text, ani_poids_mort, ani_poids_mort_na, ani_remarque, ani_mort_x, ani_mort_y, ani_inconnu, ani_cause_mort_classes, ani_necropsie, ani_crane, ani_lpa_mort, ani_remarque_suivi, cap_id, cap_faon, cap_age, cap_age_corrige, cap_circou, cap_etat_sante, cap_heure_lacher, cap_proximite_contact, cap_pertinent, cap_num_sabot, cap_age_faon, cap_telemetrie, cap_date_fin_capteur, cap_ect_id, cap_ecl_id, sit_id, sen_association, eqc_couleur_boitier, eqc_couleur_collier, eqc_memoire, eqc_remarque, cpt_nom_capteur
FROM public.v_individus_total
where concat(ani_id, cap_date) in (
  select concat(ani_id, max(cap_date))
  FROM public.v_individus_total where  cap_annee_suivi = ",annee_suivi," and eqa_date_fin is null and cap_age_classe = 'faon' and teq_nom_court = 'VHF' group by ani_id)
")))

loc<-utf8(dbGetQuery(serveur,
paste0("
  select lfa_ani_id, lfa_date, lfa_time, lfa_x_coord, lfa_y_coord from public.t_locfaons_lfa
  where lfa_id in ( select s.lfa_id FROM
                    (SELECT max(lfa_id) lfa_id, lfa_ani_id,  max(concat(lfa_date,' ', lfa_time))
                      FROM public.t_locfaons_lfa  where lfa_ani_id in ",v2dbn(dat[,"ani_id"])," group by lfa_ani_id) s)")))

if (dim(dat)[1] != dim(loc)[1]) {for (i in 1:100) {cat(red("ATTENTION: les deux objets n'ont pas la même taille\n"))}}

dat<-merge(dat,loc, by.x= "ani_id", by.y = "lfa_ani_id")

#setwd("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Programmes/R/google_drive/DATA")
setwd("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/DonneesBrutes")
drive_download(as_id(drive_find(pattern = "suivi_adultes.geojson")$id), overwrite = TRUE)
2
#########################################avec des shape files #########################################
# drive_download(as_id(drive_find(pattern = "saisie_localisation_faons.geojson")$id), overwrite = TRUE)
# drive_download(as_id(drive_find(pattern = "localisations_faons.shx")$id), overwrite = TRUE)
# drive_download(as_id(drive_find(pattern = "localisations_faons.cpg")$id), overwrite = TRUE)
# drive_download(as_id(drive_find(pattern = "localisations_faons.proj")$id), overwrite = TRUE)
# drive_download(as_id(drive_find(pattern = "localisations_faons.qpj")$id), overwrite = TRUE)
# drive_download(as_id(drive_find(pattern = "localisation_faons.shp")$id), overwrite = TRUE)
# drive_download(as_id(drive_find(pattern = "localisation_faons.shx")$id), overwrite = TRUE)
###########################################avec sp##########################################
#dat <- geojsonio::geojson_read("saisie_localisation_faons.geojson", what = "sp",  stringsAsFactors = TRUE)
####
datad<-st_read("suivi_adultes.geojson")

datad<-as.data.frame(datad)

####problème avec les valeur nulles
datt<-dat[,c("eqc_memoire","teq_nom_court", "ani_etiq","eqc_couleur_boitier","eqc_couleur_collier","ani_id","ani_name")]
names(datt)<-c("Memoire","Type_collier","Animal","Boitier","Collier","ani_id","nom")

datt[,"Remarque"] <- rep("",dim(dat)[1])
datt[,"Date_dernier_contact"]<-week(dat[,"lfa_date"])
datt[,"Alarme_GPS"]<- rep(0,dim(dat)[1])
datt[,"Alarme_mortalite"]<- rep(0,dim(dat)[1])
datt[,"Alarme_intermittente"] <- rep(0,dim(dat)[1])
datt[,"Hors_service"]<- rep(0,dim(dat)[1])
datt[,"Cause_fin_suivi"]<- rep("",dim(dat)[1])
datt[,"Date_fin_suivi"]<- rep(NA,dim(datt)[1])
datt[,"Date_fin_suivi_text"] <- rep("",dim(dat)[1])
datt[,"Date_mort"] <- rep(NA,dim(dat)[1])
datt[,"Date_mort_text"] <- rep("",dim(dat)[1])
datt[,"Cause_mort"] <- rep("",dim(dat)[1])
datt[,"Cause_mort_classe"]<- rep("",dim(dat)[1])
datt[,"Pds_mort"]<- rep("",dim(dat)[1])
datt[,"Lpa_mort"]<- rep("",dim(dat)[1])
datt[,"Congel"]<- rep(0,dim(dat)[1])
datt[,"lfa_x_coord"] <- dat[,"lfa_x_coord"]
datt[,"lfa_y_coord"] <- dat[,"lfa_y_coord"]

datt<-st_as_sf(datt, coords = c("lfa_x_coord","lfa_y_coord"), crs = 2154)

#datt<-datt[,-grep("lfa_x_coord",names(datt))]
#datt<-datt[,-grep("lfa_y_coord",names(datt))]

dat<-rbind(datad,datt)
st_write(dat, dsn = "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_saisie_tablette/suivi_adultes.geojson", layer = "suivi_adultes", driver = "GeoJSON", append = FALSE)
