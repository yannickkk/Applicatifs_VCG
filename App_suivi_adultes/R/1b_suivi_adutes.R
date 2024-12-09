#------------------------------------1b_suivi_adultes--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  06/08/2021
#  Description: veriante de 1_suivi_adultes.R qui ajoute les nouvelles captures au geojson de suivi des adultes 
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

#######on réccupères tous les animaux déjà capturés
dat<-utf8(dbGetQuery(serveur,paste0("
select
ani_id, ani_etiq, ani_name, rfi_tag_code, cap_bague, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal, cap_annee_suivi, cap_date, ani_mere_observee, ani_mere_pedigree, ani_pere_pedigree, ani_fratrie, ani_fratrie_oct, ani_surv_faon_marque_oct, ani_agres_faon_marque, ani_sexe, cap_age_classe, cap_poids, cap_lpa, sit_nom_court, teq_nom_court, eqt_id_usuel, mar_libelle, mod_libelle, eqa_date_debut, eqa_date_fin, eqa_date_fin_suivi, eqa_date_fin_suivi_text, eqa_cause_fin_suivi, eqa_dernier_contact, ani_mortalite, ani_date_mort, ani_cause_mort, eqa_activite, eqa_probleme, eqa_date_fin_text, eqa_date_fin_arrondi, ani_date_mort_arrondi, ani_date_mort_text, ani_poids_mort, ani_poids_mort_na, ani_remarque, ani_mort_x, ani_mort_y, ani_inconnu, ani_cause_mort_classes, ani_necropsie, ani_crane, ani_lpa_mort, ani_remarque_suivi, cap_id, cap_faon, cap_age, cap_age_corrige, cap_circou, cap_etat_sante, cap_heure_lacher, cap_proximite_contact, cap_pertinent, cap_num_sabot, cap_age_faon, cap_telemetrie, cap_date_fin_capteur, cap_ect_id, cap_ecl_id, sit_id, sen_association, eqc_couleur_boitier, eqc_couleur_collier, eqc_memoire, eqc_remarque, cpt_nom_capteur
FROM public.v_individus_total
where concat(ani_id, cap_date) in (select concat(ani_id, max(cap_date))  
FROM public.v_individus_total where  cap_annee_suivi in (",annee_suivi,")
and cap_age_classe != 'faon'
and ani_date_mort is null
and teq_nom_court is not null
group by ani_id)
")))


source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
cent<-dbGetQuery(serveur,"SELECT id, secteur, ST_x(ST_centroid(geom)) x, ST_y(ST_centroid(geom)) y  FROM env_data.zones_captures_hivernales")

# if (length(grep("suivi_adultes.geojson",list.files("C:/Users/ychaval/Documents/BD_tools/saisie_capture_shiny/Outputs/"))) != 0) {
# datt<-read_sf("C:/Users/ychaval/Documents/BD_tools/saisie_capture_shiny/Outputs/suivi_adultes.geojson", as_tibble =FALSE)
# #dat<-sf::st_transform(dat,wgs84)
# #dat<-as.data.frame(dat)
# }

########générer le champ telemetrie
tele <-NA
counn<-NA
dat[,"cap_age_classe"]<-trimws(dat[,"cap_age_classe"])
for (i in 1:dim(dat)[1]){
  #☻if (i == 18 | i == 26) {dat[i,"cap_age_classe"] <- "adulte"}
  cap_annee_suivi<-dat[i,"cap_annee_suivi"]
  annee<- year(Sys.Date())
  dif<-annee-cap_annee_suivi
  clas<-dat[i,"cap_age_classe"]
  classes<-data.frame(c(1:4),c("faon","jeune","yearling","adulte"))
  if (dif < 4 & !is.na(dat[i,"cap_age_classe"])) {if (match(clas, classes[,2])+dif > 4) {dat[i,"cap_age_classe"]<- "adulte" }else{dat[i,"cap_age_classe"]<-as.character(classes[match(clas, classes[,2])+dif,2])}}
  if (dat[i,"cap_age_classe"] == "jeune") {
    cat_age_all <- "jeune" 
    cat_age <- "j"}
  if (dat[i,"cap_age_classe"] == "faon") {
    cat_age_all <- "faon" 
    cat_age <- ""}
  if (dat[i,"cap_age_classe"] == "yearling") {
    cat_age_all <- "yearling"
    cat_age <- "y" }
  if (dat[i,"cap_age_classe"] == "adulte") {
    cat_age_all="adulte"
    cat_age=""}
  if (cat_age != "" & cat_age_all != "faon") { tel<-paste0(tolower(dat[i,"ani_sexe"]),cat_age,"_",dat[i,"ani_etiq"])} else if (cat_age_all == "faon" & cap_annee_suivi == year(Sys.Date())-1) {tel<-paste0(tolower(dat[i,"ani_sexe"]),"j_",dat[i,"ani_etiq"]) } else if (cat_age_all == "faon") {tel<- paste0(tolower(dat[i,"ani_sexe"]),"_",dat[i,"ani_etiq"])} else {tel<-paste0(tolower(dat[i,"ani_sexe"]),"_",dat[i,"ani_etiq"])}
  tele <- append(tele,tel)
}
tele<-tele[2:length(tele)]
dat[,"telemetrie"]<- tele

###############générer les codes collier

dat<-apply(dat,2,as.character)
dat[dat[,"sen_association"] == "rien","sen_association"] <- NA
dat[dat[,"sen_association"] == "activite","sen_association"] <- NA
dat[dat[,"sen_association"] == "activite_proximite","sen_association"] <-  "prox"	
dat[dat[,"sen_association"] == "activite_proximite_accelerometre","sen_association"] <-  "prox_acc"	
dat[dat[,"sen_association"] == "activite_proximite_accelerometre_magnetometre","sen_association"] <-  "prox_acc_magne"	
dat[dat[,"sen_association"] == "proximite","sen_association"] <-  "prox"	
dat[dat[,"sen_association"] == "proximite_accelerometre","sen_association"] <-  "prox_acc"
dat[dat[,"sen_association"] == "proximite_accelerometre_magnetometre","sen_association"] <-  "prox_acc_magne"	
dat[dat[,"sen_association"] == "accelerometre","sen_association"] <-  "acc"	
dat[dat[,"sen_association"] == "accelerometre_magnetometre","sen_association"] <-  "acc_magne"	
dat[dat[,"sen_association"] == "activite_accelerometre","sen_association"] <-  "acc"

dat[is.na(dat[,"sen_association"]),"sen_association"]<-""
dat<-as.data.frame(dat)
dat[,"Type_collier"] <-paste0(dat[,"teq_nom_court"],dat[,"sen_association"])

qfield<-dat[,c("eqc_memoire","Type_collier","telemetrie","eqc_couleur_collier", "eqc_couleur_boitier","ani_id","ani_name","cap_etat_sante","sit_nom_court")]
names(qfield)<-c("Memoire","Type_collier","Animal","Collier","Boitier","ani_id","nom","Remarque","sit_nom_court")
qfield<-as.data.frame(qfield)
qfield[,c("Date_dernier_contact","Alarme_GPS","Alarme_mortalite","Alarme_intermittente","Hors_service","Cause_fin_suivi","Date_fin_suivi","Date_fin_suivi_text","Date_mort","Date_mort_text","Cause_mort","Cause_mort_classe","Pds_mort","Lpa_mort","Congel")]<-NA

qfield<-qfield[order(qfield$sit_nom_court),]

result<-qfield[0,]
for (j in unique(qfield[,"sit_nom_court"])){
  qfieldi<- subset(qfield,qfield[,"sit_nom_court"] == j)
  for (i in 1:dim(qfieldi)[1]) { 
    qfieldi[i,"x"]<- cent[which(cent[,"secteur"] == qfieldi[i,"sit_nom_court"]),"x"]
    qfieldi[i,"y"]<- cent[which(cent[,"secteur"] == qfieldi[i,"sit_nom_court"]),"y"] - 400 + 100 * i #-800 + 80 *
  }
  result<-rbind(result, qfieldi)  
}
qfield<-result
qfield<-qfield[,c("Memoire","Type_collier","Animal","Collier","Boitier","ani_id","nom","Remarque","Date_dernier_contact","Alarme_GPS","Alarme_mortalite","Alarme_intermittente","Hors_service","Cause_fin_suivi","Date_fin_suivi","Date_fin_suivi_text","Date_mort","Date_mort_text","Cause_mort","Cause_mort_classe","Pds_mort","Lpa_mort","Congel","x","y")]
qfield[,"Date_dernier_contact"]<-week(Sys.Date())

####je renomme les lignes pour que dans l'app Qfield on puisse rechercher les individus par mémoire (l'appli zoom alors sur l'individus recherché)
row.names(qfield)<-wsr(qfield[,"Memoire"])

coordinates(qfield) = c("x", "y")

#######on reccupère dans le geojson de l'année en cours les animaux déjà présents
setwd(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/suivi_adultes/",year(Sys.Date()),""))

dat<-st_read("suivi_adultes.geojson")

dat<-as.data.frame(dat)

####récupération des individus de la base qui ne sont pas présent dans le fihier geojson
qfield<-qfield[is.na(match(dat$ani_id, as.numeric(qfield$ani_id))),]

qfied<-rbind(dat,qfield)

dir.create(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/suivi_adultes/",year(Sys.Date()),""))
rawPath<-"C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/suivi_adultes/"
dataPath<-paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/suivi_adultes/",year(Sys.Date()),"/")
files<-grep("suivi_adultes",list.files(rawPath,include.dirs = FALSE), value =TRUE)
file.copy(paste(rawPath, files, sep = .Platform$file.sep), dataPath, overwrite = TRUE)
setwd(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/suivi_adultes/",year(Sys.Date()),""))
writeOGR(qfield, dsn=paste0(dataPath,"suivi_adultes.geojson"), layer="suivi_adultes", driver="GeoJSON", check_exists=TRUE, overwrite_layer= TRUE, delete_dsn=TRUE, encoding="UTF-8" )
writeOGR(qfield, dsn="suivi_adultes.geojson", layer="suivi_adultes", driver="GeoJSON", check_exists=TRUE, overwrite_layer= TRUE, delete_dsn=TRUE, encoding="UTF-8" )

