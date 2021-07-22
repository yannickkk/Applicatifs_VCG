#------------------------------------TITRE--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  15/02/2021
#  Description: generer un geojson pour l'application suivi_adultes de la tablette a partir des donnees de la bd_chevreuils contenue sur le raspberry
#  Documentation:
#
#
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------
mypackages<-c("RPostgreSQL", "data.table","lubridate","crayon","rgdal","sf")
for (p in mypackages){
if(!require(p, character.only = TRUE)){
install.packages(p)
library(p, character.only = TRUE)
}
}
#-----------------------------------------------------------------------------
#-------------------------- connection aux bases de donnees ------------------
source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R")
source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
#-------------------------- chargement de mes fonctions ----------------------
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")

annee<-2021
memoires_faons_n_1<-v2dbn(c(32,119,40,31)) ####mémoire des colliers faons de l'annee d'avant
memoires_faons_n_2<-v2dbn(c(40,31)) ####mémoire des colliers faons de deux ans avant
ani_etiq<-v2db(c("F1349")) ####collier vhf ou GPS non récupérés à continuer à suivre


utf8 <- function(x) {
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
  # Same on column names:
  Encoding(names(x)) <- "UTF-8"
  return(x)
}


#############selection des individus à suivre
dato<- utf8(dbGetQuery(raspi, paste0("SELECT distinct ON (ani_etiq) * FROM public.v_individus_total where eqc_memoire in ",memoires_faons_n_1," and cap_annee_suivi = ",annee-1," or  eqc_memoire in ",memoires_faons_n_2," and cap_annee_suivi = ",annee-2," or ani_etiq in ",ani_etiq," order by ani_etiq, cap_id DESC;")))
data<- utf8(dbGetQuery(raspi, paste0("SELECT * FROM public.v_individus_total where cap_annee_suivi = ",annee," and ani_mortalite = FALSE ;"))) ##and ani_etiq != '3141'

dat<-rbind(data,dato)

dbDisconnect(raspi)
#fwrite(dat, file = "C:/Users/ychaval/Documents/BD_tools/saisie_capture_shiny/DonneesBrutes/suivi.csv")

######recuperation des coordonnees de secteur
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

coordinates(qfield) = c("x", "y")
dir.create(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/suivi_adultes/",year(Sys.Date()),""))
rawPath<-"C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/suivi_adultes/"
dataPath<-paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/suivi_adultes/",year(Sys.Date()),"/")
files<-grep("suivi_adultes",list.files(rawPath,include.dirs = FALSE), value =TRUE)
file.copy(paste(rawPath, files, sep = .Platform$file.sep), dataPath, overwrite = TRUE)
setwd(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/suivi_adultes/",year(Sys.Date()),""))
writeOGR(qfield, dsn=paste0(dataPath,"suivi_adultes.geojson"), layer="suivi_adultes", driver="GeoJSON", check_exists=TRUE, overwrite_layer= TRUE, delete_dsn=TRUE, encoding="UTF-8" )
writeOGR(qfield, dsn="suivi_adultes.geojson", layer="suivi_adultes", driver="GeoJSON", check_exists=TRUE, overwrite_layer= TRUE, delete_dsn=TRUE, encoding="UTF-8" )
