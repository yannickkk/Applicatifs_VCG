#------------------------------------TITRE--------------------------------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  23/07/2021
#  Description: mise en forme des fichiers de loc et d'activité pour l'import des données
#  Documentation:
#  ###Si un animal est équipé deux fois dans la même année nommé le second fichier ani_etiq_yy_b
#  regarder les types lotek et vectronics sur les fichier de 2020 reprendre le même format de fichier.
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------
mypackages<-c("lubridate", "magrittr", "sf","RPostgreSQL","data.table", "foreign", "crayon", "chron")
for (p in mypackages){
if(!require(p, character.only = TRUE)){
install.packages(p)
library(p, character.only = TRUE)
}
}
#-------------------------- chargement de mes fonctions ----------------------
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")
rm(serveur)
#-----------------------------------------------------------------------------
#-------------------------- connection aux bases de donnees ------------------
#source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R")
#source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R"))
source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_CEFS/con_localhost_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/con_serveur_dbgardouch.R")
#serveur<-local


defaultW <- getOption("warn") 

logpath<-"C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Programmes/R/log.R"
logpath2<-"C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Programmes/R/log_date_fin_suivi_manquante.R"
##############fonction qui genere le numero de telemetrie 
telemet<-function(eqt_id_usuel, ani_etiq){
  tele <-NA
  counn<-NA
  if(!exists("annee")){annee<- year(Sys.Date())}
  dat<-dbGetQuery(serveur,paste0("Select ani_etiq, ani_sexe, cap_age_classe, eqt_id_usuel, mar_libelle, mod_libelle, cap_annee_suivi from public.v_individus_total where eqt_id_usuel = '",eqt_id_usuel,"' and cap_annee_suivi = '",annee,"' "))
  if (length(dat$ani_etiq) != 1) {if (!is.na(ani_etiq)) {dat<-dat[grep(ani_etiq,dat$ani_etiq),]} else {cat(bgRed(black(rq<-"Le collier ",eqt_id_usuel," a été posé ",length(dat$ani_etiq)," fois pour l'année ",annee," les fichiers de données doivent être placés dans ..\\DonneesBrutes\\",annee,"\\ani_etiq pour pouvoir être interprétés")))}}
  dat[,"cap_age_classe"]<-trimws(dat[,"cap_age_classe"])
  cap_annee_suivi<-dat[,"cap_annee_suivi"]
  dif<-as.numeric(annee)-as.numeric(cap_annee_suivi)
  clas<-dat[,"cap_age_classe"]
  classes<-data.frame(c(1:4),c("faon","jeune","yearling","adulte"))
  if (dif < 4 & !is.na(dat[,"cap_age_classe"])) {if (match(clas, classes[,2])+dif > 4) {dat[,"cap_age_classe"]<- "adulte" }else{dat[,"cap_age_classe"]<-as.character(classes[match(clas, classes[,2])+dif,2])}}
  if (dat[,"cap_age_classe"] == "jeune") {
    cat_age_all <- "jeune" 
    cat_age <- "J"}
  if (dat[,"cap_age_classe"] == "faon") {
    cat_age_all <- "faon" 
    cat_age <- ""}
  if (dat[,"cap_age_classe"] == "yearling") {
    cat_age_all <- "yearling"
    cat_age <- "Y" }
  if (dat[,"cap_age_classe"] == "adulte") {
    cat_age_all="adulte"
    cat_age=""}
  if (cat_age != "" & cat_age_all != "faon") { tel<-paste0(toupper(dat[,"ani_sexe"]),cat_age,"_",dat[,"ani_etiq"])} else if (cat_age_all == "faon" & cap_annee_suivi == year(Sys.Date())-1) {tel<-paste0(toupper(dat[,"ani_sexe"]),"J_",dat[,"ani_etiq"]) } else if (cat_age_all == "faon") {tel<- paste0(toupper(dat[,"ani_sexe"]),"_",dat[,"ani_etiq"])} else {tel<-paste0(toupper(dat[,"ani_sexe"]),"_",dat[,"ani_etiq"])}
  tele <- append(tele,tel)
  tele<-tele[2:length(tele)]
  dat[,"telemetrie"]<- tele
  return(dat)
}

annee<-2021
h<-now()

#options(warn = -1)

if(!dir.exists(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Docs/",annee,""))){
  dir.create(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Docs/",annee,""))
}

if(!dir.exists(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/DonneesBrutes/",annee,""))){
  dir.create(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/DonneesBrutes/",annee,""))
}

if(!dir.exists(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/DonneesTravaillees/",annee,""))){
  dir.create(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/DonneesTravaillees/",annee,""))
}

if(!dir.exists(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Outputs/",annee,""))){
  dir.create(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Outputs/",annee,""))
}

if(!dir.exists(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Programmes/",annee,""))){
  dir.create(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Programmes/",annee,""))
}
########placer tous les fichiers dans le dossier nouvellement créé

path<-paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/DonneesTravaillees/",annee,"")

setwd(path)

path_global<-file.path(path, paste0("bd_",substr(annee,3,4),"a"))

if(!dir.exists(paste0(path_global))){
dir.create(path_global)
}

path_global_gps_vectro<-"/gps_vectro_12"
path_global_gsm_vectro<-"/gsm_vectro"
path_global_iridium_vectro<-"/iridium_vectro"
path_global_gsm_lotek<-"/gsm_lotek"

if(!dir.exists(paste0(path_global,path_global_gps_vectro))){
dir.create(paste0(path_global,path_global_gps_vectro))
dir.create(paste0(path_global,path_global_gps_vectro,"/sensor"))
}
if(!dir.exists(paste0(path_global,path_global_gsm_vectro))){
dir.create(paste0(path_global,path_global_gsm_vectro))
dir.create(paste0(path_global,path_global_gsm_vectro,"/sensor"))
}
if(!dir.exists(paste0(path_global,path_global_gsm_lotek))){
dir.create(paste0(path_global,path_global_gsm_lotek))
dir.create(paste0(path_global,path_global_gsm_lotek,"/sensor"))
}
if(!dir.exists(paste0(path_global,path_global_iridium_vectro))){
  dir.create(paste0(path_global,path_global_iridium_vectro))
  dir.create(paste0(path_global,path_global_iridium_vectro,"/sensor"))
}

setwd(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/DonneesBrutes/",annee,""))

###lotek: GSM
#GPS: ANIMAL	NUM	LINE_NO	GMT_DATE	GMT_TIME	LATITUDE	LONGITUDE	HEIGHT	ALTCOR2	DOP	DOPCOR	NAV	VALIDATED	SATS_USED	TEMP	XL3	YL3	XL3C	YL3C	DELTA	INDEXE	DELTAT
#activité: GMT_DATE	GMT_TIME	LMT_DATE	LMT_TIME	ACTIVITY_X	ACTIVITY_Y	TEMP

###Vectronic: GPS
#gps: ANIMAL	NO	NUM	COLLARID	GMT_DATE	GMT_TIME	LATITUDE	LONGITUDE	HEIGHT	DOP	FIXTYPE	SATS_USED	XL3	YL3	TEMP
#activité: NO	CollarID	UTC_DATE	UTC_TIME	LMT_DATE	LMT_TIME	ACT_MODE	DT	ActivityX	ActivityY	ActivityZ	TEMP	AnimalID	GroupID

###Vectronic: GSM
#gps ANIMAL	NO	NUM	COLLARID	GMT_DATE	GMT_TIME	LATITUDE	LONGITUDE	HEIGHT	DOP	FIXTYPE	SATS_USED	XL3	YL3	TEMP
#activité: NO	CollarID	UTC_DATE	UTC_TIME	LMT_DATE	LMT_TIME	ACT_MODE	DT	ActivityX	ActivityY	ActivityZ	TEMP	AnimalID	GroupID


#list_files_activity<-list.files()[grep(".DBF",list.files())][grep("ACT_",list.files()[grep(".DBF",list.files())])]
#list_files_GPS<-list.files()[grep(".DBF",list.files())][grep("GPS_",list.files()[grep(".DBF",list.files())])]

dbf_files<-list.files(recursive = TRUE)[grep(".DBF",toupper(list.files(recursive = TRUE)))]

#####Je mets tout les GPS en premier de façon à ce que la eqa_ate_fin soit renseigné pour chaque individu. Ainsi je peux recouper l'activité sur al date de fin de GPS
dbf_files<- append(dbf_files[grep("GPS", dbf_files)],dbf_files[grep("ACT", dbf_files)])

######On met l'activité à false et on va la mettre à TRUE à chaque fois qu'un fichier d'activité est traité
dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_activite = FALSE WHERE eqa_annee_suivi = ",annee,""))

#initialisation du df de collecte des heures de fin des GPS pour tronquer l''activité
maxndbf<-data.frame(ani_etiq = NA,eqt_id_usuel = NA,date_heure_fin =NA)

for (i in 1:length(dbf_files)){
  
  cat(i)
  fil<- dbf_files[i]
  if (length(strsplit(fil,"/")[[1]]) != 1) {filname<-strsplit(fil,"/")[[1]][length(strsplit(fil,"/")[[1]])]} else {filname<-fil}
  if (length(strsplit(fil,"/")[[1]]) != 1) {ani_etiq<-strsplit(fil,"/")[[1]][length(strsplit(fil,"/")[[1]])-1]} else {ani_etiq<- NA}
  eqt_id_usuel<- gsub("Collar","",unlist(lapply(str_split(fil,"_"),"[[",2)))
  
  #####reccuperation des donnees d'association du collier pour l'annee consideree ani_etiq ani_sexe cap_age_classe eqt_id_usuel mar_libelle mod_libelle cap_annee_suivi telemetrie
  dat<- telemet(eqt_id_usuel, ani_etiq)
  
  #nom_GPS<- paste0(dat[,"telemetrie"],"_",substr(annee,3,4),"BDN.DBF")
   nom_GPS<- paste0(dat[,"ani_etiq"],"_",substr(annee,3,4),".DBF")
   
  ######voir la réponse de A.benard pour choisir la bonne terminaison
  #nom_ACT<- paste0("s",tolower(dat[,"telemetrie"]),"_20n.DBF")
   nom_ACT<- paste0("s_",toupper(dat[,"ani_etiq"]),"_",substr(annee,3,4),".DBF")
   
  if(!exists("annee")){annee<- year(Sys.Date())}
  
  if (!is.na(ani_etiq)){
  datt<- dbGetQuery(serveur, paste0("SELECT ani_id, ani_etiq, eqt_id_usuel, cap_annee_suivi, 
        eqa_date_debut,eqa_date_fin,eqa_date_fin_suivi,eqa_date_fin_suivi_text,eqa_cause_fin_suivi,eqa_dernier_contact,
        eqa_activite,eqa_date_fin_activite,eqa_probleme,eqa_date_fin_text,eqa_date_fin_arrondi,eqa_remarque_suivi, sen_association, cpt_heure_lache,cpt_heure_second_lache 
        from public.v_individus_total where ani_etiq = '",ani_etiq,"' and eqt_id_usuel = '",eqt_id_usuel,"' and cap_annee_suivi = '",annee,"' "))
  } else {
    datt<- dbGetQuery(serveur, paste0("SELECT ani_id, ani_etiq, eqt_id_usuel, cap_annee_suivi, 
        eqa_date_debut,eqa_date_fin,eqa_date_fin_suivi,eqa_date_fin_suivi_text,eqa_cause_fin_suivi,eqa_dernier_contact,
        eqa_activite,eqa_date_fin_activite,eqa_probleme,eqa_date_fin_text,eqa_date_fin_arrondi,eqa_remarque_suivi, sen_association, cpt_heure_lache,cpt_heure_second_lache 
        from public.v_individus_total where eqt_id_usuel = '",eqt_id_usuel,"' and cap_annee_suivi = '",annee,"' "))
  }
  
  if (dim(datt)[1] !=1) {cat(bgRed(black(rq<-"Le collier ",eqt_id_usuel," a été posé ",length(dat$ani_etiq)," fois pour l'année ",annee," les fichiers de données doivent être placés dans ..\\DonneesBrutes\\",annee,"\\ani_etiq pour pouvoir être interprétés")));source(logpath)}

  if (is.na(datt[,"eqa_date_fin_suivi"])) {cat(bgRed(white(pas_date_fin_suivi<-"",dat$ani_etiq," équipé du collier ",eqt_id_usuel," n'a pas de date de fin de suivi pour l'année ",annee," vérifier qu'il est bien toujours suivi en  ",annee+1," dans l'appli suivi_adultes ou voir avec l'équipe terrain pourquoi il n'y  pas cette date")));source(logpath2)};
  
  #########################################################GPS LOTEK PLUS UTILISES #####################################
  if (length(grep("LOTEK",toupper(dat[,"mar_libelle"]))) != 0){
    if (length(grep("GPS",dat[,"mod_libelle"]))!= 0){
      if (length(grep("ACT",fil))!= 0){
        cat(bgRed(black(rq<-"Ce type de fichier activité n'est plus censé exister")));source(logpath);break
        # file.copy(from = file.path(),
        #           to   = "path_to_move_to")
        # file.rename()
      } else {
        cat(bgRed(black(rq<-"Ce type de fichier GPS n'est plus censé exister")));source(logpath);break
        # file.copy(from = file.path(),
        #           to   = "path_to_move_to")
        # file.rename()
      }
    }
  }
  
#########################################################GSM LOTEK #####################################
  if (length(grep("LOTEK",toupper(dat[,"mar_libelle"]))) != 0){
    if (length(grep("GSM",dat[,"mod_libelle"])) != 0){
      if (length(grep("ACT",fil)) != 0){
        #####on deplace le fichier
        file.copy(from = paste0(getwd(),"/",fil),
                  to   = paste0(path_global,path_global_gsm_lotek,"/sensor/",filname))
        #######On tronque le fichier sur la date de capture et > heure de lache        
        ndbf<-read.dbf(paste0(path_global,path_global_gsm_lotek,"/sensor/",filname))
        names(ndbf) <-toupper(names(ndbf))
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
        
        if(is.na(datt[,"cpt_heure_lache"])) {datt[,"cpt_heure_lache"]<-datt[,"cpt_heure_second_lache"]}
        t_db<-as.POSIXct(paste(datt[,"eqa_date_debut"],datt[,"cpt_heure_lache"]),format="%Y-%m-%d %H:%M:%S")
        attr(t_db, "tzone") <- "UTC"
        ndbf<- ndbf[which(as.POSIXct(paste(ndbf[,"UTC_DATE"],ndbf[,"UTC_TIME"]),format="%Y-%m-%d %H:%M:%S", tz="UTC") >= t_db) ,]
        #####on tronque la date et heure de fin sur celle du GPS correspondant
        date_heure_fin<-as.POSIXct(maxndbf[which(maxndbf[,"ani_etiq"] == datt[,"ani_etiq"] & maxndbf[,"eqt_id_usuel"] == datt[,"eqt_id_usuel"]),"date_heure_fin"],format="%Y-%m-%d %H:%M:%S", tz="UTC")
        ndbf<- ndbf[which(as.POSIXct(paste(ndbf[,"UTC_DATE"],ndbf[,"UTC_TIME"]),format="%Y-%m-%d %H:%M:%S", tz="UTC") <= date_heure_fin) ,]

        if (dim(ndbf)[1] == 0) {cat(bgRed(black(rq<-paste0("Les données d'activité du fichier ",fil," sont en dehors de la plage de déploiement !!"))));source(logpath);file.remove(paste0(path_global,path_global_gsm_lotek,"/sensor/",filname))} else {

        
          names(ndbf)<-gsub("UTC_","GMT_",names(ndbf))

          ndbf<-ndbf[,c("GMT_DATE","GMT_TIME","LMT_DATE","LMT_TIME","ACTIVITY_X","ACTIVITY_Y","TEMP")]
          if (dim(ndbf)[2] != 7) {cat(bgRed(white(format<-"problème de format de fichier pour l'animal",dat$ani_etiq," équipé du collier ",eqt_id_usuel," ")));source(logpath2)};
        ###############################################################################################################
         
        #######On enregistre le fichier tronque 
        write.dbf(ndbf, paste0(path_global,path_global_gsm_lotek,"/sensor/",filname))
        file.rename(paste0(path_global,path_global_gsm_lotek,"/sensor/",filname),paste0(path_global,path_global_gsm_lotek,"/sensor/",nom_ACT))
        #######On met a jour la bd_cefs pour la date de fin de collier
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin_activite = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"', eqa_activite = TRUE
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   "))
          }} else {
        #####on deplace le fichier
        file.copy(from = paste0(getwd(),"/",fil),
                  to   = paste0(path_global,path_global_gsm_lotek,"/",filname))
        ndbf<-read.dbf(paste0(path_global,path_global_gsm_lotek,"/",filname))
        ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
        #######On rajoute les colonnes ANIMAL et NUM
        ndbf<-cbind(ANIMAL = rep(tolower(dat[,"telemetrie"]) ,dim(read.dbf(paste0(path_global,path_global_gsm_lotek,"/",filname)))[1]),NUM =paste0("a",read.dbf(paste0(path_global,path_global_gsm_lotek,"/",filname))[,1]),read.dbf(paste0(path_global,path_global_gsm_lotek,"/",filname)))
        names(ndbf)[grep(".N",names(ndbf),fixed = TRUE)]<-gsub("_C.N","_C\\/N",names(ndbf)[grep(".N",names(ndbf),fixed = TRUE)])
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
        names(ndbf) <-toupper(names(ndbf))
        ######en fonction des données brutes lotek, selectionner les champs pour arriver à (ANIMAL et Num est calculé après):
        ###lotek: GSM Bruno
        #GPS: ANIMAL	NUM	LINE_NO	GMT_DATE	GMT_TIME	LATITUDE	LONGITUDE	HEIGHT	ALTCOR2	DOP	DOPCOR	NAV	VALIDATED	SATS_USED	TEMP	XL3	YL3	XL3C	YL3C	DELTA	INDEXE	DELTAT
        #activité: GMT_DATE	GMT_TIME	LMT_DATE	LMT_TIME	ACTIVITY_X	ACTIVITY_Y	TEMP
        
        #ndbf<-ndbf[,c("ANIMAL","NUM","LINE_NO","GMT_DATE","GMT_TIME","LATITUDE","LONGITUDE","HEIGHT","DOP","NAV","VALIDATED","SATS_USED","TEMP")]
        #if (dim(ndbf)[2] != 13) {cat(bgRed(white(format<-"problème de format de fichier pour l'animal",dat$ani_etiq," équipé du collier ",eqt_id_usuel," ")));source(logpath2)};
        
        #######On tronque le fichier sur la date de capture et > heure de lache
        if(is.na(datt[,"cpt_heure_lache"])) {datt[,"cpt_heure_lache"]<-datt[,"cpt_heure_second_lache"]} 
        if(is.na(datt[,"cpt_heure_lache"])) {cat(bgRed(black(rq<-"pas d'heure de lâché pour ",datt[,"ani_eriq"],"")))}
        #####pas de loc avant la date et l'heure de lacher
        ndbf<- ndbf[which(
          as.POSIXct(paste0(ndbf[,"UTC_DATE"]," ",ndbf[,"UTC_TIME"]), format="%Y-%m-%d %H:%M:%S" ,tz="UTC")>=
            as.POSIXct(paste0(datt[,"eqa_date_debut"]," ",datt[,"cpt_heure_lache"], format="%Y-%m-%d %H:%M:%S" ,tz="CET"))),]
        ####si la cause de fin de suivi est drop off activé, Collier au sol, recapture, pas de loc après la date de fin de suivi
        if (!is.na(datt[,"eqa_date_fin_suivi"]) & grepl("activ|sol$|recapture|^drop",datt[,"eqa_cause_fin_suivi"])) {
          ndbf<- ndbf[which(
            as.Date(ndbf[,"UTC_DATE"], format="%Y-%m-%d")<=
              as.Date(paste0(datt[,"eqa_date_fin_suivi"], format="%Y-%m-%d"))),]
        }
        if (dim(ndbf)[1] == 0) {cat(bgRed(black(rq<-paste0("Les données GPS du fichier ",fil," sont en dehors de la plage de déploiement !!"))));source(logpath);file.remove(paste0(path_global,path_global_gsm_lotek,"/",fil))} else {
          #######je collecte la date et l'heure de fin pour pouvoir tronquer l'activité
          maxndbf<- rbind(maxndbf, cbind(ani_etiq = datt[,"ani_etiq"], eqt_id_usuel = datt[,"eqt_id_usuel"],date_heure_fin = as.character(max(as.POSIXct(paste0(ndbf[,"UTC_DATE"]," ",ndbf[,"UTC_TIME"]), format="%Y-%m-%d %H:%M:%S" ,tz="UTC")))))
   
        #################################################################A TERMINER ##############################################
          names(ndbf)<-gsub("UTC_","GMT_",names(ndbf))
          ######en fonction des données brutes lotek, selectionner les champ pour arriver à (ANIMAL ET NUM sont calculés plus haut):
          ###lotek: GSM Bruno
          #activité: GMT_DATE	GMT_TIME	LMT_DATE	LMT_TIME	ACTIVITY_X	ACTIVITY_Y	TEMP
          #ndbf<-ndbf[,c("ANIMAL","NUM","LINE_NO","GMT_DATE","GMT_TIME","LATITUDE","LONGITUDE","HEIGHT","DOP","NAV","VALIDATED","SATS_USED","TEMP")]
          ndbf<-ndbf[,c("ANIMAL","LINE_NO","GMT_DATE","GMT_TIME","LATITUDE","LONGITUDE","HEIGHT","DOP","NAV","VALIDATED","SATS_USED","TEMP")]
          
          #on teste que tous les champs sont bien sélectionnés
          if (dim(ndbf)[2] != 12) {cat(bgRed(white(format<-"problème de format de fichier pour l'animal",dat$ani_etiq," équipé du collier ",eqt_id_usuel," ")));source(logpath2)};
 
         ########################################################################################################################## 
          
          
        #######On enregistre le fichier tronque
          ndbf$LATITUDE<-as.factor(round(ndbf$LATITUDE,7))
          ndbf$LATITUDE<-as.character(ndbf$LATITUDE)
          ndbf$LATITUDE<-gsub("\\.",",",ndbf$LATITUDE)
          ndbf[which(is.na(ndbf$LATITUDE)),"LATITUDE"]<-rep("",length(ndbf[which(is.na(ndbf$LATITUDE)),"LATITUDE"]))
          ndbf$LATITUDE<-as.factor(ndbf$LATITUDE)
          ndbf$LONGITUDE<-as.factor(round(ndbf$LONGITUDE,7))
          ndbf$LONGITUDE<-as.character(ndbf$LONGITUDE)
          ndbf$LONGITUDE<-gsub("\\.",",",ndbf$LONGITUDE)
          ndbf[which(is.na(ndbf$LONGITUDE)),"LONGITUDE"]<-rep("",length(ndbf[which(is.na(ndbf$LONGITUDE)),"LONGITUDE"]))
          ndbf$LONGITUDE<-as.factor(ndbf$LONGITUDE)
          ndbf$HEIGHT<-as.factor(round(ndbf$HEIGHT,2))
          ndbf$HEIGHT<-as.character(ndbf$HEIGHT)
          ndbf$HEIGHT<-gsub("\\.",",",ndbf$HEIGHT)
          ndbf[which(is.na(ndbf$HEIGHT)),"HEIGHT"]<-rep("",length(ndbf[which(is.na(ndbf$HEIGHT)),"HEIGHT"]))
          ndbf$HEIGHT<-as.factor(ndbf$HEIGHT)
          ndbf$DOP<-as.factor(round(ndbf$DOP,1))
          ndbf$DOP<-as.character(ndbf$DOP)
          ndbf$DOP<-gsub("\\.",",",ndbf$DOP)
          ndbf[which(is.na(ndbf$DOP)),"DOP"]<-rep("",length(ndbf[which(is.na(ndbf$DOP)),"DOP"]))
          ndbf$DOP<-as.factor(ndbf$DOP)
          
        write.dbf(ndbf, paste0(path_global,path_global_gsm_lotek,"/",filname))
        #######On renomme
        file.rename(paste0(path_global,path_global_gsm_lotek,"/",filname),paste0(path_global,path_global_gsm_lotek,"/",nom_GPS))
        #######On met a jour la bd_cefs pour la date de fin de collier
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        if (!is.na(ymd(dbGetQuery(serveur,paste0("SELECT ani_date_mort FROM public.t_animal_ani where ani_id = ",datt[,"ani_id"],";"))[,1]))){
          if (ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]) > ymd(dbGetQuery(serveur,paste0("SELECT ani_date_mort FROM public.t_animal_ani where ani_id = ",datt[,"ani_id"],";"))[,1]))
          {cat(bgRed(black(rq<-"La date de fin d'équipement du collier est supérieure à la date de mort de l'animal")));source(logpath);file.remove(paste0(path_global,path_global_gsm_lotek,"/",fil))}else {
            dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"'
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   "))  
          }}else{
        dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"'
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   "))}
      }
     }
    }
  }
#########################################################GPS vECTRONIC ##################################### 
  if (length(grep("VECTRONIC",toupper(dat[,"mar_libelle"]))) != 0){
    if (length(grep("GPS",dat[,"mod_libelle"])) != 0){
      if (length(grep("ACT",fil)) != 0){
        file.copy(from = paste0(getwd(),"/",fil),
                  to   = paste0(path_global,path_global_gps_vectro,"/sensor/",filname))
        #file.rename(paste0(path_global,path_global_gps_vectro,"/sensor/",fil),paste0(path_global,path_global_gps_vectro,"/sensor/",nom_ACT))
        #date_act(eqt_id_usuel)
        #######On tronque le fichier sur la date de capture et > heure de lache        
        ndbf<-read.dbf(paste0(path_global,path_global_gps_vectro,"/sensor/",filname))
        names(ndbf) <-toupper(names(ndbf))
        ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        if(is.na(datt[,"cpt_heure_lache"])) {datt[,"cpt_heure_lache"]<-datt[,"cpt_heure_second_lache"]}
        t_db<-as.POSIXct(paste(datt[,"eqa_date_debut"],datt[,"cpt_heure_lache"]),format="%Y-%m-%d %H:%M:%S")
        attr(t_db, "tzone") <- "UTC"
        ndbf<- ndbf[which(as.POSIXct(paste(ndbf[,"UTC_DATE"],ndbf[,"UTC_TIME"]),format="%Y-%m-%d %H:%M:%S", tz="UTC") >= t_db) ,]
        #####on tronque la date et heure de fin sur celle du GPS correspondant
        date_heure_fin<-as.POSIXct(maxndbf[which(maxndbf[,"ani_etiq"] == datt[,"ani_etiq"] & maxndbf[,"eqt_id_usuel"] == datt[,"eqt_id_usuel"]),"date_heure_fin"],format="%Y-%m-%d %H:%M:%S", tz="UTC")
        ndbf<- ndbf[which(as.POSIXct(paste(ndbf[,"UTC_DATE"],ndbf[,"UTC_TIME"]),format="%Y-%m-%d %H:%M:%S", tz="UTC") <= date_heure_fin) ,]
 
        if (dim(ndbf)[1] == 0) {
        cat(bgRed(black(rq<-paste0("Les données d'activité du fichier ",fil," sont en dehors de la plage de déploiement !!"))));source(logpath);file.remove(paste0(path_global,path_global_gps_vectro,"/sensor/",fil))} else {
        #######on récupère les champs du fichier exporté
          ###Vectronic: GPS format de fichier d'export vers Alain
          #activité: NO	CollarID	UTC_DATE	UTC_TIME	LMT_DATE	LMT_TIME	ACT_MODE	DT	ActivityX	ActivityY	ActivityZ	TEMP	AnimalID	GroupID
          
          ndbf<-ndbf[,c("NO","COLLARID","UTC_DATE","UTC_TIME","LMT_DATE","LMT_TIME","ACT_MODE","DT","ACTIVITYX","ACTIVITYY","ACTIVITYZ","TEMP","ANIMALID","GROUPID")]
          names(ndbf)<-c("NO","CollarID","UTC_DATE","UTC_TIME","LMT_DATE","LMT_TIME","ACT_MODE","DT","ActivityX","ActivityY","ActivityZ","TEMP","AnimalID","GroupID")
          ####on teste qu'on a bien récupéré tout le monde
          if (dim(ndbf)[2] != 14) {cat(bgRed(white(format<-"problème de format de fichier pour l'animal",dat$ani_etiq," équipé du collier ",eqt_id_usuel," ")));source(logpath2)};
        ########On enregistre le fichier tronque############################################################################
        write.dbf(ndbf, paste0(path_global,path_global_gps_vectro,"/sensor/",filname))
        file.rename(paste0(path_global,path_global_gps_vectro,"/sensor/",filname),paste0(path_global,path_global_gps_vectro,"/sensor/",nom_ACT))
        #######On test si il y a un capteur d'activite si c'est non alors on écrit une erreur dans le log
        if(length(grep("activite",datt[,"sen_association"]) != 0)){act <- TRUE
          dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin_activite = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"', 
                                   eqa_activite = ",act,"
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",unique(datt[,"eqt_id_usuel"]),"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   "))} else {cat(bgRed(black(rq<-"problème il y a un fichier d'activité alors que le collier est déclaré sans capteur d'activité !!")));source(logpath);file.remove(paste0(path_global,path_global_gps_vectro,"/sensor/",nom_ACT))}
        
      }} else {
        file.copy(from = paste0(getwd(),"/",fil),
                  to   = paste0(path_global,path_global_gps_vectro,"/",filname))
        ndbf<-read.dbf(paste0(path_global,path_global_gps_vectro,"/",filname))
        #######On rajoute les colonnes ANIMAL et NUM
        #setwd(paste0(path_global,path_global_gps_vectro,"/"))
        ndbf<-cbind(ANIMAL = rep(tolower(dat[,"telemetrie"]) ,dim(read.dbf(paste0(path_global,path_global_gps_vectro,"/",filname)))[1]),NUM =paste0("a",read.dbf(paste0(path_global,path_global_gps_vectro,"/",filname))[,1]),read.dbf(paste0(path_global,path_global_gps_vectro,"/",filname)))
        ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
        names(ndbf)[grep(".N",names(ndbf),fixed = TRUE)]<-gsub("_C.N","_C\\/N",names(ndbf)[grep(".N",names(ndbf),fixed = TRUE)])
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        names(ndbf) <-toupper(names(ndbf))
        #######On tronque le fichier sur la date de capture et > heure de lache
        if(is.na(datt[,"cpt_heure_lache"])) {datt[,"cpt_heure_lache"]<-datt[,"cpt_heure_second_lache"]} 
        if(is.na(datt[,"cpt_heure_lache"])) {cat(bgRed(black(rq<-"pas d'heure de lâché pour ",datt[,"ani_etiq"],"")))}
        #####pas de loc avant la date et l'heure de lacher
        ndbf<- ndbf[which(
          as.POSIXct(paste0(ndbf[,"UTC_DATE"]," ",ndbf[,"UTC_TIME"]), format="%Y-%m-%d %H:%M:%S" ,tz="UTC")>=
            as.POSIXct(paste0(datt[,"eqa_date_debut"]," ",datt[,"cpt_heure_lache"], format="%Y-%m-%d %H:%M:%S" ,tz="CET"))),]
        ####si la cause de fin de suivi est drop off activé, pas de loc après la date de fin de suivi
        if (!is.na(datt[,"eqa_date_fin_suivi"]) & grepl("activ|sol$|recapture|^drop",datt[,"eqa_cause_fin_suivi"])) {
          ndbf<- ndbf[which(
            as.Date(ndbf[,"UTC_DATE"], format="%Y-%m-%d")<=
              as.Date(paste0(datt[,"eqa_date_fin_suivi"], format="%Y-%m-%d"))),]
        }
        if (dim(ndbf)[1] == 0) {cat(bgRed(black(rq<-paste0("Les données GPS du fichier ",fil," sont en dehors de la plage de déploiement !!"))));source(logpath);file.remove(paste0(path_global,path_global_gps_vectro,"/",fil))} else {
          #######je collecte la date et l'heure de fin pour pouvoir tronquer l'activité
          maxndbf<- rbind(maxndbf, cbind(ani_etiq = datt[,"ani_etiq"], eqt_id_usuel = datt[,"eqt_id_usuel"],date_heure_fin = as.character(max(as.POSIXct(paste0(ndbf[,"UTC_DATE"]," ",ndbf[,"UTC_TIME"]), format="%Y-%m-%d %H:%M:%S" ,tz="UTC")))))
          ####on sélectionne les champs pour l'export vers Alain
          names(ndbf)<-gsub("UTC_","GMT_",names(ndbf))
          ###Vectronic: GPS format d'export de Bruno
          #gps: ANIMAL	NO	NUM	COLLARID	GMT_DATE	GMT_TIME	LATITUDE	LONGITUDE	HEIGHT	DOP	FIXTYPE	SATS_USED	XL3	YL3	TEMP
          #ndbf<-ndbf[,c("ANIMAL","NO","NUM","COLLARID","GMT_DATE","GMT_TIME","LATITUDE","LONGITUDE","HEIGHT","DOP","FIXTYPE","SATS_USED","TEMP")]
          ndbf<-ndbf[,c("ANIMAL","NO","COLLARID","GMT_DATE","GMT_TIME","LATITUDE","LONGITUDE","HEIGHT","DOP","FIXTYPE","SATS_USED","TEMP")]
          
          #on teste que tous les champs sont bien sélectionnés
          if (dim(ndbf)[2] != 12) {cat(bgRed(white(format<-"problème de format de fichier pour l'animal",dat$ani_etiq," équipé du collier ",eqt_id_usuel," ")));source(logpath2)};

        ######On enregistre le fichier tronque
          ndbf$LATITUDE<-as.factor(round(ndbf$LATITUDE,7))
          ndbf$LATITUDE<-as.character(ndbf$LATITUDE)
          ndbf$LATITUDE<-gsub("\\.",",",ndbf$LATITUDE)
          ndbf[which(is.na(ndbf$LATITUDE)),"LATITUDE"]<-rep("",length(ndbf[which(is.na(ndbf$LATITUDE)),"LATITUDE"]))
          ndbf$LATITUDE<-as.factor(ndbf$LATITUDE)
          ndbf$LONGITUDE<-as.factor(round(ndbf$LONGITUDE,7))
          ndbf$LONGITUDE<-as.character(ndbf$LONGITUDE)
          ndbf$LONGITUDE<-gsub("\\.",",",ndbf$LONGITUDE)
          ndbf[which(is.na(ndbf$LONGITUDE)),"LONGITUDE"]<-rep("",length(ndbf[which(is.na(ndbf$LONGITUDE)),"LONGITUDE"]))
          ndbf$LONGITUDE<-as.factor(ndbf$LONGITUDE)
          ndbf$HEIGHT<-as.factor(round(ndbf$HEIGHT,2))
          ndbf$HEIGHT<-as.character(ndbf$HEIGHT)
          ndbf$HEIGHT<-gsub("\\.",",",ndbf$HEIGHT)
          ndbf[which(is.na(ndbf$HEIGHT)),"HEIGHT"]<-rep("",length(ndbf[which(is.na(ndbf$HEIGHT)),"HEIGHT"]))
          ndbf$HEIGHT<-as.factor(ndbf$HEIGHT)
          ndbf$DOP<-as.factor(round(ndbf$DOP,1))
          ndbf$DOP<-as.character(ndbf$DOP)
          ndbf$DOP<-gsub("\\.",",",ndbf$DOP)
          ndbf[which(is.na(ndbf$DOP)),"DOP"]<-rep("",length(ndbf[which(is.na(ndbf$DOP)),"DOP"]))
          ndbf$DOP<-as.factor(ndbf$DOP)
        write.dbf(ndbf, paste0(path_global,path_global_gps_vectro,"/",filname))
        file.rename(paste0(path_global,path_global_gps_vectro,"/",filname),paste0(path_global,path_global_gps_vectro,"/",nom_GPS))
        #######On met a jour la bd_cefs pour la date de fin de collier
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        if (!is.na(ymd(dbGetQuery(serveur,paste0("SELECT ani_date_mort FROM public.t_animal_ani where ani_id = ",datt[,"ani_id"],";"))[,1]))){
        if (ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]) > ymd(dbGetQuery(serveur,paste0("SELECT ani_date_mort FROM public.t_animal_ani where ani_id = ",datt[,"ani_id"],";"))[,1]))
        {cat(bgRed(black(rq<-"La date de fin d'équipement du collier est supérieure à la date de mort de l'animal")));source(logpath);file.remove(paste0(path_global,path_global_gps_vectro,"/",fil))} else {
          dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"'
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   "))  
        }}else{
        dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"'
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   "))}
        
       } #date_GPS(eqt_id_usuel)
      }
    }
  }
 
#########################################################GSM vECTRONIC #####################################  
  if (length(grep("VECTRONIC",toupper(dat[,"mar_libelle"]))) != 0){
    if (length(grep("GSM",dat[,"mod_libelle"])) != 0){
      if (length(grep("ACT",fil)) != 0){
        file.copy(from = paste0(getwd(),"/",fil),
                  to   = paste0(path_global,path_global_gsm_vectro,"/sensor/",filname))
        #file.rename(paste0(path_global,path_global_gsm_vectro,"/sensor/",fil),paste0(path_global,path_global_gsm_vectro,"/sensor/",nom_ACT))
        #date_act(eqt_id_usuel)
        #######On tronque le fichier sur la date de capture et > heure de lache        
        ndbf<-read.dbf(paste0(path_global,path_global_gsm_vectro,"/sensor/",filname))
        names(ndbf) <-toupper(names(ndbf))
        ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        if(is.na(datt[,"cpt_heure_lache"])) {datt[,"cpt_heure_lache"]<-datt[,"cpt_heure_second_lache"]}
        t_db<-as.POSIXct(paste(datt[,"eqa_date_debut"],datt[,"cpt_heure_lache"]),format="%Y-%m-%d %H:%M:%S")
        attr(t_db, "tzone") <- "UTC"
        ndbf<- ndbf[which(as.POSIXct(paste(ndbf[,"UTC_DATE"],ndbf[,"UTC_TIME"]),format="%Y-%m-%d %H:%M:%S", tz="UTC") >= t_db) ,]
        #####on tronque la date et heure de fin sur celle du GPS correspondant
        date_heure_fin<-as.POSIXct(maxndbf[which(maxndbf[,"ani_etiq"] == datt[,"ani_etiq"] & maxndbf[,"eqt_id_usuel"] == datt[,"eqt_id_usuel"]),"date_heure_fin"],format="%Y-%m-%d %H:%M:%S", tz="UTC")
        ndbf<- ndbf[which(as.POSIXct(paste(ndbf[,"UTC_DATE"],ndbf[,"UTC_TIME"]),format="%Y-%m-%d %H:%M:%S", tz="UTC") <= date_heure_fin) ,]

        if (dim(ndbf)[1] == 0) {cat(bgRed(black(rq<-paste0("Les données d'activité  du fichier ",fil," sont en dehors de la plage de déploiement !!"))));source(logpath);file.remove(paste0(path_global,path_global_gsm_vectro,"/sensor/",fil))} else {
        #######on sélectionne les champs pour l'export vers Alain
          ###Vectronic: GSM
          #activité: NO	CollarID	UTC_DATE	UTC_TIME	LMT_DATE	LMT_TIME	ACT_MODE	DT	ActivityX	ActivityY	ActivityZ	TEMP	AnimalID	GroupID
          ndbf<-ndbf[,c("NO","COLLARID","UTC_DATE","UTC_TIME","LMT_DATE","LMT_TIME","ACT_MODE","DT","ACTIVITYX","ACTIVITYY","ACTIVITYZ","TEMP","ANIMALID","GROUPID")]
          names(ndbf)<-c("NO","CollarID","UTC_DATE","UTC_TIME","LMT_DATE","LMT_TIME","ACT_MODE","DT","ActivityX","ActivityY","ActivityZ","TEMP","AnimalID","GroupID")
          ####on test qu'on est bien toutes les colonnes
          if (dim(ndbf)[2] != 14) {cat(bgRed(white(format<-"problème de format de fichier pour l'animal",dat$ani_etiq," équipé du collier ",eqt_id_usuel," ")));source(logpath2)};
          #################On enregistre le fichier tronque####################################################
        write.dbf(ndbf, paste0(path_global,path_global_gsm_vectro,"/sensor/",filname))
        file.rename(paste0(path_global,path_global_gsm_vectro,"/sensor/",filname),paste0(path_global,path_global_gsm_vectro,"/sensor/",nom_ACT))
        #######On test si il y a un capteur d'activite si c'est non alors on écrit une erreur dans le log
        if(length(grep("activite",datt[,"sen_association"]) != 0)){act <- TRUE
        dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin_activite = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"', 
                                   eqa_activite = ",act,"
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   ")
          )} else {cat(bgRed(black(rq<-"problème il y a un fichier d'activité alors que le collier est déclaré sans capteur d'activité !!")));source(logpath);file.remove(paste0(path_global,path_global_gsm_vectro,"/sensor/",fil))}
        
      }} else {
        file.copy(from = paste0(getwd(),"/",fil),
                  to   = paste0(path_global,path_global_gsm_vectro,"/",filname))
        ndbf<-read.dbf(paste0(path_global,path_global_gsm_vectro,"/",filname), as.is = TRUE)
        ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
        #######On rajoute les colonnes ANIMAL et NUM
        #setwd(paste0(path_global,path_global_gsm_vectro,"/"))
        ndbf<-cbind(ANIMAL = rep(tolower(dat[,"telemetrie"]) ,dim(read.dbf(paste0(path_global,path_global_gsm_vectro,"/",filname)))[1]),NUM =paste0("a",read.dbf(paste0(path_global,path_global_gsm_vectro,"/",filname))[,1]),read.dbf(paste0(path_global,path_global_gsm_vectro,"/",filname)))
        names(ndbf)[grep(".N",names(ndbf),fixed = TRUE)]<-gsub("_C.N","_C\\/N",names(ndbf)[grep(".N",names(ndbf),fixed = TRUE)])
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
        names(ndbf) <-toupper(names(ndbf))
        #######On tronque le fichier sur la date de capture et > heure de lache
        if(is.na(datt[,"cpt_heure_lache"])) {datt[,"cpt_heure_lache"]<-datt[,"cpt_heure_second_lache"]} 
        if(is.na(datt[,"cpt_heure_lache"])) {cat(bgRed(black(rq<-"pas d'heure de lâché pour ",datt[,"ani_eriq"],"")));source(logpath);file.remove(paste0(path_global,path_global_gsm_vectro,"/",fil))}
        #####pas de loc avant la date et l'heure de lacher
        ndbf<- ndbf[which(
          as.POSIXct(paste0(ndbf[,"UTC_DATE"]," ",ndbf[,"UTC_TIME"]), format="%Y-%m-%d %H:%M:%S" ,tz="UTC")>=
            as.POSIXct(paste0(datt[,"eqa_date_debut"]," ",datt[,"cpt_heure_lache"], format="%Y-%m-%d %H:%M:%S" ,tz="CET"))),]
        ####si la cause de fin de suivi est drop off activé, pas de loc après la date de fin de suivi
        if (!is.na(datt[,"eqa_date_fin_suivi"]) & grepl("activ|sol$|recapture|^drop",datt[,"eqa_cause_fin_suivi"])) {
          ndbf<- ndbf[which(
            as.Date(ndbf[,"UTC_DATE"], format="%Y-%m-%d")<=
              as.Date(paste0(datt[,"eqa_date_fin_suivi"], format="%Y-%m-%d"))),]
        }
        if (dim(ndbf)[1] == 0) {cat(bgRed(black(rq<-paste0("Les données GPS  du fichier ",fil," sont en dehors de la plage de déploiement !!"))));source(logpath);file.remove(paste0(path_global,path_global_gsm_vectro,"/",fil))} else {
          #######je collecte la date et l'heure de fin pour pouvoir tronquer l'activité
          maxndbf<- rbind(maxndbf, cbind(ani_etiq = datt[,"ani_etiq"], eqt_id_usuel = datt[,"eqt_id_usuel"],date_heure_fin = as.character(max(as.POSIXct(paste0(ndbf[,"UTC_DATE"]," ",ndbf[,"UTC_TIME"]), format="%Y-%m-%d %H:%M:%S" ,tz="UTC")))))
          ####on sélectionne les champs pour l'export vers Alain
          names(ndbf)<-gsub("UTC_","GMT_",names(ndbf))
          ###Vectronic: GPS foramt d'export de Bruno
          #gps: ANIMAL	NO	NUM	COLLARID	GMT_DATE	GMT_TIME	LATITUDE	LONGITUDE	HEIGHT	DOP	FIXTYPE	SATS_USED	XL3	YL3	TEMP
          #ndbf<-ndbf[,c("ANIMAL","NO","NUM","COLLARID","GMT_DATE","GMT_TIME","LATITUDE","LONGITUDE","HEIGHT","DOP","FIXTYPE","SATS_USED","TEMP")]
          ndbf<-ndbf[,c("ANIMAL","NO","COLLARID","GMT_DATE","GMT_TIME","LATITUDE","LONGITUDE","HEIGHT","DOP","FIXTYPE","SATS_USED","TEMP")]
          
          names(ndbf)<-toupper(names(ndbf))
          #on teste que tous les champs sont bien sélectionnés
          if (dim(ndbf)[2] != 12) {cat(bgRed(white(format<-"problème de format de fichier pour l'animal",dat$ani_etiq," équipé du collier ",eqt_id_usuel," ")));source(logpath2)};
          ######On enregistre le fichier tronque
          ndbf$LATITUDE<-as.factor(round(ndbf$LATITUDE,7))
          ndbf$LATITUDE<-as.character(ndbf$LATITUDE)
          ndbf$LATITUDE<-gsub("\\.",",",ndbf$LATITUDE)
          ndbf[which(is.na(ndbf$LATITUDE)),"LATITUDE"]<-rep("",length(ndbf[which(is.na(ndbf$LATITUDE)),"LATITUDE"]))
          ndbf$LATITUDE<-as.factor(ndbf$LATITUDE)
          ndbf$LONGITUDE<-as.factor(round(ndbf$LONGITUDE,7))
          ndbf$LONGITUDE<-as.character(ndbf$LONGITUDE)
          ndbf$LONGITUDE<-gsub("\\.",",",ndbf$LONGITUDE)
          ndbf[which(is.na(ndbf$LONGITUDE)),"LONGITUDE"]<-rep("",length(ndbf[which(is.na(ndbf$LONGITUDE)),"LONGITUDE"]))
          ndbf$LONGITUDE<-as.factor(ndbf$LONGITUDE)
          ndbf$HEIGHT<-as.factor(round(ndbf$HEIGHT,2))
          ndbf$HEIGHT<-as.character(ndbf$HEIGHT)
          ndbf$HEIGHT<-gsub("\\.",",",ndbf$HEIGHT)
          ndbf[which(is.na(ndbf$HEIGHT)),"HEIGHT"]<-rep("",length(ndbf[which(is.na(ndbf$HEIGHT)),"HEIGHT"]))
          ndbf$HEIGHT<-as.factor(ndbf$HEIGHT)
          ndbf$DOP<-as.factor(round(ndbf$DOP,1))
          ndbf$DOP<-as.character(ndbf$DOP)
          ndbf$DOP<-gsub("\\.",",",ndbf$DOP)
          ndbf[which(is.na(ndbf$DOP)),"DOP"]<-rep("",length(ndbf[which(is.na(ndbf$DOP)),"DOP"]))
          ndbf$DOP<-as.factor(ndbf$DOP)
        write.dbf(ndbf, paste0(path_global,path_global_gsm_vectro,"/",filname))
        file.rename(paste0(path_global,path_global_gsm_vectro,"/",filname),paste0(path_global,path_global_gsm_vectro,"/",nom_GPS))
        #######On met a jour la bd_cefs pour la date de fin de collier
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        if (!is.na(ymd(dbGetQuery(serveur,paste0("SELECT ani_date_mort FROM public.t_animal_ani where ani_id = ",datt[,"ani_id"],";"))[,1]))){
          if (ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]) > ymd(dbGetQuery(serveur,paste0("SELECT ani_date_mort FROM public.t_animal_ani where ani_id = ",datt[,"ani_id"],";"))[,1]))
          {cat(bgRed(black(rq<-"La date de fin d'équipement du collier est supérieure à la date de mort de l'animal")));source(logpath);file.remove(paste0(path_global,path_global_gsm_vectro,"/",fil))}else {
            dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"'
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   "))  
          }}else{
        dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"'
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   "))}
        } ### fin dim(ndbf)[1] == 0, else {}
      } ### fil activite non else{}
    } ### si gsm
  } ### si vectronic
  
#########################################################IRIDIUM VECTRONIC #####################################  
  if (length(grep("VECTRONIC",toupper(dat[,"mar_libelle"]))) != 0){
    if (length(grep("IRIDIUM",dat[,"mod_libelle"])) != 0){
      if (length(grep("ACT",fil)) != 0){
        file.copy(from = paste0(getwd(),"/",fil),
                  to   = paste0(path_global,path_global_iridium_vectro,"/sensor/",filname))
        #file.rename(paste0(path_global,path_global_iridium_vectro,"/sensor/",fil),paste0(path_global,path_global_iridium_vectro,"/sensor/",nom_ACT))
        #date_act(eqt_id_usuel)
        #######On tronque le fichier sur la date de capture et > heure de lache        
        ndbf<-read.dbf(paste0(path_global,path_global_iridium_vectro,"/sensor/",filname))
        ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
        names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
        if(is.na(datt[,"cpt_heure_lache"])) {datt[,"cpt_heure_lache"]<-datt[,"cpt_heure_second_lache"]}
        t_db<-as.POSIXct(paste(datt[,"eqa_date_debut"],datt[,"cpt_heure_lache"]),format="%Y-%m-%d %H:%M:%S")
        attr(t_db, "tzone") <- "UTC"
        ndbf<- ndbf[which(as.POSIXct(paste(ndbf[,"UTC_DATE"],ndbf[,"UTC_TIME"]),format="%Y-%m-%d %H:%M:%S", tz="UTC") >= t_db) ,]
        #####on tronque la date et heure de fin sur celle du GPS correspondant
        date_heure_fin<-as.POSIXct(maxndbf[which(maxndbf[,"ani_etiq"] == datt[,"ani_etiq"] & maxndbf[,"eqt_id_usuel"] == datt[,"eqt_id_usuel"]),"date_heure_fin"],format="%Y-%m-%d %H:%M:%S", tz="UTC")
        ndbf<- ndbf[which(as.POSIXct(paste(ndbf[,"UTC_DATE"],ndbf[,"UTC_TIME"]),format="%Y-%m-%d %H:%M:%S", tz="UTC") <= date_heure_fin) ,]

        if (dim(ndbf)[1] == 0) {cat(bgRed(black(rq<-paste0("Les données d'activité  du fichier ",fil," sont en dehors de la plage de déploiement !!"))));source(logpath);file.remove(paste0(path_global,path_global_iridium_vectro,"/sensor/",fil))} else {

          
          #########################################################A TERMINER ################################
          names(ndbf)<-gsub("UTC_","GMT_",names(ndbf))
          ######en fonction des données brutes activité iridium, selectionner les champ à exporter vers Alain:
          ndbf<-ndbf[,c("NO","COLLARID","UTC_DATE","UTC_TIME","LMT_DATE","LMT_TIME","ACT_MODE","DT","ACTIVITYX","ACTIVITYY","ACTIVITYZ","TEMP","ANIMALID","GROUPID")]
          #####test que tous les champ sont bien sélectionnés
          if (dim(ndbf)[2] != 14) {cat(bgRed(white(format<-"problème de format de fichier pour l'animal",dat$ani_etiq," équipé du collier ",eqt_id_usuel," ")));source(logpath2)};
          ###############################################################################################################
          
          
          #######On enregistre le fichier tronque
          write.dbf(ndbf, paste0(path_global,path_global_iridium_vectro,"/sensor/",filname))
          file.rename(paste0(path_global,path_global_iridium_vectro,"/sensor/",filname),paste0(path_global,path_global_iridium_vectro,"/sensor/",nom_ACT))
          #######On test si il y a un capteur d'activite si c'est non alors on écrit une erreur dans le log
          if(length(grep("activite",datt[,"sen_association"]) != 0)){act <- TRUE
          dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin_activite = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"', 
                                   eqa_activite = ",act,"
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   ")
          )} else {cat(bgRed(black(rq<-"problème il y a un fichier d'activité alors que le collier est déclaré sans capteur d'activité !!")));source(logpath);file.remove(paste0(path_global,path_global_iridium_vectro,"/sensor/",fil))}
          
        }} else {
          file.copy(from = paste0(getwd(),"/",fil),
                    to   = paste0(path_global,path_global_iridium_vectro,"/",filname))
          ndbf<-read.dbf(paste0(path_global,path_global_iridium_vectro,"/",filname))
          ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
          #######On rajoute les colonnes ANIMAL et NUM
          #setwd(paste0(path_global,path_global_iridium_vectro,"/"))
          ndbf<-cbind(ANIMAL = rep(tolower(dat[,"telemetrie"]) ,dim(read.dbf(paste0(path_global,path_global_iridium_vectro,"/",filname)))[1]),NUM =paste0("a",read.dbf(paste0(path_global,path_global_iridium_vectro,"/",filname))[,1]),read.dbf(paste0(path_global,path_global_iridium_vectro,"/",filname)))
          names(ndbf)[grep(".N",names(ndbf),fixed = TRUE)]<-gsub("_C.N","_C\\/N",names(ndbf)[grep(".N",names(ndbf),fixed = TRUE)])
          names(ndbf)<-gsub("GMT_","UTC_",names(ndbf))
          ndbf<-ndbf[order(as.Date(ndbf$UTC_DATE), chron(times= ndbf$UTC_TIME)),]
          names(ndbf) <-toupper(names(ndbf))
          #######On tronque le fichier sur la date de capture et pour une heure > heure de lache
          if(is.na(datt[,"cpt_heure_lache"])) {datt[,"cpt_heure_lache"]<-datt[,"cpt_heure_second_lache"]} 
          if(is.na(datt[,"cpt_heure_lache"])) {cat(bgRed(black(rq<-"pas d'heure de lâché pour ",datt[,"ani_eriq"],"")))}
          #####pas de loc avant la date et l'heure de lacher
          ndbf<- ndbf[which(
            as.POSIXct(paste0(ndbf[,"UTC_DATE"]," ",ndbf[,"UTC_TIME"]), format="%Y-%m-%d %H:%M:%S" ,tz="UTC")>=
              as.POSIXct(paste0(datt[,"eqa_date_debut"]," ",datt[,"cpt_heure_lache"], format="%Y-%m-%d %H:%M:%S" ,tz="CET"))),]
          ####si la cause de fin de suivi est drop off activé, pas de loc après la date de fin de suivi
          if (!is.na(datt[,"eqa_date_fin_suivi"]) & grepl("activ|sol$|recapture|^drop",datt[,"eqa_cause_fin_suivi"])) {
            ndbf<- ndbf[which(
              as.Date(ndbf[,"UTC_DATE"], format="%Y-%m-%d")<=
                as.Date(paste0(datt[,"eqa_date_fin_suivi"], format="%Y-%m-%d"))),]
          }
          if (dim(ndbf)[1] == 0) {cat(bgRed(black(rq<-paste0("Les données GPS  du fichier ",fil," sont en dehors de la plage de déploiement !!"))));source(logpath);file.remove(paste0(path_global,path_global_iridium_vectro,"/",fil))} else {
            #######je collecte la date et l'heure de fin pour pouvoir tronquer l'activité
            maxndbf<- rbind(maxndbf, cbind(ani_etiq = datt[,"ani_etiq"], eqt_id_usuel = datt[,"eqt_id_usuel"],date_heure_fin = as.character(max(as.POSIXct(paste0(ndbf[,"UTC_DATE"]," ",ndbf[,"UTC_TIME"]), format="%Y-%m-%d %H:%M:%S" ,tz="UTC")))))
 
            ###############################################################A TERMINER #################################################
            ####on sélectionne les champs pour l'export vers Alain
            names(ndbf)<-gsub("UTC_","GMT_",names(ndbf))
            ###Vectronic IRIDIUM: GPS format d'export de Bruno pour vectronic
            #gps: ANIMAL	NO	NUM	COLLARID	GMT_DATE	GMT_TIME	LATITUDE	LONGITUDE	HEIGHT	DOP	FIXTYPE	SATS_USED	XL3	YL3	TEMP
            ndbf<-ndbf[,c("ANIMAL","NO","CollarID","UTC_DATE","UTC_TIME","LATITUDE","LONGITUDE","HEIGHT","DOP","FIXTYPE","SATS_USED","TEMP")]
            names(ndbf)<-toupper(ndbf)
            #on teste que tous les champs sont bien sélectionnés
            if (dim(ndbf)[2] != 12) {cat(bgRed(white(format<-"problème de format de fichier pour l'animal",dat$ani_etiq," équipé du collier ",eqt_id_usuel," ")));source(logpath2)};
            ############################################################################################################################
            
            ######On enregistre le fichier tronque
            ndbf$LATITUDE<-as.factor(round(ndbf$LATITUDE,7))
            ndbf$LATITUDE<-as.character(ndbf$LATITUDE)
            ndbf$LATITUDE<-gsub("\\.",",",ndbf$LATITUDE)
            ndbf[which(is.na(ndbf$LATITUDE)),"LATITUDE"]<-rep("",length(ndbf[which(is.na(ndbf$LATITUDE)),"LATITUDE"]))
            ndbf$LATITUDE<-as.factor(ndbf$LATITUDE)
            ndbf$LONGITUDE<-as.factor(round(ndbf$LONGITUDE,7))
            ndbf$LONGITUDE<-as.character(ndbf$LONGITUDE)
            ndbf$LONGITUDE<-gsub("\\.",",",ndbf$LONGITUDE)
            ndbf[which(is.na(ndbf$LONGITUDE)),"LONGITUDE"]<-rep("",length(ndbf[which(is.na(ndbf$LONGITUDE)),"LONGITUDE"]))
            ndbf$LONGITUDE<-as.factor(ndbf$LONGITUDE)
            ndbf$HEIGHT<-as.factor(round(ndbf$HEIGHT,2))
            ndbf$HEIGHT<-as.character(ndbf$HEIGHT)
            ndbf$HEIGHT<-gsub("\\.",",",ndbf$HEIGHT)
            ndbf[which(is.na(ndbf$HEIGHT)),"HEIGHT"]<-rep("",length(ndbf[which(is.na(ndbf$HEIGHT)),"HEIGHT"]))
            ndbf$HEIGHT<-as.factor(ndbf$HEIGHT)
            ndbf$DOP<-as.factor(round(ndbf$DOP,1))
            ndbf$DOP<-as.character(ndbf$DOP)
            ndbf$DOP<-gsub("\\.",",",ndbf$DOP)
            ndbf[which(is.na(ndbf$DOP)),"DOP"]<-rep("",length(ndbf[which(is.na(ndbf$DOP)),"DOP"]))
            ndbf$DOP<-as.factor(ndbf$DOP)
            
            write.dbf(ndbf, paste0(path_global,path_global_iridium_vectro,"/",filname))
            file.rename(paste0(path_global,path_global_iridium_vectro,"/",filname),paste0(path_global,path_global_iridium_vectro,"/",nom_GPS))
            #######On met a jour la bd_cefs pour la date de fin de collier
            if (!is.na(ymd(dbGetQuery(serveur,paste0("SELECT ani_date_mort FROM public.t_animal_ani where ani_id = ",datt[,"ani_id"],";"))[,1]))){
              if (ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]) > ymd(dbGetQuery(serveur,paste0("SELECT ani_date_mort FROM public.t_animal_ani where ani_id = ",datt[,"ani_id"],";"))[,1]))
              {cat(bgRed(black(rq<-"La date de fin d'équipement du collier est supérieure à la date de mort de l'animal")));source(logpath);file.remove(paste0(path_global,path_global_iridium_vectro,"/",fil))}else {
                dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"'
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   "))  
              }}else{
            dbSendQuery(serveur,paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin = '",ymd(ndbf[dim(ndbf)[1],"UTC_DATE"]),"'
                                   where eqa_ani_id = ",datt[,"ani_id"]," and eqa_eqt_id = ",dbGetQuery(serveur,paste0("SELECT eqt_id from t_equipement_eqt where eqt_id_usuel = '",datt[,"eqt_id_usuel"],"' "))[,1]," and eqa_annee_suivi = ",annee,"
                                   "))}
          } ### fin dim(ndbf)[1] == 0, else {}
        } ### fil activite non else{}
    } ### si gsm
  } ### si vectronic
}


if (length(dbf_files) != i){
  cat(bgRed(white(rq<-"Attention le dossier de données brutes contient ",length(dbf_files)," fichiers pour l'année ",annee," et seuls ",i," fichiers ont été intégrés")))
} 


if (file.exists(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Docs/",annee,"/log_",h,".csv"))){
  cat(bgRed(black(rq<-"Des erreures sont survenues consulter le fichier de log  C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Docs/",annee," corrigez les erreurs et relancer le script")))
} 

options(warn = defaultW)

##individus équipés dans bd_cefs et pour lesquels je n'ai pas eu de fichier


etq<-unlist(lapply(str_split(dbf_files[grep("GPS", dbf_files)],"/"),"[[",1))

colliers_manquants<-dbGetQuery(serveur,
paste0("
SELECT ani_etiq, ani_name, cap_annee_suivi, cap_date, ani_sexe, cap_age_classe, sit_nom_court, teq_nom_court, eqt_id_usuel, mar_libelle, mod_libelle, eqa_date_debut, eqa_date_fin, eqa_date_fin_suivi, eqa_date_fin_suivi_text, eqa_cause_fin_suivi, eqa_dernier_contact, ani_mortalite, ani_date_mort, ani_cause_mort, eqa_activite, eqa_date_fin_activite, eqa_probleme, eqa_date_fin_text, eqa_date_fin_arrondi, eqa_remarque_suivi, ani_date_mort_arrondi, ani_date_mort_text
FROM public.v_individus_total where cap_annee_suivi = 2021 and teq_nom_court is not null and teq_nom_court != 'VHF' and ani_etiq not in ",v2db(etq),"")
)
if (length(colliers_manquants[,1])!=0){
  cat(bgRed(white(rq2<-paste0("Des colliers déployés n'ont pas de fichiers de données associés voir le fichier  C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Docs/",annee,"/colliers_manquants_",trimws(h),".csv pour plus de détails. Corrigez les erreurs et relancer le script"))));source(logpath)
  }

setwd(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_colliers/Docs/",annee,"/"))
individus<-dbGetQuery(serveur,gsub("[\r\n]", " ",paste0("SELECT concat(ani_etiq,'_',substring(cap_annee_suivi::varchar,3,2)) fichier, 
                TO_CHAR(cap_date::date,'DD/MM/YYYY') cap_date,             
                case            
                WHEN cpt_heure_second_lache IS not NULL THEN cpt_heure_second_lache            
                ELSE cpt_heure_lache            
                end heure_lacher,            
                TO_CHAR(eqa_date_debut::date,'DD/MM/YYYY') eqa_date_debut,
                TO_CHAR(eqa_date_fin::date,'DD/MM/YYYY') eqa_date_fin,            
                TO_CHAR(eqa_date_fin_activite::date,'DD/MM/YYYY') eqa_date_fin_activite
                From v_individus_total where cap_annee_suivi = ",annee," and cap_age_classe != 'faon' and teq_nom_court in ('GPS','GSM')")))

write.table(individus , file = paste0("colliersbd_",gsub("-","_",Sys.Date()), ".csv"), append = FALSE, col.names=!file.exists(paste0("captures_",gsub("-","_",Sys.Date()), ".csv")), na="", row.names = F, sep=";", fileEncoding = "UTF-8")

