library(RPostgreSQL)
library(lubridate)
library(stringr)
library(xlsx)

#con<- dbConnect(PostgreSQL(), host="localhost", dbname="db_chevreuils", user="postgres", password="postgres")
source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_CEFS/con_raspi_dbchevreuils.R")
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")
raspi<-serveur
utf8 <- function(x) {
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
  # Same on column names:
  Encoding(names(x)) <- "UTF-8"
  return(x)
}

####♣le csv est générer à partir du serveur
#raspi<-serveur

date<- dbGetQuery(raspi,paste0("select distinct cap_date from t_capture_cap"))[,1]
date<-paste0("('",paste0(date, collapse = "','"),"')")
#date<-Sys.Date()

###################N°Animal à  machoire########################################
noms_colonnes<- c("N°Animal","ani_nom","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date"
                  ,"jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","cap_tag_droit","cap_tag_gauche",
                  "cap_tag_droit_metal","cap_tag_gauche_metal","cap_pertinent","cap_lactation","RFID","Poids","Cir Cou","Long patte Ar","machoire")


dat<-utf8(dbGetQuery(raspi,enc2utf8(paste0("select cap_id, ani_id, ani_etiq as N°Animal, ani_name as ani_nom, cap_telemetrie as N°Animal_telemetrie, cap_bague as N°bague_annee_capture,'' as Nombre_capture, '' as inconnue,sit_nom_court as Site_Capture, cap_faon as capture_faon, cap_date as Date, extract(day from cap_date) as jour,extract(month from cap_date) as mois, extract(year from cap_date) as annee,cap_annee_suivi as annee_de_suivi, ani_sexe as Sexe, cap_age as Age_cahier, cap_age_corrige as Age_corrige, cap_age_classe as categorie_d_age,cap_etat_sante as etat_sante, cap_tag_droit, cap_tag_gauche,cap_tag_droit_metal,cap_tag_gauche_metal,cap_pertinent, '' as cap_lactation, '' as RFID, cap_poids as Poids,cap_circou as Cir_Cou,cap_lpa as Long_patte_Ar,'' as machoire 
from t_animal_ani left join t_capture_cap ON cap_ani_id = ani_id left join tr_site_capture_sit ON sit_id = cap_sit_id order by cap_date, ani_etiq"))))

dat[which(nchar(dat[,"jour"]) == 1),"jour"]<-paste0("0",dat[which(nchar(dat[,"jour"]) == 1),"jour"])
dat[which(nchar(dat[,"mois"]) == 1),"mois"]<-paste0("0",dat[which(nchar(dat[,"mois"]) == 1),"mois"])

RFID<- dbGetQuery(raspi,paste0("select rfi_cap_id, rfi_tag_code  from t_rfid_rfi where rfi_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),""))
if (dim(RFID)[1] != 0 &&  !is.null(dim(RFID)[1])) {
  data<-merge(dat[,1:grep("rfid",names(dat))-1], RFID, by.x ="cap_id", by.y ="rfi_cap_id", all.x = TRUE)
  names(data)[length(names(data))]<- "RFID"
} else {data<- cbind(data , "RFID" = "")}

dat<-merge(data,dat[,c(1,(grep("rfid",names(dat))+1):length(names(dat)))], by.x ="cap_id", by.y ="cap_id")

lact<- dbGetQuery(raspi,paste0("select aca_cap_id, aca_valeur  from tj_mesureealpha_capture_aca, tr_variable_mesuree_var where aca_var_id = var_id and var_nom_court = 'lactation' and aca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),""))
lact[,2]<-trimws(lact[,2])
if (dim(lact)[1] != 0 &&  !is.null(dim(lact)[1])) {
  data<-merge(dat[,1:grep("cap_lactation",names(dat))-1], lact, by.x ="cap_id", by.y ="aca_cap_id", all.x = TRUE)
  names(data)[length(names(data))]<- "cap_lactation"
} else {data<- cbind(data , "cap_lactation" = "")}

dat<-merge(data,dat[,c(1,(grep("cap_lactation",names(dat))+1):length(names(dat)))], by.x ="cap_id", by.y ="cap_id")

mach<- dbGetQuery(raspi,paste0("select nca_cap_id, nca_valeur  from tj_mesureenum_capture_nca, tr_variable_mesuree_var where nca_var_id = var_id and var_nom_court = 'longueur machoire' and nca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),""))
if (dim(mach)[1] != 0 &&  !is.null(dim(mach)[1])) {
  data<-merge(dat[,1:grep("machoire",names(dat))-1], mach, by.x ="cap_id", by.y ="nca_cap_id", all.x = TRUE)
  names(data)[length(names(data))]<- "machoire"
} else {data<- cbind(data , "machoire" = "")}

dat<-data ##c'est la derniere colonne

###################bois########################################
bois_gauche<- dbGetQuery(raspi,paste0("select nca_cap_id, nca_valeur  from tj_mesureenum_capture_nca, tr_variable_mesuree_var where nca_var_id = var_id and var_nom_court = 'longueur bois gauche' and nca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),""))
if (dim(bois_gauche)[1] != 0 &&  !is.null(dim(bois_gauche)[1])) {
  dat<-merge(dat, bois_gauche, by.x ="cap_id", by.y ="nca_cap_id", all.x = TRUE)
  names(dat)[length(names(dat))]<- "long bois gauche"
} else {dat<- cbind(dat , "long bois gauche" = "")}

bois_droit<- dbGetQuery(raspi,paste0("select nca_cap_id, nca_valeur  from tj_mesureenum_capture_nca, tr_variable_mesuree_var where nca_var_id = var_id and var_nom_court = 'longueur bois droit' and nca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),""))
if (dim(bois_droit)[1] != 0 &&  !is.null(dim(bois_droit)[1])) {
  dat<-merge(dat, bois_droit, by.x ="cap_id", by.y ="nca_cap_id", all.x = TRUE)
  names(dat)[length(names(dat))]<- "long bois droit"
} else {dat<- cbind(dat , "long bois droit" = "")}


# glu<- dbGetQuery(raspi,paste0("select exphy_cap_id, glucose_blood  from para_phy.t_phy_exphy where exphy_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),""))
# if (dim(glu)[1] != 0 &&  !is.null(dim(glu)[1])) {
#   glu<-na.omit(glu)
#   dat<-merge(dat, glu, by.x ="cap_id", by.y ="exphy_cap_id", all.x = TRUE)
#   names(dat)[length(names(dat))]<- "glucose"
# } else {
  dat<- cbind(dat , "glucose" = "")
  # }


###################temperature exterieure########################################
temp_ext<-data.frame()
temp_ex<- dbGetQuery(raspi,paste0("select nhca_cap_id, concat('ysi-',nhca_heure_locale_cest,'-',nhca_valeur)  from tj_mesureenum_heure_capture_nhca, tr_variable_mesuree_var where nhca_var_id = var_id and var_nom_court = 'temperature ext_ysi' and nhca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),""))
if (dim(temp_ex)[1] != 0 &&  !is.null(dim(temp_ex)[1])) {
  for (i in unique(temp_ex[,"nhca_cap_id"])) {
    if (length(temp_ex[temp_ex[,"nhca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("nhca_cap_id"= unique(temp_ex[temp_ex[,"nhca_cap_id"]==i,"nhca_cap_id"]),"T°C_ext" = paste0(temp_ex[temp_ex[,"nhca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("nhca_cap_id"= temp_ex[temp_ex[,"nhca_cap_id"]==i,"nhca_cap_id"],"T°C_ext" = temp_ex[temp_ex[,"nhca_cap_id"]==i,"concat"])}
    temp_ext <- rbind(temp_ext,temp)
  }
  dat<-merge(dat, temp_ext, by.x ="cap_id", by.y ="nhca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "T°C_ext" = "")}
#names(dat)[length(names(dat))]<- "T°C_ext"

#####t° ext historiques 
temp_ext<- dbGetQuery(raspi,paste0("select cpt_cap_id, cpt_tble_temp_exterieur from cmpt.t_capture_cpt where cpt_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and extract(year from cpt_date) != 2019"))
temp_ext<-na.omit(temp_ext)

ind <- match(temp_ext$cpt_cap_id, dat$cap_id)
dat[,dim(dat)[2]]<-as.character(dat[,dim(dat)[2]])
dat[ind,dim(dat)[2]] <- temp_ext[2][,1]
dat[,dim(dat)[2]]<-as.factor(dat[,dim(dat)[2]])

###################tiques fixées########################################
tiques<- dbGetQuery(raspi,paste0("select para_ani_id,para_daysampling, para_res from para_phy.t_parasitology_para, para_phy.tr_variable_measured_var where var_id = para_var_id and (var_name_long = 'ticks_count')"))
if (dim(tiques)[1] != 0 &&  !is.null(dim(tiques)[1])) {dat<-merge(dat, tiques, by.x =c("ani_id","date"), by.y =c("para_ani_id","para_daysampling"), all.x = TRUE)
}else{dat<- cbind(dat , "TIQUES FIXES" = "")}
names(dat)[length(names(dat))]<- "TIQUES FIXES"

###################autres parasites########################################
autrespar<- dbGetQuery(raspi,paste0("select para_ani_id, para_res from para_phy.t_parasitology_para, para_phy.tr_variable_measured_var where var_id = para_var_id and var_name_long = 'divers_macro_parasites'"))
if (dim(autrespar)[1] != 0 &&  !is.null(dim(autrespar)[1])) {dat<-merge(dat, autrespar, by.x =c("ani_id","date"), by.y =c("para_ani_id","para_daysampling"), all.x = TRUE) 
}else{dat<- cbind(dat , "autres parasites" = "")}
names(dat)[length(names(dat))]<- "autres parasites"

###################peau########################################
#########elimination des doublons d'échantillons (pas de clefs unicite sur l'outil pour eviter les plantages)
dbSendQuery(raspi,"
delete from public.t_sample_capture_sca where 
sca_id not in ( SELECT t.sca_id FROM (
  SELECT MIN(sca_id) AS sca_id, sca_cap_id, sca_sat_id, sca_sal_id, sca_sac_id, sca_sas_id, 
  sca_value, sca_remarque
  FROM public.t_sample_capture_sca
  GROUP BY sca_cap_id, sca_sat_id, sca_sal_id, sca_sac_id, sca_sas_id, 
  sca_value, sca_remarque order by sca_id ) as t)")

peau <- utf8(dbGetQuery(raspi,paste0("select sca_cap_id, concat(sat_type,'-',sal_localisation,'-',sac_contenant,'-',sas_solvant,'-',sca_value) from t_sample_capture_sca, t_capture_cap,listes.tr_samples_contenant_sac,listes.tr_samples_localisation_sal,listes.tr_samples_solvant_sas,listes.tr_samples_types_sat where cap_id = sca_cap_id and sca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and sca_sat_id = sat_id and sca_sal_id = sal_id and sca_sac_id = sac_id and sca_sas_id = sas_id and sat_type ='peau'")))
peau_f<-data.frame()
if (dim(peau)[1] != 0 &&  !is.null(dim(peau)[1])) {
  for (i in unique(peau[,"sca_cap_id"])) {
    if (length(peau[peau[,"sca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("sca_cap_id"= unique(peau[peau[,"sca_cap_id"]==i,"sca_cap_id"]),"Peau" = paste0(peau[peau[,"sca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("sca_cap_id"= peau[peau[,"sca_cap_id"]==i,"sca_cap_id"],"Peau" = peau[peau[,"sca_cap_id"]==i,"concat"])}
    peau_f <- rbind(peau_f,temp)
  }
  dat<-merge(dat, peau_f, by.x ="cap_id", by.y ="sca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "Peau" = "")}            

###################poils########################################
poils <- utf8(dbGetQuery(raspi,paste0("select sca_cap_id, concat(sat_type,'-',sal_localisation,'-',sac_contenant,'-',sas_solvant,'-',sca_value) from t_sample_capture_sca, t_capture_cap,listes.tr_samples_contenant_sac,listes.tr_samples_localisation_sal,listes.tr_samples_solvant_sas,listes.tr_samples_types_sat where cap_id = sca_cap_id and sca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and sca_sat_id = sat_id and sca_sal_id = sal_id and sca_sac_id = sac_id and sca_sas_id = sas_id and sat_type ='poils'")))  
poils_f<-data.frame()
if (dim(poils)[1] != 0 &&  !is.null(dim(poils)[1])) {
  for (i in unique(poils[,"sca_cap_id"])) {
    if (length(poils[poils[,"sca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("sca_cap_id"= unique(poils[poils[,"sca_cap_id"]==i,"sca_cap_id"]),"poils" = paste0(poils[poils[,"sca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("sca_cap_id"= poils[poils[,"sca_cap_id"]==i,"sca_cap_id"],"poils" = poils[poils[,"sca_cap_id"]==i,"concat"])}
    poils_f <- rbind(poils_f,temp)
  }
  dat<-merge(dat, poils_f, by.x ="cap_id", by.y ="sca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "poils" = "")}            
###################sang########################################
sang <- utf8(dbGetQuery(raspi,paste0("select sca_cap_id, concat(sat_type,'-',sal_localisation,'-',sac_contenant,'-',sas_solvant,'-',sca_value) from t_sample_capture_sca, t_capture_cap,listes.tr_samples_contenant_sac,listes.tr_samples_localisation_sal,listes.tr_samples_solvant_sas,listes.tr_samples_types_sat where cap_id = sca_cap_id and sca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and sca_sat_id = sat_id and sca_sal_id = sal_id and sca_sac_id = sac_id and sca_sas_id = sas_id and sat_type ='sang'")))
sang_f<-data.frame()
if (dim(sang)[1] != 0 &&  !is.null(dim(sang)[1])) {
  for (i in unique(sang[,"sca_cap_id"])) {
    if (length(sang[sang[,"sca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("sca_cap_id"= unique(sang[sang[,"sca_cap_id"]==i,"sca_cap_id"]),"sang" = paste0(sang[sang[,"sca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("sca_cap_id"= sang[sang[,"sca_cap_id"]==i,"sca_cap_id"],"sang" = sang[sang[,"sca_cap_id"]==i,"concat"])}
    sang_f <- rbind(sang_f,temp)
  }
  dat<-merge(dat, sang_f, by.x ="cap_id", by.y ="sca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "sang" = "")}            
###################feces########################################
feces <- utf8(dbGetQuery(raspi,paste0("select sca_cap_id, concat(sat_type,'-',sal_localisation,'-',sac_contenant,'-',sas_solvant,'-',sca_value) from t_sample_capture_sca, t_capture_cap,listes.tr_samples_contenant_sac,listes.tr_samples_localisation_sal,listes.tr_samples_solvant_sas,listes.tr_samples_types_sat where cap_id = sca_cap_id and sca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and sca_sat_id = sat_id and sca_sal_id = sal_id and sca_sac_id = sac_id and sca_sas_id = sas_id and sat_type ='feces'")))
feces_f<-data.frame()
if (dim(feces)[1] != 0 &&  !is.null(dim(feces)[1])) {
  for (i in unique(feces[,"sca_cap_id"])) {
    if (length(feces[feces[,"sca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("sca_cap_id"= unique(feces[feces[,"sca_cap_id"]==i,"sca_cap_id"]),"feces" = paste0(feces[feces[,"sca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("sca_cap_id"= feces[feces[,"sca_cap_id"]==i,"sca_cap_id"],"feces" = feces[feces[,"sca_cap_id"]==i,"concat"])}
    feces_f <- rbind(feces_f,temp)
  }
  dat<-merge(dat, feces_f, by.x ="cap_id", by.y ="sca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "feces" = "")}            
###################tiques########################################
tiques <- utf8(dbGetQuery(raspi,paste0("select sca_cap_id, concat(sat_type,'-',sal_localisation,'-',sac_contenant,'-',sas_solvant,'-',sca_value) from t_sample_capture_sca, t_capture_cap,listes.tr_samples_contenant_sac,listes.tr_samples_localisation_sal,listes.tr_samples_solvant_sas,listes.tr_samples_types_sat where cap_id = sca_cap_id and sca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and sca_sat_id = sat_id and sca_sal_id = sal_id and sca_sac_id = sac_id and sca_sas_id = sas_id and sat_type ='tiques'")))
tiques_f<-data.frame()
if (dim(tiques)[1] != 0 &&  !is.null(dim(tiques)[1])) {
  for (i in unique(tiques[,"sca_cap_id"])) {
    if (length(tiques[tiques[,"sca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("sca_cap_id"= unique(tiques[tiques[,"sca_cap_id"]==i,"sca_cap_id"]),"tiques" = paste0(tiques[tiques[,"sca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("sca_cap_id"= tiques[tiques[,"sca_cap_id"]==i,"sca_cap_id"],"tiques" = tiques[tiques[,"sca_cap_id"]==i,"concat"])}
    tiques_f <- rbind(tiques_f,temp)
  }
  dat<-merge(dat, tiques_f, by.x ="cap_id", by.y ="sca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "tiques" = "")}            
###################mucus########################################
vaginal <- utf8(dbGetQuery(raspi,paste0("select sca_cap_id, concat(sat_type,'-',sal_localisation,'-',sac_contenant,'-',sas_solvant,'-',sca_value) from t_sample_capture_sca, t_capture_cap,listes.tr_samples_contenant_sac,listes.tr_samples_localisation_sal,listes.tr_samples_solvant_sas,listes.tr_samples_types_sat where cap_id = sca_cap_id and sca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and sca_sat_id = sat_id and sca_sal_id = sal_id and sca_sac_id = sac_id and sca_sas_id = sas_id and sat_type ='mucus'")))
vaginal_f<-data.frame()
if (dim(vaginal)[1] != 0 &&  !is.null(dim(vaginal)[1])) {
  for (i in unique(vaginal[,"sca_cap_id"])) {
    if (length(vaginal[vaginal[,"sca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("sca_cap_id"= unique(vaginal[vaginal[,"sca_cap_id"]==i,"sca_cap_id"]),"vaginal" = paste0(vaginal[vaginal[,"sca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("sca_cap_id"= vaginal[vaginal[,"sca_cap_id"]==i,"sca_cap_id"],"vaginal" = vaginal[vaginal[,"sca_cap_id"]==i,"concat"])}
    vaginal_f <- rbind(vaginal_f,temp)
  }
  dat<-merge(dat,vaginal_f, by.x ="cap_id", by.y ="sca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "vaginal" = "")} 

Nasal <- utf8(dbGetQuery(raspi,paste0("select sca_cap_id, concat(sat_type,'-',sal_localisation,'-',sac_contenant,'-',sas_solvant,'-',sca_value) from t_sample_capture_sca, t_capture_cap,listes.tr_samples_contenant_sac,listes.tr_samples_localisation_sal,listes.tr_samples_solvant_sas,listes.tr_samples_types_sat where cap_id = sca_cap_id and sca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and sca_sat_id = sat_id and sca_sal_id = sal_id and sca_sac_id = sac_id and sca_sas_id = sas_id and sat_type ='mucus'")))
Nasal_f<-data.frame()
if (dim(Nasal)[1] != 0 &&  !is.null(dim(Nasal)[1])) {
  for (i in unique(Nasal[,"sca_cap_id"])) {
    if (length(Nasal[Nasal[,"sca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("sca_cap_id"= unique(Nasal[Nasal[,"sca_cap_id"]==i,"sca_cap_id"]),"Nasal" = paste0(Nasal[Nasal[,"sca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("sca_cap_id"= Nasal[Nasal[,"sca_cap_id"]==i,"sca_cap_id"],"Nasal" = Nasal[Nasal[,"sca_cap_id"]==i,"concat"])}
    Nasal_f <- rbind(Nasal_f,temp)
  }
  dat<-merge(dat,Nasal_f, by.x ="cap_id", by.y ="sca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "Nasal" = "")}            

mucus <- utf8(dbGetQuery(raspi,paste0("select sca_cap_id, concat(sat_type,'-',sal_localisation,'-',sac_contenant,'-',sas_solvant,'-',sca_value) from t_sample_capture_sca, t_capture_cap,listes.tr_samples_contenant_sac,listes.tr_samples_localisation_sal,listes.tr_samples_solvant_sas,listes.tr_samples_types_sat where cap_id = sca_cap_id and sca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and sca_sat_id = sat_id and sca_sal_id = sal_id and sca_sac_id = sac_id and sca_sas_id = sas_id and sat_type ='mucus'")))
mucus_f<-data.frame()
if (dim(mucus)[1] != 0 &&  !is.null(dim(mucus)[1])) {
  for (i in unique(mucus[,"sca_cap_id"])) {
    if (length(mucus[mucus[,"sca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("sca_cap_id"= unique(mucus[mucus[,"sca_cap_id"]==i,"sca_cap_id"]),"mucus" = paste0(mucus[mucus[,"sca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("sca_cap_id"= mucus[mucus[,"sca_cap_id"]==i,"sca_cap_id"],"mucus" = mucus[mucus[,"sca_cap_id"]==i,"concat"])}
    mucus_f <- rbind(mucus_f,temp)
  }
  dat<-merge(dat,mucus_f, by.x ="cap_id", by.y ="sca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "mucus" = "")}            


dat<- cbind(dat , "remarque" = "")  ###############compléter

###################type collier########################################

col<-  utf8(dbGetQuery(raspi,paste0("SELECT ani_id, cap_id, teq_nom_court 
                              FROM t_animal_ani
                             LEFT JOIN t_capture_cap ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
                             LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
                             LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
                             LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
                             LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
                             LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
                             LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
                             ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;")))
if (dim(col)[1] != 0 &&  !is.null(dim(col)[1])) {
  dat<-merge(dat, col, by.x =c("ani_id", "cap_id"), by.y =c("ani_id", "cap_id"), all.x = TRUE)
}else{dat<- cbind(dat , "Collier" = "")}
names(dat)[length(names(dat))]<- "Collier"

###################capteur accelero########################################
acc<-  utf8(dbGetQuery(raspi,paste0("SELECT ani_id, cap_id, eqt_accelerometrie
                              FROM t_animal_ani
                             LEFT JOIN t_capture_cap ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
                             LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
                             LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
                             LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
                             LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
                             LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
                             LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
                             ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;")))
if (dim(acc)[1] != 0 &&  !is.null(dim(acc)[1])) {
  dat<-merge(dat, acc, by.x =c("ani_id", "cap_id"), by.y =c("ani_id", "cap_id"), all.x = TRUE)
}else{dat<- cbind(dat , "accelero" = "")}
names(dat)[length(names(dat))]<- "accelero"
###################capteur prox########################################
acc<-  utf8(dbGetQuery(raspi,paste0("SELECT ani_id, cap_id,  eqt_proximite
                              FROM t_animal_ani
                             LEFT JOIN t_capture_cap ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
                             LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
                             LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
                             LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
                             LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
                             LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
                             LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
                             ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;")))
if (dim(acc)[1] != 0 &&  !is.null(dim(acc)[1])) {
  dat<-merge(dat, acc, by.x =c("ani_id", "cap_id"), by.y =c("ani_id", "cap_id"), all.x = TRUE)
}else{dat<- cbind(dat , "proximite" = "")}
names(dat)[length(names(dat))]<- "proximite"

###################collier id_usuel########################################
acc<-  utf8(dbGetQuery(raspi,paste0("SELECT ani_id, cap_id, eqt_id_usuel
                              FROM t_animal_ani
                             LEFT JOIN t_capture_cap ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
                             LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
                             LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
                             LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
                             LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
                             LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
                             LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
                             ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;")))
if (dim(acc)[1] != 0 &&  !is.null(dim(acc)[1])) {
  dat<-merge(dat, acc, by.x =c("ani_id", "cap_id"), by.y =c("ani_id", "cap_id"), all.x = TRUE)
}else{dat<- cbind(dat , "id_collier" = "")}
names(dat)[length(names(dat))]<- "id_collier"

###################date début collier########################################
dat_deb<-  utf8(dbGetQuery(raspi,paste0("SELECT ani_id, cap_id, eqa_date_debut
                              FROM t_animal_ani
                             LEFT JOIN t_capture_cap ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
                                 LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
                                 LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
                                 LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
                                 LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
                                 LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
                                 LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
                                 ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;")))
if (dim(dat_deb)[1] != 0 &&  !is.null(dim(dat_deb)[1])) {
  dat<-merge(dat, dat_deb, by.x =c("ani_id", "cap_id"), by.y =c("ani_id", "cap_id"), all.x = TRUE)
}else{dat<- cbind(dat , "date_deb" = "")}
names(dat)[length(names(dat))]<- "date_deb"
###################date fin collier########################################
dat_fin<-  utf8(dbGetQuery(raspi,paste0("SELECT ani_id, cap_id,  eqa_date_fin_text
                              FROM t_animal_ani
                             LEFT JOIN t_capture_cap ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
                             LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
                             LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
                             LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
                             LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
                             LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
                             LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
                             ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;")))
if (dim(dat_fin)[1] != 0 &&  !is.null(dim(dat_fin)[1])) {
  dat<-merge(dat, dat_fin, by.x =c("ani_id", "cap_id"), by.y =c("ani_id", "cap_id"), all.x = TRUE)
}else{dat<- cbind(dat , "date_fin" = "")}
names(dat)[length(names(dat))]<- "date_fin"
###################date fin collier arrondi########################################
dat_fin_ar<-  utf8(dbGetQuery(raspi,paste0("SELECT ani_id, cap_id,  eqa_date_fin
                              FROM t_animal_ani
                             LEFT JOIN t_capture_cap ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
                             LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
                             LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
                             LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
                             LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
                             LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
                             LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
                             ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;")))
if (dim(dat_fin_ar)[1] != 0 &&  !is.null(dim(dat_fin_ar)[1])) {
  dat<-merge(dat, dat_fin_ar, by.x =c("ani_id", "cap_id"), by.y =c("ani_id", "cap_id"), all.x = TRUE)
}else{dat<- cbind(dat , "date_fin arrondie" = "")}
names(dat)[length(names(dat))]<- "date_fin arrondie"

###################date fin capteur suivi 60j jours suivi########################################

dat_fin_cp<-  utf8(dbGetQuery(raspi,paste0("SELECT cap_id,  eqa_date_fin_activite FROM t_capture_cap, public.tj_equipement_animal_eqt_ani_eqa where cap_ani_id= eqa_ani_id and cap_date= eqa_date_debut")))
if (dim(dat_fin_cp)[1] != 0 &&  !is.null(dim(dat_fin_cp)[1])) {
  dat<-merge(dat, dat_fin_cp, by.x = "cap_id", by.y ="cap_id", all.x = TRUE)
}else{dat<- cbind(dat , "date_fin_capteur" = "")}
names(dat)[length(names(dat))]<- "date_fin_capteur"


dat<- cbind(dat , "suivi_GPS oui si>60jours" = "") 
dat<- cbind(dat , "jrs_suivi" = "")

# ###################capteur activité########################################
annee<- year(Sys.Date())-1
a<-append(list.files(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_activite_deplacement/DonneesBrutes/",annee,"/bd_",substr(annee, 3, 4),"a/gps_vectro_15/sensor/")),
                  list.files(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_activite_deplacement/DonneesBrutes/",annee,"/bd_",substr(annee, 3, 4),"a/gsm_lotek/sensor/")))
b<-append(a,list.files(paste0("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_activite_deplacement/DonneesBrutes/",annee,"/bd_",substr(annee, 3, 4),"a/gsm_vectro/sensor")))
b<-gsub("^s","",gsub(".DBF","",gsub("n.dbf","",b)))

# setwd("c:/users/ychaval/Desktop")
# do<-read.csv2("capturesbd_2020_03_27.csv")[,c(2,12)]
# do<-as.character(do[which(do[,2]== 2019),1])
# do<-do[which(do != "")]

###############attention cette partie fonctionne dans l'urgence mais est à modifier
b<-gsub("s","",gsub("j","",gsub("y","",gsub("sm","",gsub("sf","",gsub("mj","",gsub(".DBF","",gsub("n.dbf","",b))))))))
b<-gsub("_19","",gsub("1485","F1485",b))
b<-gsub("_20","",b)
b<-gsub("f_","",b)
b<-gsub("m_","",b)
b<-toupper(gsub("_","",b))

ani<-v2db(dbGetQuery(raspi,paste0("SELECT ani_id from t_animal_ani where ani_etiq in ",v2db(b),""))[,1])
dbSendQuery(raspi,paste0("update tj_equipement_animal_eqt_ani_eqa SET eqa_activite = TRUE where eqa_ani_id in ",ani," and eqa_annee_suivi = ",annee," "))

acc<-  utf8(dbGetQuery(raspi,paste0("SELECT ani_id, cap_id,  eqa_activite, mod_libelle
                              FROM t_animal_ani
                                    LEFT JOIN t_capture_cap ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
                                    LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
                                    LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
                                    LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
                                    LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
                                    LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
                                    LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
                                    ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;")))

# acc[grep("GPS",acc[,"mod_libelle"]),"eqa_activite"] <- TRUE   
# acc[grep("GSM",acc[,"mod_libelle"]),"eqa_activite"] <- TRUE   
# acc[grep("VHF",acc[,"mod_libelle"]),"eqa_activite"] <- FALSE  

if (dim(acc)[1] != 0 &&  !is.null(dim(acc)[1])) {
  dat<-merge(dat, acc, by.x =c("ani_id", "cap_id"), by.y =c("ani_id", "cap_id"), all.x = TRUE)
}else{dat<- cbind(dat , "capteur Activite" = "")}
names(dat)[length(names(dat))]<- "capteur Activite"  
#names(dat)[length(names(dat))]<- "capteur Activite"


###################probleme collier########################################
acc<-  utf8(dbGetQuery(raspi,paste0("SELECT ani_id,cap_id, eqa_probleme
                             FROM t_animal_ani
                             LEFT JOIN t_capture_cap ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
                             LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
                             LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
                             LEFT JOIN t_equipement_conf_eqc ON eqt_id = eqc_eqt_id and eqc_annee_suivi = cap_annee_suivi
                             LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
                             LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
                             LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
                             LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
                             ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;")))
acc<- acc[!is.na(acc[,"eqa_probleme"]),]
if (dim(acc)[1] != 0 &&  !is.null(dim(acc)[1])) {
  dat<-merge(dat, acc, by.x =c("ani_id", "cap_id"), by.y =c("ani_id", "cap_id"), all.x = TRUE)
}else{dat<- cbind(dat , "probleme collier" = "")}
names(dat)[length(names(dat))]<- "probleme collier"

###################site vie secteur########################################
dat<- cbind(dat , "site vie" = "")
dat<- cbind(dat , "secteur" = "")

mort<-dbGetQuery(raspi,paste0("select cap_id, ani_mortalite
                            from t_capture_cap
                            left join t_animal_ani ON cap_ani_id = ani_id
                            "))

if (dim(mort)[1] != 0 &&  !is.null(dim(mort)[1])) {
  dat<-merge(dat, mort, by.x ="cap_id", by.y ="cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "Mort" = "")}
names(dat)[length(names(dat))]<- "Mort"

###################date mort########################################
mort<-utf8(dbGetQuery(raspi,paste0("select cap_id, ani_date_mort_text
                            from t_capture_cap
                            left join t_animal_ani ON cap_ani_id = ani_id")))

if (dim(mort)[1] != 0 &&  !is.null(dim(mort)[1])) {
  dat<-merge(dat, mort, by.x ="cap_id", by.y ="cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "Date mort" = "")}
names(dat)[length(names(dat))]<- "Date mort"
###################date mort arrondie########################################
mort<-utf8(dbGetQuery(raspi,paste0("select cap_id, ani_date_mort
                            from t_capture_cap
                            left join t_animal_ani ON cap_ani_id = ani_id
                            ")))

if (dim(mort)[1] != 0 &&  !is.null(dim(mort)[1])) {
  dat<-merge(dat, mort, by.x ="cap_id", by.y ="cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "Date mort arrondie" = "")}
names(dat)[length(names(dat))]<- "Date mort arrondie"
###################cause mort########################################
mort<-utf8(dbGetQuery(raspi,paste0("select cap_id, ani_cause_mort
                            from t_capture_cap
                            left join t_animal_ani ON cap_ani_id = ani_id
                            ")))
if (dim(mort)[1] != 0 &&  !is.null(dim(mort)[1])) {
  dat<-merge(dat, mort, by.x ="cap_id", by.y ="cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "Cause detaillle" = "")}
names(dat)[length(names(dat))]<- "Cause detaillle"

###################cause categorie########################################
dat<- cbind(dat , "cause categories" = "")

###################poids mort##################
mort<-utf8(dbGetQuery(raspi,paste0("select cap_id, ani_poids_mort
                            from t_capture_cap
                            left join t_animal_ani ON cap_ani_id = ani_id
                            ")))
mort[is.na(mort[,"ani_poids_mort"]),"ani_poids_mort"]<- ""
if (dim(mort)[1] != 0 &&  !is.null(dim(mort)[1])) {
  dat<-merge(dat, mort, by.x ="cap_id", by.y ="cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "Pds mort" = "")}
names(dat)[length(names(dat))]<- "Pds mort"

nm<- dbGetQuery(raspi,"select * from cmpt.t_capture_cpt")
###################nom capteur à dose acepromazine########################################
capteur<- utf8(dbGetQuery(raspi,paste0("select cpt_cap_id, cpt_nom_capteur, cpt_nbre_pers_experimentes, cpt_arrivee_filet_course, cpt_arrivee_filet_panique, cpt_filet_lutte,cpt_filet_haletement,cpt_filet_cri,cpt_dose_acepromazine
                                from cmpt.t_capture_cpt
                                ")))
if (dim(capteur)[1] != 0 &&  !is.null(dim(capteur)[1])) {
  dat<-merge(dat, capteur, by.x ="cap_id", by.y ="cpt_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "nom capteur" = "")}
names(dat)[(length(names(dat))-7) :length(names(dat))]<- c("nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)")
###################num sabot########################################
sab<-utf8(dbGetQuery(raspi,paste0("select cap_id, cap_num_sabot 
                           from t_capture_cap
                           ")))
if (dim(sab)[1] != 0 &&  !is.null(dim(sab)[1])) {
  dat<-merge(dat, sab, by.x ="cap_id", by.y ="cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "num_sabot" = "")}
names(dat)[length(names(dat))]<- "num_sabot"
###################de sabot couche à cpt_tble_cri_synthese########################################
capteur<- utf8(dbGetQuery(raspi,paste0("select cpt_cap_id, cpt_sabot_couche,cpt_sabot_agitation,
                                cpt_sabot_retournement,cpt_hre_fin_surv,
                                cpt_temps_surveillance_mn,cpt_distance_km,
                                cpt_tble_lutte,cpt_tble_halete,
                                cpt_tble_cri_synthese
                                from cmpt.t_capture_cpt
                                ")))
if (dim(capteur)[1] != 0 &&  !is.null(dim(capteur)[1])) {
  dat<-merge(dat, capteur, by.x ="cap_id", by.y ="cpt_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "nom capteur" = "")}
#names(dat)[(length(names(dat))-8) :length(names(dat))]<- c("couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)")
###################T°C 1########################################
temp_anal<-data.frame()
temp_an<- utf8(dbGetQuery(raspi,paste0("select nhca_cap_id, concat('ysi-',nhca_heure_locale_cest,'-',nhca_valeur)  from tj_mesureenum_heure_capture_nhca, tr_variable_mesuree_var where nhca_var_id = var_id and var_nom_court = 'temperature anale_ysi' and nhca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),"")))
if (dim(temp_an)[1] != 0 &&  !is.null(dim(temp_an)[1])) {
  for (i in unique(temp_an[,"nhca_cap_id"])) {
    if (length(temp_an[temp_an[,"nhca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("nhca_cap_id"= unique(temp_an[temp_an[,"nhca_cap_id"]==i,"nhca_cap_id"]),"T°C_ext" = paste0(temp_an[temp_an[,"nhca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("nhca_cap_id"= temp_an[temp_an[,"nhca_cap_id"]==i,"nhca_cap_id"],"T°C_ext" = temp_an[temp_an[,"nhca_cap_id"]==i,"concat"])}
    temp_anal <- rbind(temp_anal,temp)
  }
  dat<-merge(dat, temp_anal, by.x ="cap_id", by.y ="nhca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "T°C 1" = "")}
names(dat)[length(names(dat))]<- "T°C 1"

#####températures anales historiques 
temp_anal<-data.frame()
temp_an<- utf8(dbGetQuery(raspi,paste0("select cpt_cap_id, cpt_tble_temp_animal from cmpt.t_capture_cpt where cpt_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and extract(year from cpt_date) != 2019")))
temp_an<-na.omit(temp_an)

ind <- match(temp_an$cpt_cap_id, dat$cap_id)
dat[,dim(dat)[2]]<-as.character(dat[,dim(dat)[2]])
dat[ind,dim(dat)[2]] <- temp_an[2][,1]
dat[,dim(dat)[2]]<-as.factor(dat[,dim(dat)[2]])

###################T°C 2########################################
dat<- cbind(dat , "T°C 2" = "")
###################coeur 1########################################
cardi<-data.frame()
card<- utf8(dbGetQuery(raspi,paste0("select nhca_cap_id, concat(nhca_heure_locale_cest,'-',nhca_valeur)  from tj_mesureenum_heure_capture_nhca, tr_variable_mesuree_var where nhca_var_id = var_id and var_nom_court = 'rythme cardiaque' and nhca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),"")))
if (dim(card)[1] != 0 &&  !is.null(dim(card)[1])) {
  for (i in unique(card[,"nhca_cap_id"])) {
    if (length(card[card[,"nhca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("nhca_cap_id"= unique(card[card[,"nhca_cap_id"]==i,"nhca_cap_id"]),"T°C_ext" = paste0(card[card[,"nhca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("nhca_cap_id"= card[card[,"nhca_cap_id"]==i,"nhca_cap_id"],"T°C_ext" = card[card[,"nhca_cap_id"]==i,"concat"])}
    cardi <- rbind(cardi,temp)
  }
  dat<-merge(dat, cardi, by.x ="cap_id", by.y ="nhca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "Cœur 1" = "")}
names(dat)[length(names(dat))]<- "Cœur 1"

#####coeur historiques 
cardi<-data.frame()
cardi<- utf8(dbGetQuery(raspi,paste0("select cpt_cap_id, cpt_tble_coeur from cmpt.t_capture_cpt where cpt_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')")," and extract(year from cpt_date) != 2019")))
cardi<-na.omit(cardi)

ind <- match(cardi$cpt_cap_id, dat$cap_id)
dat[,dim(dat)[2]]<-as.character(dat[,dim(dat)[2]])
dat[ind,dim(dat)[2]] <- cardi[2][,1]
dat[,dim(dat)[2]]<-as.factor(dat[,dim(dat)[2]])


###################coeur 2########################################
dat<- cbind(dat , "Cœur 2" = "")
###################ventil########################################
ventil<-data.frame()
vent<- utf8(dbGetQuery(raspi,paste0("select nhca_cap_id, concat(nhca_heure_locale_cest,'-',nhca_valeur)  from tj_mesureenum_heure_capture_nhca, tr_variable_mesuree_var where nhca_var_id = var_id and var_nom_court = 'ventilation' and nhca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),"")))
if (dim(vent)[1] != 0 &&  !is.null(dim(vent)[1])) {
  for (i in unique(vent[,"nhca_cap_id"])) {
    if (length(vent[vent[,"nhca_cap_id"]==i,"concat"]) > 1) {temp <- data.frame("nhca_cap_id"= unique(vent[vent[,"nhca_cap_id"]==i,"nhca_cap_id"]),"ventilation" = paste0(vent[vent[,"nhca_cap_id"]==i,"concat"],collapse = "~"))} else 
    {temp <-data.frame("nhca_cap_id"= vent[vent[,"nhca_cap_id"]==i,"nhca_cap_id"],"ventilation" = vent[vent[,"nhca_cap_id"]==i,"concat"])}
    ventil <- rbind(ventil,temp)
  }
  dat<-merge(dat, ventil, by.x ="cap_id", by.y ="nhca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "ventilation" = "")}
names(dat)[length(names(dat))]<- "ventilation"
###################echographie########################################
echogra<-data.frame()
echo<- utf8(dbGetQuery(raspi,paste0("select nhca_cap_id, nhca_valeur  from tj_mesureenum_heure_capture_nhca, tr_variable_mesuree_var where nhca_var_id = var_id and var_nom_court = 'echographie' and nhca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),"")))
if (dim(echo)[1] != 0 &&  !is.null(dim(echo)[1])) {
  for (i in unique(echo[,"nhca_cap_id"])) {
    if (length(echo[echo[,"nhca_cap_id"]==i,"nhca_valeur"]) > 1) {temp <- data.frame("nhca_cap_id"= unique(echo[echo[,"nhca_cap_id"]==i,"nhca_cap_id"]),"echographie" = paste0(echo[echo[,"nhca_cap_id"]==i,"nhca_valeur"],collapse = "~"))} else 
    {temp <-data.frame("nhca_cap_id"= echo[echo[,"nhca_cap_id"]==i,"nhca_cap_id"],"echographie" = echo[echo[,"nhca_cap_id"]==i,"nhca_valeur"])}
    echogra <- rbind(echogra,temp)
  }
  dat<-merge(dat, echogra, by.x ="cap_id", by.y ="nhca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "echographie" = "")}
names(dat)[length(names(dat))]<- "echographie"
###################echographie_remarque########################################
echogra<-data.frame()
echo<- utf8(dbGetQuery(raspi,paste0("select nhca_cap_id, nhca_remarque  from tj_mesureenum_heure_capture_nhca, tr_variable_mesuree_var where nhca_var_id = var_id and var_nom_court = 'echographie' and nhca_cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),"")))
if (dim(echo)[1] != 0 &&  !is.null(dim(echo)[1])) {
  for (i in unique(echo[,"nhca_cap_id"])) {
    if (length(echo[echo[,"nhca_cap_id"]==i,"nhca_remarque"]) > 1) {temp <- data.frame("nhca_cap_id"= unique(echo[echo[,"nhca_cap_id"]==i,"nhca_cap_id"]),"echographie_remarque" = paste0(echo[echo[,"nhca_cap_id"]==i,"nhca_remarque"],collapse = "~"))} else 
    {temp <-data.frame("nhca_cap_id"= echo[echo[,"nhca_cap_id"]==i,"nhca_cap_id"],"echographie_remarque" = echo[echo[,"nhca_cap_id"]==i,"nhca_remarque"])}
    echogra <- rbind(echogra,temp)
  }
  dat<-merge(dat, echogra, by.x ="cap_id", by.y ="nhca_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "echographie_remarque" = "")}
names(dat)[length(names(dat))]<- "echographie_remarque"
###################remarque table eurodeer########################################
dat<- cbind(dat , "remarque_table" = "")
dat<- cbind(dat , "eurodeer" = "")
###################comportement lache########################################
lache<- utf8(dbGetQuery(raspi,paste0("select cpt_cap_id, cpt_lache_titube,cpt_lache_couche,
                              cpt_lache_course,cpt_lache_tombe,
                              cpt_lache_gratte_collier,cpt_lache_cabriole,
                              cpt_lache_bolide,cpt_lache_aboiement_cri
                              from cmpt.t_capture_cpt
                             ")))
if (dim(lache)[1] != 0 &&  !is.null(dim(lache)[1])) {
  dat<-merge(dat, lache, by.x ="cap_id", by.y ="cpt_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "titube" = "")}
#names(dat)[(length(names(dat))-7) :length(names(dat))]<- c("titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)")
###################comportement heures########################################
heure<- utf8(dbGetQuery(raspi,paste0("select cpt_cap_id, cpt_temps_filet,cpt_temps_sabot_sur_place,
                              cpt_temps_transport_attente,cpt_temps_marquage,
                              cpt_temps_total,cpt_heure_debut_filet,
                              cpt_heure_mise_sabot,cpt_heure_acepro,
                              cpt_heure_debut_transport,cpt_heure_debut_table,
                              cpt_heure_lache
                              from cmpt.t_capture_cpt
                              ")))
if (dim(heure)[1] != 0 &&  !is.null(dim(heure)[1])) {
  dat<-merge(dat, heure, by.x ="cap_id", by.y ="cpt_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "filet" = "")}
#names(dat)[(length(names(dat))-10) :length(names(dat))]<- c("filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache")
###################remarque generale########################################

remarque_gen<- utf8(dbGetQuery(raspi,paste0("select cap_id, ani_remarque
                              from t_capture_cap, t_animal_ani
                               WHERE cap_ani_id = ani_id")))
if (dim(remarque_gen)[1] != 0 &&  !is.null(dim(remarque_gen)[1])) {
  dat<-merge(dat, remarque_gen, by.x ="cap_id", by.y ="cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "remarque_gen" = "")}
names(dat)[length(names(dat))]<-"remarque_gen"


lache<- utf8(dbGetQuery(raspi,paste0("select cpt_cap_id, cpt_tble_cri_bague, cpt_tble_cri_autre,
                              cpt_lache_nbre_stop,cpt_lache_habitat_lache,
                              cpt_lache_habitat_pertevue,cpt_lache_visibilite,
                              cpt_lache_public
                              from cmpt.t_capture_cpt
                              ")))
if (dim(lache)[1] != 0 &&  !is.null(dim(lache)[1])) {
  dat<-merge(dat, lache, by.x ="cap_id", by.y ="cpt_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "filet" = "")}
names(dat)[(length(names(dat))-6) :length(names(dat))]<- c("bague","autre","stop","habitat lacher","habitat perte vue","visibilite","nb_public")

remise_sabot<- dbGetQuery(raspi,paste0("select cpt_cap_id, cpt_tble_remise_sabot
                              from cmpt.t_capture_cpt
                             "))
if (dim(remise_sabot)[1] != 0 &&  !is.null(dim(remise_sabot)[1])) {
  dat<-merge(dat, remise_sabot, by.x ="cap_id", by.y ="cpt_cap_id", all.x = TRUE)
} else {dats<- cbind(dat , "remise sabot" = "")}
names(dat)[length(names(dat))]<- c("remise sabot")


################### heure lache 2 ########################################
lache<- utf8(dbGetQuery(raspi,paste0("select cpt_cap_id, cpt_heure_second_lache
                              from cmpt.t_capture_cpt
                             ")))
if (dim(lache)[1] != 0 &&  !is.null(dim(lache)[1])) {
  dat<-merge(dat, lache, by.x ="cap_id", by.y ="cpt_cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "hre_lacher_2" = "")}
names(dat)[length(names(dat))]<- c("hre_lacher_2")


# dat[,"cap_age_faon"]<- NA
# #dat<-dat[order(dat$Date),]
# 
# age_faon<- dbGetQuery(raspi,paste0("select cap_id, cap_age_faon from t_capture_cap where cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),""))
# age_faon[,2]<-trimws(age_faon[,2])
# if (dim(age_faon)[1] != 0 &&  !is.null(dim(age_faon)[1])) {
#   data<-merge(dat[,1:grep("cap_age_faon",names(dat))-1], age_faon, by.x ="cap_id", by.y ="cap_id", all.x = TRUE)
#   #names(data)[length(names(data))]<- "cap_age_faon"
# } else {data<- cbind(data , "cap_age_faon" = "")}
# dat<-data

age_faon<-  utf8(dbGetQuery(raspi,paste0("select cap_id, cap_age_faon from t_capture_cap where cap_id in ",paste0("('",paste0(dat[,"cap_id"],collapse = "','"),"')"),"")))
if (dim(lache)[1] != 0 &&  !is.null(dim(lache)[1])) {
  dat<-merge(dat, age_faon, by.x ="cap_id", by.y ="cap_id", all.x = TRUE)
} else {dat<- cbind(dat , "cap_age_faon" = "")}
names(dat)[length(names(dat))]<- c("cap_age_faon")

datr<-dat
dat<-dat[,c(3:dim(dat)[2])]
dat<-dat[,c(2:9,1,10:55,57:dim(dat)[2])]
noms_colonnes<- c("N°Animal","ani_nom","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_pertinent","cap_lactation","RFID","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit","glucose","T°C_ext","TIQUES FIXES","autres parasites", "Peau","poils","sang","feces","tiques","vaginal","Nasal","mucus","remarque","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)","num_sabot","couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2","ventilation","echographie","echographie_remarque","remarque_table","eurodeer","titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque_generale","bague","autre","stop","habitat lacher","habitat perte vue","visibilite","nb_public","remise sabot","hre_lacher_2","cap_age_faon")
names(dat)<-noms_colonnes

################### supression espaces ###########################
# dat<-dat[order(dmy(dat[,"Date"])),]
# dat<-apply(dat,2,as.character)
# # dat[which(dat[,"N°Animal"] == "3028"),"N°Animal"]<-"426"
# # dat[which(dat[,"N°Animal"] == "762"),"N°Animal"]<-"418"
# # dat[which(dat[,"N°Animal"] == "892"),"N°Animal"]<-"554"
# # dat[which(dat[,"N°Animal"] == "979"),"N°Animal"]<-"820"
# dat<-apply(dat,2,trimws)
# dat<-as.data.frame(dat)
# 


################### numero telemetrie && nombre capture ###########################
tele <-NA
counn<-NA
dat[,"categorie d'age"]<-trimws(dat[,"categorie d'age"])
for (i in 1:dim(dat)[1]){
  # cap_annee_suivi<-dat[i,"annee  de suivi"]
  # annee<- year(Sys.Date())
  # dif<-annee-cap_annee_suivi
  # clas<-dat[i,"categorie d'age"]
  # classes<-data.frame(c(1:4),c("faon","jeune","yearling","adulte"))
  # if (dif < 4 & !is.na(dat[i,"categorie d'age"])) {if (match(clas, classes[,2])+dif > 4) {dat[i,"categorie d'age"]<- "adulte" }else{dat[i,"categorie d'age"]<-as.character(classes[match(clas, classes[,2])+dif,2])}}
  if (dat[i,"categorie d'age"] == "jeune") {
    cat_age_all <- "jeune" 
    cat_age <- "j"}
  if (dat[i,"categorie d'age"] == "faon") {
    cat_age_all <- "faon" 
    cat_age <- ""}
  if (dat[i,"categorie d'age"] == "yearling") {
    cat_age_all <- "yearling"
    cat_age <- "y" }
  if (dat[i,"categorie d'age"] == "adulte") {
  cat_age_all="adulte"
  cat_age=""}
  #if (is.na(dat[i,"Collier"])){dat[i,"Collier"]<-""}
  if (cat_age != "" & cat_age_all != "faon" & length(grep("GPS|GSM",dat[i,"Collier"])) == 1) { 
  tel<-paste0(tolower(dat[i,"Sexe"]),cat_age,"_",dat[i,"N°Animal"])} else 
  if (cat_age_all == "faon") {tel<- ""} else 
  if (length(grep("GPS|GSM|VHF",dat[i,"Collier"])) == 1) {tel<-paste0(tolower(dat[i,"Sexe"]),"_",dat[i,"N°Animal"])} else {tel<- ""}
  tele <- append(tele,tel)
  coun<- dbGetQuery(raspi,paste0("select count(cap_id) from t_capture_cap, t_animal_ani where ani_id = cap_ani_id and ani_etiq = '", dat[i,1],"' and cap_date <  '", dat[i,"Date"],"'"))
  counn<-append(counn,coun+1)
}
tele<-tele[2:length(tele)]
counn<-na.omit(as.character(unlist(counn)))
dat[,"N°Animal telemetrie"]<- tele
dat[,"Nombre capture"]<- counn

# for (i in 1:dim(dat)[1]){
# if (dat[i,"N°Animal telemetrie"] != "" & length(grep(paste0("^",dat[i,"N°Animal telemetrie"],"$"),dat[,"N°Animal telemetrie"])) != 1) {
#   l<-grep(paste0("^",dat[i,"N°Animal telemetrie"],"$"),dat[,"N°Animal telemetrie"]);
#   for (j in l [2:length(l)]){
    #dat[j,"N°Animal telemetrie"]<- paste0(dat[j,"N°Animal telemetrie"],"_", substr(dat[j,"annee  de suivi"],3,4))
for (i in 1:dim(dat)[1]){
if (dat[i,"N°Animal telemetrie"] != ""){    
dat[i,"N°Animal telemetrie"]<- paste0(dat[i,"N°Animal telemetrie"],"_", substr(dat[i,"annee  de suivi"],3,4))
}}
  # }
  # }}

dat[-grep("vagin",dat[,"vaginal"]),"vaginal"]<- NA
dat[-grep("nez",dat[,"Nasal"]),"Nasal"]<- NA
dat[-grep("conduit auditif",dat[,"mucus"]),"mucus"] <- NA


######a finir
# pos_gps_gsm<-grep("GPS|GSM",dat[,"Collier"])
# tele<-tele[pos_gps_gsm]
# tele_doub<-names(which(table(tele)>1))
# for (i in 1:length(tele_doub)){
#   pos<-grep(paste0("^",tele_doub[i],"$"),dat[,"N°Animal telemetrie"])[2:length(grep(paste0("^",tele_doub[i],"$"),dat[,"N°Animal telemetrie"]))]
#   pos<-na.omit(pos_gps_gsm[match(pos,pos_gps_gsm)])
#   an<-substr(dat[pos, "annee"],3,4)
#   telemetrie<-as.data.frame(cbind(dat[pos, "N°Animal telemetrie"],an))
#   telemetrie<-paste(telemetrie$V1,telemetrie$an,sep="_")
#         dat[pos, "N°Animal telemetrie"]<- telemetrie
# }

########passage des dates au format français   
dat[,"Date"]<-format(as.Date(dat[,"Date"], format = '%Y/%m/%d'),'%d/%m/%Y')
dat[,"date_deb"]<-format(as.Date(dat[,"date_deb"], format = '%Y/%m/%d'),'%d/%m/%Y')
dat[,"date_fin arrondie"]<-format(as.Date(dat[,"date_fin arrondie"], format = '%Y/%m/%d'),'%d/%m/%Y')
dat[,"date_fin_capteur"]<-format(as.Date(dat[,"date_fin_capteur"], format = '%Y/%m/%d'),'%d/%m/%Y')
dat[,"Date mort arrondie"]<-format(as.Date(dat[,"Date mort arrondie"], format = '%Y/%m/%d'),'%d/%m/%Y')

######passsage de booleen a binaire
datr<-dat
#dat<-datr
dat[which(dat[,"capture faon"] != TRUE),"capture faon"]<- "non"
dat[which(dat[,"capture faon"] == TRUE),"capture faon"]<- "oui"
dat[which(dat[,"Mort"] != TRUE),"Mort"]<- "non"
dat[which(dat[,"Mort"] == TRUE),"Mort"]<- "oui"
dat[which(dat[,"capteur Activite"] != TRUE),"capteur Activite"]<- "0"
dat[which(dat[,"capteur Activite"] == TRUE),"capteur Activite"]<- "1"
dat[which(dat[,"accelero"] != TRUE),"accelero"]<- "0"
dat[which(dat[,"accelero"] == TRUE),"accelero"]<- "1"
dat[which(dat[,"proximite"] != TRUE),"proximite"]<- "0"
dat[which(dat[,"proximite"] == TRUE),"proximite"]<- "1"

dat<-apply(dat,2,as.character)
dat<-apply(dat,2,trimws)
dat<-as.data.frame(dat)

dat<- dat[,c("N°Animal","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit", "Peau","poils","sang","feces","tiques","vaginal","Nasal","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)","couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2","titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque_generale","bague","autre","TIQUES FIXES","stop","habitat lacher","habitat perte vue","visibilite","nb_public","glucose","T°C_ext","remise sabot","hre_lacher_2","cap_pertinent","mucus","remarque","Cœur 1","ventilation","echographie","echographie_remarque","ani_nom","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_lactation","RFID","autres parasites","remarque","num_sabot","remarque_table","eurodeer","cap_age_faon")]
names(dat)<-c("N°Animal","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit","Peau","poils","sang","feces","tiques","vaginal","Nasal","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort \"oui\" oui \"    \" non","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur"," nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)"," lutte"," haletement (1/0)","cri (1/0)"," acepromazine (1=0,3cc)","couche (1/0)"," agitation (1/0)"," retournement (1/0)","hre fin surv"," surveillance (mn)"," surveillance (mn)","distance (KM)"," lutte (1/0)"," halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2"," titube (1/0)"," couche (1/0)"," course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)"," bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque","bague","autre","TIQUES FIXES","stop","habitat lacher","habitat perte vue","visibilite","nb_public","glucose","T°C_ext","remise sabot","hre_lacher_2","cap_pert","ecouvillon_oreille","myophos","hre_coeur","respiration","echographie","echographie_remarque","ani_nom","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_lactation","RFID","autres parasites","remarque","num_sabot","remarque_table","eurodeer","cap_age_faon")

dat[,"lache"]<-substr(dat[,"lache"],1,5)
dat<-dat[order(dmy(dat[,"Date"])),]


dat[which(dat[,"cap_pert"] == FALSE),c("Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier")]<- NA

#datf<- dat[-which(dat[,"categorie d'age"] == "faon"),]
dat[,"Poids"]<-str_replace_all(as.character(dat[,"Poids"]),"\\.",",")
dat[,"Cir Cou"]<-str_replace_all(as.character(dat[,"Cir Cou"]),"\\.",",")
dat[,"Long patte Ar"]<-str_replace_all(as.character(dat[,"Long patte Ar"]),"\\.",",")
dat[,"machoire"]<-str_replace_all(as.character(dat[,"machoire"]),"\\.",",")
dat[,"long bois gauche"]<-str_replace_all(as.character(dat[,"long bois gauche"]),"\\.",",")
dat[,"long bois droit"]<-str_replace_all(as.character(dat[,"long bois droit"]),"\\.",",")
dat[,"Pds mort"]<-str_replace_all(as.character(dat[,"Pds mort"]),"\\.",",")
dat[,"acepro"]<-str_replace_all(as.character(dat[,"acepro"]),"\\.",",")


setwd("c:/users/ychaval/Desktop")
#setwd("/home/pi/Desktop/")
write.table(dat , file = paste0("capturesbd_",gsub("-","_",Sys.Date()), ".csv"), append = TRUE, col.names=!file.exists(paste0("captures_",gsub("-","_",Sys.Date()), ".csv")), na="", row.names = F, sep=";", fileEncoding = "UTF-8")

dat[,"Date"]<- dmy(dat[,"Date"])
dat[,"date_deb"]<- dmy(dat[,"date_deb"])
dat[,"date_fin arrondie"]<- dmy(dat[,"date_fin arrondie"])
dat[,"date_fin_capteur"]<- dmy(dat[,"date_fin_capteur"])
dat[,"Date mort arrondie"]<- dmy(dat[,"Date mort arrondie"])

oldOpt <- options()
options(xlsx.date.format="dd/mm/yyyy")
write.xlsx(dat, paste0("capturesbd_",gsub("-","_",Sys.Date()), ".xls"), sheetName = paste0("capturesbd_",gsub("-","_",Sys.Date()),""), col.names = TRUE, row.names = FALSE, append = FALSE, showNA=FALSE)
options(oldOpt) 


equipement<- utf8(dbGetQuery(raspi,"SELECT eqt_id_usuel, teq_nom_court, mod_libelle as eqt_model, mar_libelle as eqt_marque, eqt_accelerometrie::integer::text, 
eqt_proximite::integer::text, eqt_frequence::text
FROM public.t_equipement_eqt, public.tr_type_equipement_teq, public.tr_eqtmarque_mar, public.tr_eqtmodel_mod where teq_id = eqt_teq_id and eqt_mar_id = mar_id and eqt_mod_id = mod_id and teq_nom_court != 'VHF' order by eqt_id_usuel::integer;"))

setwd("/home/pi/Desktop/")
write.table(equipement , file = paste0("equipement_",gsub("-","_",Sys.Date()), ".txt"), na='"NA"', row.names = F, sep=",")
