library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(shinyBS)
library(shinyTime)
library(RPostgreSQL)
library(shinyalert)
library(chron)
library(shinyWidgets)
library(shinyjs)
library(V8)
library(stringr)
library(serial)
library(audio)

source("connect.R")
if(exists("cap_id")){rm(cap_id)}
noms_colonnes<- c("N°Animal","ani_nom","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_pertinent","cap_lactation","RFID","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit","glucose","T°C_ext","TIQUES FIXES","autres parasites", "Peau","poils","sang","feces","tiques","vaginal","Nasal","remarque","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)","num_sabot","couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2","remarque_table","localisation sonde temperature","eurodeer","titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque_generale","bague","autre","stop","habitat lacher","habitat perte vue","visibilite","nb_public","eurodeer_lacher","remise sabot","hre_lacher_2")

dbSendQuery(con, paste0("update listes.tr_sabots_sab set sab_disponible = DEFAULT"))
#dbSendQuery(con,paste0("UPDATE public.t_equipement_conf_eqc SET eqc_pose = DEFAULT"))

choix<- list()
choix[["visibilite"]]<-c(choisir = "", "0-10","11-50","51-100",">100","Nuit")
choix[["habitat"]]<-c(choisir = "", dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_lache) from cmpt.t_capture_cpt order by cpt_lache_habitat_lache ASC")[,1])
choix[["habitat_perte"]]<-c(choisir = "", dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_pertevue) from cmpt.t_capture_cpt order by cpt_lache_habitat_pertevue ASC")[,1])
choix[["tiques"]]<-c(choisir = "",0:30,'>30')
choix[["diarrhee"]]<-c(choisir = "", c(TRUE,FALSE))
choix[["lactation"]]<-c(choisir = "", "oui", "non", "indetermine")
choix[["Nbre_pers_experimentes"]]<-c(choisir = "", 0:5)
choix[["sexe"]]<-c(choisir = "","M","F")
choix[["names_oui_non"]]<-c("NA","Oui","Non")
choix[["values_oui_non"]]<-c("NA",1,0)
choix[["nbre_personnes"]]<-c(choisir = "", "4-5","6-10","11-20", "21-50",">50")
####ancienne notation avec selectizeInput pas besoin de valeur nulle car il y a un placeholder dans les options
#choix[["Notation_euro"]]<-dbGetQuery(con,"select (ecl_comportement_lache) from listes.tr_eurodeer_comp_lache_ecl")
choix[["Notation_euro"]]<-c(choisir = "", dbGetQuery(con,"select (ecl_comportement_lache) from listes.tr_eurodeer_comp_lache_ecl")[,1])
choix[["Notation_euro_table"]]<-c(choisir = "", dbGetQuery(con,"select (ect_comportement) from listes.tr_eurodeer_comp_table_ect")[,1])
#choix[["numSabot_capture"]]<-dbGetQuery(con,paste0("select toto.sab_valeur from (select distinct sab_id, sab_valeur FROM listes.tr_sabots_sab order by sab_id) as toto"))
choix[["idRFID"]]<-c(choisir = "", dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null ORDER BY rfi_tag_code ASC")[,1])
choix[["idSite"]]<-c(choisir = "", dbGetQuery(con,"select sit_nom_court from public.tr_site_capture_sit where sit_actif = TRUE")[,1])
choix[["nAnimal2"]]<-c(choisir = "", dbGetQuery(con,"select ani_etiq from public.t_animal_ani order by ani_id DESC")[,1])
choix[["cirCou"]]<-dbGetQuery(con,"select max(cap_circou) from t_capture_cap")
choix[["lPattArriere"]]<-dbGetQuery(con,"select max(cap_lpa) from t_capture_cap")
choix[["lBoisGauche"]]<-dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")
choix[["lBoisDroit"]]<-dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")
choix[["etatBois"]]<-dbGetQuery(con,"select distinct etb_description from listes.tr_etat_bois_etb order by etb_description")
choix[["idTagOrG2"]]<-c(choisir = "", dbGetQuery(con, "select distinct cap_tag_gauche from public.t_capture_cap")[,1])
choix[["idTagOrD2"]]<-c(choisir = "", dbGetQuery(con, "select distinct cap_tag_droit from public.t_capture_cap")[,1])
choix[["idRFID2"]]<-c(choisir = "", dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id  ORDER BY rfi_tag_code ASC")[,1])
choix[["idSite2"]]<-c(choisir = "", dbGetQuery(con, "select sit_nom_court from public.tr_site_capture_sit where (sit_id in (select cap_sit_id from public.t_capture_cap, t_animal_ani where sit_actif = TRUE))")[,1])
choix[["idRFID_new"]]<-c(choisir = "", dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null  ORDER BY rfi_tag_code ASC")[,1])
choix[["age"]]<-c(choisir = "", "0.5" = "<1", "1.5" = "1", "2.5" = "2", "3.5" = "3", "4.5-5.5"= "4-5", ">6.5" = ">=6")
choix[["numSabot"]]<-c(choisir = "", dbGetQuery(con,"select sab_valeur from listes.tr_sabots_sab where sab_disponible order by sab_id")[,1])
# choix[["position_temp1"]]<-dbGetQuery(con,"select tel_localisation from listes.tr_temperatures_localisation_tel")
# choix[["position_temp2"]]<-dbGetQuery(con,"select tel_localisation from listes.tr_temperatures_localisation_tel")
choix[["cpt_dose_acepromazine"]]<- c(choisir = "", dbGetQuery(con,"select distinct cpt_dose_acepromazine from cmpt.t_capture_cpt order by cpt_dose_acepromazine")[,1])
choix[["cribague"]]<-c("NA","0", "1-2", ">2")
choix[["criautre"]]<-c("NA","0", "1-2", ">2")
choix[["vitesse"]]<-c("NA","Pas","Course")
choix[["allure"]]<-c("NA","Reflechi","Bolide")
choix[["values_vitesse"]]<-c("NA",0,1)
choix[["values_allure"]]<-c("NA",0,1)
choix[["cpt_filet_lutte"]]<-c("NA",0,1,2)
choix[["nbre_echant"]]<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
choix[["prel"]]<-rbind(rep("",4),dbGetQuery(con, "select sat_type, sal_localisation, sac_contenant, sas_solvant from listes.tr_samples_types_sat, listes.tr_samples_contenant_sac, listes.tr_samples_localisation_sal, listes.tr_samples_solvant_sas where sat_id=sac_sat_id and sat_id=sal_sat_id and sat_id=sas_sat_id and sac_id=sas_sac_id"))
choix[["mes_heure"]]<-c(choisir = "", dbGetQuery(con,"select var_nom_court from tr_variable_mesuree_var where var_nom_court in ('temperature anale_ysi','temperature ext_ysi','rythme cardiaque','ventilation','echographie')")[,1])
choix[["booleens"]]<-c(choisir = "", dbGetQuery(con,enc2utf8("select var_nom_court from tr_variable_mesuree_var where var_nom_court in ('myophos','lidocaïne')"))[,1])


######extraire la partie decimal d'un reel
frac <- function(x) abs( x - trunc(x))

#######hour decimal (result of difftime in hours) to hh:mm:ss
hour_conv <- function(x){
h<-trunc(x); if (nchar(h) == 1) {h<-paste0("0",h,"")}; m<-frac(x)*60;s<-round(frac(m)*60); if (nchar(s) == 1) {s<-paste0("0",s,"")};if (nchar(m) == 1) {m<-paste0("0",m,"")} else {m<- trunc(m)}
y<-paste0(h,":",m,":",s,"")
return(y)
}
#decomment for rapberry pi
tousb<-"/media/pi/9296-590E/App_capture_ad/csv"
tosd<-"/home/pi/Desktop/App_capture_ad/csv"

###mise a jour heure raspberry a partir de l''orloge temps reel
system("sudo hwclock -s")

#decomment for pc
#tousb<-"H:/captures_chevreuils"
# tousb<-"C:/Users/ychaval/Documents/BD_tools/saisie_capture_shiny/Outputs/csv"
# tosd<-"C:/Users/ychaval/Documents/BD_tools/saisie_capture_shiny/Outputs/csv"
