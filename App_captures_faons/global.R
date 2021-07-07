#https://stackoverflow.com/questions/52427281/add-and-delete-rows-of-dt-datatable-in-r-shiny
### https://yihui.shinyapps.io/DT-edit/
##https://rstudio.github.io/DT/shiny.html
####listes inside data table
###https://stackoverflow.com/questions/61672626/r-shiny-using-dt-package-to-edit-a-table-in-a-drop-down-list-and-update-the-tab
###html widget in datatable https://stackoverflow.com/questions/55034483/shiny-widgets-in-dt-table/55041373#55041373
######ref r studio
###https://shiny.rstudio.com/reference/shiny/0.11/dateInput.html

#####callback DT editable
##https://stackoverflow.com/questions/57215607/render-dropdown-for-single-column-in-dt-shiny



library(shiny)
library(dplyr)
library(DT)
library(RPostgreSQL)
library(data.table)
library(lubridate)
library(shinyalert)
library(magrittr)
library(shinythemes)
library(shinybusy)
library(shinyTime)
#library(chron)
#library(anytime)

rm(list=ls())
source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")

#source("recreate_v_individus_total.R")

setwd("C:/Users/ychaval/Documents/BD_tools/Shinyforms/Programmes/edit_table/from_v_individus_total")

input_data <- utf8(dbGetQuery(con,paste0("SELECT * from v_individus_total order by cap_date, ani_id")))
nm<-names(input_data)

animal<-c("ani_etiq","ani_sexe","ani_mortalite","ani_date_mort","ani_date_mort_arrondi","ani_date_mort_text","ani_poids_mort","ani_cause_mort","ani_poids_mort_na","ani_remarque","ani_mort_x","ani_mort_y","ani_inconnu","ani_name","ani_mere_observee","ani_mere_pedigree","ani_pere_pedigree","ani_fratrie","ani_fratrie_oct","ani_surv_faon_marque_oct","ani_agres_faon_marque","ani_cause_fin_suivi","ani_cause_fin_suivi_remarque","ani_date_fin_suivi","ani_cause_mort_classes","ani_necropsie","ani_crane","ani_lpa_mort","ani_remarque_suivi","ani_dernier_contact","ani_date_fin_suivi_text")
capture<-c("ani_etiq","ani_sexe","rfi_tag_code","cap_bague","sit_nom_court","cap_date","cap_annee_suivi","cap_faon","cap_age","cap_age_corrige","cap_age_classe","cap_poids","cap_circou","cap_lpa","cap_etat_sante","cap_heure_lacher","cap_proximite_contact","cap_pertinent","cap_num_sabot","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_age_faon","cap_telemetrie","cap_date_fin_capteur","cap_ect_id","cap_ecl_id", "eqc_couleur_collier", "eqc_couleur_boitier","eqc_memoire","sen_association")
comportement_adulte<-c("ani_etiq","ani_sexe,cap_bague","sit_nom_court,cap_annee_suivi","cpt_nom_capteur","cpt_nbre_pers_experimentes","cpt_arrivee_filet_course","cpt_arrivee_filet_panique","cpt_filet_lutte","cpt_filet_haletement","cpt_filet_cri","cpt_dose_acepromazine","cpt_sabot_couche","cpt_sabot_agitation","cpt_sabot_retournement","cpt_hre_fin_surv","cpt_temps_surveillance_mn","cpt_distance_km","cpt_tble_lutte","cpt_tble_halete","cpt_tble_cri_synthese","cpt_tble_cri_bague","cpt_tble_cri_autre","cpt_tble_temp_animal","cpt_tble_temp_exterieur","cpt_tble_coeur","cpt_tble_remise_sabot","cpt_table_eurodeer","cpt_lache_titube","cpt_lache_couche","cpt_lache_course","cpt_lache_tombe","cpt_lache_gratte_collier","cpt_lache_cabriole","cpt_lache_bolide","cpt_lache_aboiement_cri","cpt_lache_nbre_stop","cpt_lache_habitat_lache","cpt_lache_habitat_pertevue","cpt_lache_visibilite","cpt_lache_public","cpt_lache_eurodeer","cpt_temps_filet","cpt_temps_sabot_sur_place","cpt_temps_transport_attente","cpt_temps_marquage","cpt_temps_total","cpt_heure_debut_filet","cpt_heure_mise_sabot","cpt_heure_acepro","cpt_heure_debut_transport","cpt_heure_debut_table","cpt_heure_lache","cpt_heure_second_lache","cpt_remarque","cpt_tble_ventilation")
#comportement_faon<-c("ani_etiq","ani_sexe","ani_name","sit_nom_court","cap_date","cap_annee_suivi","avant_capture_actif","avant_capture_cri","avant_capture_etat_faon","avant_capture_faon_visible","avant_capture_gite","avant_capture_cri_detresse","avant_capture_mere_gite","avant_capture_mere_vis","avant_capture_position_init","avant_capture_reaction","capture_heure", "capture","capture_cri","pesee_agitation","entre_manip_agitation","entre_manip_cri","apres_lacher_latence","apres_lacher_activite","apres_lacher_comportement","apres_lacher_parti","apres_lacher_vocalise")
comportement_faon<-c("ani_etiq","ani_sexe","ani_name","sit_nom_court","cap_date","cap_annee_suivi","capture_heure","avant_capture_gite","avant_capture_faon_visible","avant_capture_mere_vis","avant_capture_actif","avant_capture_mere_gite","avant_capture_cri","avant_capture_position_init","avant_capture_cri_detresse","avant_capture_reaction", "capture","avant_capture_etat_faon","capture_cri","pesee_agitation","entre_manip_agitation","entre_manip_cri","apres_lacher_latence","apres_lacher_activite","apres_lacher_comportement","apres_lacher_parti","apres_lacher_vocalise")

capture_faon<-c("ani_etiq","ani_sexe","ani_name","ani_mere_observee","ani_fratrie","sit_nom_court","cap_date","ani_date_mort", "cap_poids","cap_circou","cap_lpa","cap_etat_sante","cap_heure_lacher","cap_tag_droit","cap_tag_gauche","cap_age_faon","eqc_memoire")

# , "eqc_couleur_collier,"eqc_couleur_boitier","eqc_memoire","sen_association","avant_capture_actif","avant_capture_cri","avant_capture_etat_faon","avant_capture_faon_visible","avant_capture_gite","avant_capture_cri_detresse","avant_capture_mere_gite","avant_capture_mere_vis","avant_capture_position_init","avant_capture_reaction","capture","capture_cri","pesee_agitation","entre_manip_agitation","entre_manip_cri","apres_lacher_latence","apres_lacher_activite","apres_lacher_comportement","apres_lacher_parti","apres_lacher_vocalise") 

#tester si le user and password existe et si appartient à écriture ou admin autorisation TRUE sinon FALSE
autorisation <-TRUE
editable <- TRUE
role <-"cefs_admin"
#autorisation <- FALSE
#editable <- FALSE
#role <-"cefs_lecture"
####subset<<-seq.int(1,dim(input_data)[1])
this_table<-reactiveVal(input_data)
####save_search<-list(NA)
####quels sont les champs qu'il ne faut pas pouvoir éditer
noneditable_base<-c("ani_mortalite","ani_date_mort_arrondi","ani_inconnu","ani_poids_mort_na")
noneditable_ecriture<-append(noneditable_base,c("eqc_memoire","ani_etiq","ani_mere_pedigree","ani_pere_pedigree","ani_crane","ani_dernier_contact","rfi_tag_code"))
#row_selected <- 0
rows<-0
cols<-0
formers_rows<- 0
selected<-matrix(c(0,0),1,2)
va<- NULL
allsel<- NA ####initialise le collecteur de lignes sélectionnées (un vecteur qui stocke les identifiants des lignes sélecionnées)
age<- c("adulte","jeune","yearling","faon")
#action<-NULL
##############
# mortauxcons <- function () {
# 
#   all_cons <- dbListConnections(PostgreSQL())
# 
#   for(con in all_cons)
#     +  dbDisconnect(con)
# 
#   print(paste(length(all_cons), " connections killed."))
# 
# }