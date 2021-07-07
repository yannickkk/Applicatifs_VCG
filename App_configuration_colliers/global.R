#------------------------------------TITRE--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  07/07/2021
#  Description:
# Documentation:
#
# https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
# https://deanattali.com/blog/shiny-persistent-data-storage/
# https://stackoverflow.com/questions/19265825/clear-text-input-after-submit
# https://thinkr.fr/dockeriser-application-shiny/
#
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------
mypackages<-c("shiny", "shinyjs","lubridate","RPostgreSQL","data.table","shinyalert","shinybusy")
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
# source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/con_serveur_dbgardouch.R")
#-------------------------- chargement de mes fonctions ----------------------
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")

con<-serveur
#####define mandatory fields
fieldsMandatory <- c("eqt_id_usuel", "eqt_teq_id", "eqt_mar_libelle", "eqt_mod_libelle") ####define mandatory fields
fieldsMandatoryconf <-c("eqt_id_usuel_conf", "eqc_annee_suivi", "sen_association", "eqc_memoire") ####define mandatory fields

#####define fields to export into the csv file and to update in db
fieldsAll <- c("eqt_id_usuel","eqt_frequence", "eqt_teq_id", "eqt_mar_libelle", "eqt_mod_libelle")
fieldsAllconf <-c("eqt_id_usuel_conf", "eqc_drop_off","eqc_annee_suivi", "eqc_couleur_boitier", "eqc_couleur_collier", "sen_association", "eqc_memoire", "eqc_remarque")

#####path way to the directory to stores results
responsesDir <- file.path("C:/Users/ychaval/Documents/BD_tools/Shinyforms/Programmes/colliers/donnees")

#####function to remove all blank space in responses
noblank<- function(fi) {gsub("[[:space:]]", "", fi)} 

####define refresh time
autoInvalidate <- reactiveTimer(5000)

###### make a dataframe with the different options
#######case 1 : without db
# dff <- data.frame(option1 = c("VHF","GPS", "GPS","GPS", "GSM", "GSM"),
#                  option2 = c("Lotek","Lotek","Lotek","VECTRONIC Aerospace GmbH","Lotek","VECTRONIC Aerospace GmbH"),
#                  option3 = c("NA","GPS 3300 Revision 1","GPS 3300 Revision 2","GPS PLUS-1C Store On Board","GSM Small WildCell","GPS PLUS Mini-1C (GSM)")
# )
#######case 2 : from db
df <- data.frame(dbGetQuery(con, "select teq_nom_court from public.tr_eqtmodel_mod, public.tr_type_equipement_teq where teq_id = mod_teq_id"),
                 dbGetQuery(con, "select mar_libelle from public.tr_eqtmodel_mod, public.tr_eqtmarque_mar where mar_id = mod_mar_id"),
                 dbGetQuery(con, "select mod_libelle from public.tr_eqtmodel_mod")
)
colnames(df)<-c("option1","option2","option3")

####function to designate a mandatory label: use labelMandatory() in front of the name of the field to designate it as mandatory
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

####css to have a red star near mandatory (obligatory) fields
appCSS <-".mandatory_star { color: red; }"

####formate un vecteur R en un vecteur sql
v2dbn<- function(x) {
  x<-paste0("('",paste(x,collapse ="','"),"')")
  return(x)}

utf8 <- function(x) {
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
  # Same on column names:
  Encoding(names(x)) <- "UTF-8"
  return(x)
}