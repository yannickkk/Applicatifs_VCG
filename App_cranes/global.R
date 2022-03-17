
#------------------------------------TITRE--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  06/10/2021
#  version 1.0.2
#  modification: ajout de l'enclos d'entrée en champ mandataire. Lorsque l'animal est nouveau, ce champ crée l'association animal/enclos dans t_asso_ani_enc_aae
#  Description:
#  Documentation:
#
#
#
#
#
#------------------------------------------------------------------------------

#####auto install packages
mypackages<-c("shiny", "RPostgreSQL", "lubridate","shinyjs","shinyalert","data.table", "shinyBS", "shinycssloaders","shinybusy","DT")#"xlsx","kableExtra","knitr"
for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

devtools::source_url("https://github.com/yannickkk/mes_fonctions/blob/main/fonctions_sans_connect.R?raw=TRUE")

#source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/structure_actuelle/con_local_db_gardouch.R")
#con<-local_gardouch
host<<-"pggeodb.nancy.inra.fr"
con<<- dbConnect(PostgreSQL(), host= host, dbname="db_chevreuils", user="ychaval",password="bvta;814")

#####define mandatory fields
fieldsMandatory <- c("ani_etiq","container_parent_identifier","sample_line","sample_column") #

#####function to remove all blank space in responses
noblank<- function(fi) {gsub("[[:space:]]", "", fi)} 

####define refresh time
autoInvalidate <- reactiveTimer(5000)

####function to designate a mandatory label: use labelMandatory() in front of the name of the field to designate it as mandatory
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

####css to have a red star near mandatory (obligatory) fields
appCSS <-".mandatory_star { color: red; }"

####j'initialise l'affichage des données
#registre<<-utf8(as.data.frame(dbGetQuery(con,paste0("SELECT ani_crane, ani_etiq, ani_name,cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal,ani_sexe, ani_mortalite, ani_cause_mort from v_individus_total"))))

