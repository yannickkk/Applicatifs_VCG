######################################################################################
###Auteur: Yannick Chaval
###Version: V01
###Date: 14/12/2020
###Objet: ce script de mettre à jour les champs de la bd_cefs
###Définition: il n'est autorisé que pour les membres de cefs_ecriture et cefs_admin
######################################################################################

####en fonction de la colonne sélectionées par l'utilisateur je récupère l'alias de la table
alias <- strsplit(names(input_data)[info$col],"_")[[1]][1]
####je récupère le noms des tables de la bdd
tables<-utf8(dbGetQuery(con,paste0("
SELECT table_name FROM information_schema.tables
WHERE table_type='BASE TABLE';")))[,1]
names(tables)<-tables
####je dissocie le nom des tables à partir de "_"
eclat <- strsplit(tables,"_")

####je réccupère l'alias des tables 
for (i in 1:length(eclat)){
  eclat[[i]] <- eclat[[i]][length(eclat[[i]])]
}
eclat<-unlist(eclat)
####je cherche quelle est la table consernée par la sélection de l'utilisateur
table <-names(eclat)[grep(alias,eclat)]
####je réccupère le nom de la colone consernée par la sélection de l'utilisateur
co<-colnames(input_data)[info$col]

######je réccupère l'identifiant de l'individu concerné
ani_id<- ani_id_tous[info$row]
######je réccupère le cap_id en fonction de l'individu et de la date de capture de la ligne sélectionnées
cap_id<- utf8(dbGetQuery(con,paste0("
SELECT cap_id FROM t_capture_cap WHERE cap_ani_id = ",ani_id," and cap_date = '",cap_date_tous[info$row],"';")))[,1]

######transformation des valeurs si il n'y a rien dans l'entrée utilisateur
if (info$value == ""){val <- "NULL"} else if (is.na(as.numeric(info$value))) {val <- paste0("'",trimws(info$value),"'")} else {val <-as.numeric(info$value)}

####MAJ des champs de la table  t_animal_ani
if (length(grep(table,c("t_animal_ani"))) != 0){
dbSendQuery(con, paste0("update ",table," SET ",co," = ",val," where  ani_id = ",ani_id," "))
#####MAJ à jour des redondances de la table t_animal_ani
d<-utf8(dbGetQuery(con,paste0("
SELECT ani_date_mort,ani_poids_mort,ani_date_mort_text  FROM t_animal_ani WHERE ani_id = ",ani_id,";")))
  
if(is.na(d["ani_date_mort"])){dbSendQuery(con, paste0("update ",table," SET ani_mortalite = FALSE where  ani_id = ",ani_id," "))}
if(is.na(d["ani_poids_mort"])){dbSendQuery(con, paste0("update ",table," SET ani_poids_mort_na = TRUE where  ani_id = ",ani_id," "))}
if(!is.na(d$ani_date_mort) & !is.na(d$ani_date_mort_text)){ if (!is.na(dmy(d$ani_date_mort_text))) {if (dmy(d$ani_date_mort_text) == ymd(d$ani_date_mort)){dbSendQuery(con, paste0("update ",table," SET ani_date_mort_arrondi = FALSE where  ani_id = ",ani_id," "))}}else{dbSendQuery(con, paste0("update ",table," SET ani_date_mort_arrondi = TRUE where  ani_id = ",ani_id," "))}}else{dbSendQuery(con, paste0("update ",table," SET ani_date_mort_arrondi = FALSE where  ani_id = ",ani_id," "))}  
}
###MAJ des champs de la table  t_capture_cap
if (length(grep(table,c("t_capture_cap"))) != 0){
dbSendQuery(con, paste0("update ",table," SET ",co," = ",val," where  cap_id = ",cap_id," ")
            )
}

###MAJ des champs de la table t_capture_cpt
if (length(grep(table,c("t_capture_cpt"))) != 0){
#identifiant<- paste0(prefixe,"_cap_id")
dbSendQuery(con, paste0("update cmpt.",table," SET ",co," = ",val," where  cpt_cap_id= ",cap_id," ")
            )
}

###############pour les administrateurs uniquement: si la valeur de eqc_memoire est changée
###############association d'un animal avec un collier (collier préalablement entré dans la table des equipements et la table des configuration)
if (length(grep(table,c("t_equipement_conf_eqc"))) != 0 & co == "eqc_memoire" & role == "cefs_admin"){
  source("scripts/association_animal_equipement.R")
}

print(info)
# ######pour les noms de sites
# if (co == "sit_nom_court") {
#   sit_nom_court<- utf8(dbGetQuery(con,paste0("
# SELECT distinct(sit_nom_court) FROM public.tr_site_capture_sit;")))[,1]
# }
# if (grep(tolower(trimws(info$value)),tolower(trimws(sit_nom_court))) != 0) {
#   sit_id<- utf8(dbGetQuery(con,paste0("
# SELECT sit_id FROM public.tr_site_capture_sit WHERE sit_nom_court = '",trimws(info$value),"';")))[,1]
# dbSendQuery(con, paste0("update t_capture_cap SET cap_sit_id = ",sit_id," where  cap_id = ",cap_id," "))
# } else {}

