######################################################################################
###Auteur: Yannick Chaval
###Version: V01
###Date: 14/12/2020
#Est lancé par le script update_field.R si
# la table est t_equipement_conf_eqc et
# la colonne est co == "eqc_memoire" et
# le role == "cefs_admin"
#########alors on met à jour les informations d'assiociation collier_animal pour la capture donnée
###############################################################

######quelle annee de suivi ?
cap_annee_suivi<-dbGetQuery(con,paste0("
  SELECT cap_annee_suivi FROM t_capture_cap WHERE cap_id = ",cap_id," "))
######quel équipement pour l'année et la mémoire choisies ?
eqt_id<-dbGetQuery(con,paste0("
SELECT eqc_eqt_id FROM public.t_equipement_conf_eqc WHERE ",co," = ",trimws(info$value)," and eqc_annee_suivi = ",cap_annee_suivi,";"))

###### si l'année de suivi et l'équipement existent

if (dim(eqt_id)[1] != 0 & dim(cap_annee_suivi)[1] != 0) {
  
#######comme cette maj concernera les faons, on met la date début à la date de capture et la date de fin à la date de mort
  eqa_date_debut<-cap_date_tous[info$row]
  eqa_date_fin<-ani_mort_tous[info$row]
  
####on insère la nouvelle association animal_collier_annee_suivi avec la date de capture comme date de début et la date de mort comme date de fin
####dans la table des associations animal_collier si pour un animal à une date donnée il y a déjà un collier alors on change le collier sinon on rajoute une association
  if(is.na(eqa_date_fin)) {eqa_date_fin <- "NULL"} else if (is.null(eqa_date_fin)) {eqa_date_fin <- "NULL"} else if (nchar(eqa_date_fin) == 0){eqa_date_fin <- "NULL"} else {eqa_date_fin <- paste0("'",trimws(eqa_date_fin),"'")}
dbSendQuery(con, paste0("INSERT INTO public.tj_equipement_animal_eqt_ani_eqa (eqa_ani_id,eqa_eqt_id,eqa_date_debut,eqa_date_fin,eqa_date_fin_arrondi,eqa_annee_suivi) VALUES (",ani_id,",",eqt_id,",'",eqa_date_debut,"',",eqa_date_fin,",FALSE,",cap_annee_suivi,") ON CONFLICT (eqa_ani_id,eqa_date_debut) DO UPDATE SET (eqa_eqt_id,eqa_date_fin,eqa_date_fin_arrondi,eqa_annee_suivi)=(EXCLUDED.eqa_eqt_id,EXCLUDED.eqa_date_fin,EXCLUDED.eqa_date_fin_arrondi,EXCLUDED.eqa_annee_suivi)")) 
dbSendQuery(con, paste0("UPDATE public.tj_equipement_animal_eqt_ani_eqa set eqa_date_fin_arrondi = ani_date_mort_arrondi FROM t_animal_ani WHERE eqa_ani_id = ani_id and eqa_annee_suivi = ",cap_annee_suivi," and ani_id=  ",ani_id,"") )
#dbSendQuery(con, paste0("UPDATE public.tj_equipement_animal_eqt_ani_eqa set eqa_date_fin = eqa_date_debut where eqa_ani_id = ",ani_id," order by ") )

}
