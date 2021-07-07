######################################################################################
###Auteur: Yannick Chaval
###Version: V01
###Date: 14/12/2020
###Objet: Si un membre de cefs_admin ajoute un individu avec un ani_etiq existant ou modifie un ani_etiq d'une ligne existante avec un ani_etiq existant alors l'appli crée une nouvelle capture pour cet individu à la date du jour

# if (conf == "faons"){ ####si cette ligne (et la dernière) sont décommentées alors on ne peut créer une capture que pour la configuration 'faons'
if (info$value != "NULL") {
ani_id <- utf8(dbGetQuery(con,paste0(" SELECT ani_id from v_individus_total where ani_etiq = '",info$value,"'")))[,1]
dbSendQuery(con,paste0("INSERT INTO t_capture_cap (cap_ani_id, cap_date, cap_sit_id, cap_bague, cap_annee_suivi, cap_faon, cap_age_classe) values (",ani_id,",DATE(now()), 1,concat('",info$value,"','_',SUBSTRING(DATE(now())::varchar FROM 3 FOR 2)),SUBSTRING(DATE(now())::varchar FROM 1 FOR 4)::integer, TRUE, 'faon');"))
}
# }
