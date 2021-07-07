#######################################################
###Auteur: Yannick Chaval
###Version: V01
###Date: 14/12/2020
###Objet: ce script permet de transmettre les droits de la bdd aux utilisateurs de l'appli
###Définition: les droits de l'appli, il y a trois niveaux
############lecture: peut visaliser, sélectionner, exporter
############écriture: à les mêmes droits que lecture mais peut corriger les lignes existantes. eqc_memoire ne peut pas être changé par ce profil
############admin: à les mêmes droits qu'écriture mais peut créer un nouvel individu et/ou une nouvelle capture
#########################################################

#####ON ESSAYE DE SE CONNECTER AVEC LES INFORMATIONS DONNEES PAR L'UTILISATEUR
test<-tryCatch(dbConnect(PostgreSQL(), host="localhost", dbname="db_chevreuils", user=login, password=password), error = function(cond) {"error"})

#test<-tryCatch(dbConnect(PostgreSQL(), host="pggeodb.nancy.inra.fr", dbname="db_cefs", user=login, password=password), error = function(cond) {"error"})
autorisation <- FALSE

#####Si la connection fonctionne 
if (class(test) == "PostgreSQLConnection") {
####j'essaye d'interrompre tous les processus de la bd_chevreuils pour éviter d'avoir 16 connection ouvertes  
# dbGetQuery(con, "
# SELECT pg_terminate_backend(pg_stat_activity.pid)
#              FROM pg_stat_activity
#              WHERE pg_stat_activity.datname = 'db_chevreuils' -- ← change this to your DB
#              AND pid <> pg_backend_pid();
#              ")
dbDisconnect(con)
####on crée la nouvelle connection
 con<- dbConnect(PostgreSQL(), host="localhost", dbname='db_chevreuils', user= login, password= password)
 #con<- dbConnect(PostgreSQL(),host="pggeodb.nancy.inra.fr", dbname="db_cefs", user= login, password= password)
####réccupère les roles auquel l'utilisateur appartient 
 role <-dbGetQuery(con,paste0("Select rolname from pg_user join pg_auth_members on (pg_user.usesysid=pg_auth_members.member) join pg_roles on (pg_roles.oid=pg_auth_members.roleid)  where usename ='",login,"' and rolname in ('cefs_admin','cefs','cefs_ecriture','cefs_lecture')"))[,1]
####l'autorisation devient TRUE si l'utilisateur est dans le role écriture
 autorisation <<- !is.na(match("cefs_ecriture",role))
####Si l'utilisateur n'est dans le role écriture mais qu'il est dans le role admin alors l'autorisation est TRUE également
 if (autorisation == FALSE) {autorisation <<- !is.na(match("cefs_admin",role))}
####Si l'utilisateur est dans le role admin alors role prend la valeur admin (le seul role qui a les droit d'utiliser add)
 if (!is.na(match("cefs_admin",role))) {role<<-"cefs_admin"}
 #####Si l'utilisateur est membre d'écriture il n'a pas le droit d'éditer ni ani_etiq ni eqc_memoire (car ce dernier associe un individu avec un collier voir: association_animal_equipement.R)
 if (autorisation == TRUE & role != "cefs_admin"){
   noneditable<-noneditable_ecriture}
 if (autorisation == TRUE & role == "cefs_admin"){
   noneditable<-noneditable_base}
}
#####Si la connection ne fonctionne pas = message d'erreur 
if (class(test) != "PostgreSQLConnection") {
  shinyalert("Connection Impossible", enc2utf8("vérifier vos identifiants"), type = "error")
}


