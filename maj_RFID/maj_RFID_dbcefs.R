#------------------------------------TITRE--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  29/11/2021
#  Version: 1.0.0
#  Description:
#  Documentation:
#
#
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------
mypackages<-c("serial","audio","RPostgreSQL","stringi")
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
#source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
source("C:/Users/ychaval/Documents/BD_CEFS/con_localhost_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/con_serveur_dbgardouch.R")
#-------------------------- chargement de mes fonctions ----------------------
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")

close(trovan)
trovan<-serialConnection("get_rfid", port = "COM5", mode ="9600,n,8,1", newline = 1, translation = "cr", handshake = "xonxoff")
open(trovan)
wait(3)
inf<- "NA";
while (inf != "DONE"){inf<-write.serialConnection(trovan,"N")} ######  Dump the contents of all Lots in reader memory. The ID code is sent with the most significant 2 hexadecimal digits first and the least significant 2 hexadecimal digits last.
wait(10)
res<-read.serialConnection(trovan) ####resultat lecture
codes<-stri_sub(res, base::seq(3, stri_length(res),by=32), length=10)
codes<-codes[1:(grep("K",codes)-1)]

####on enlève les rfid déjà présents dans la table
codes<- codes[-which(!is.na(match(codes,dbGetQuery(local,"Select rfi_tag_code from t_rfid_rfi")[,1])))]

dbSendQuery(local,"insert into t_rfid_rfi (rfi_cap_id, rfi_tag_code, rfi_remarque) values ",paste0("(NULL,'",codes,"',NULL)", collapse = ",")," RETURNING  count(rfi_id)")


#####effacer la memoire
# close(trovan)
# trovan<-serialConnection("get_rfid", port = "COM5", mode ="9600,N,8,1", newline = 1, translation = "cr", handshake = "xonxoff")
# wait(1)
# open(trovan)
# wait(1)
# inf<-write.serialConnection(trovan,"C")
# wait(40)
# res<-read.serialConnection(trovan)
# if (res == "COK") {cat("mémoire effacée")}
# close(trovan)
