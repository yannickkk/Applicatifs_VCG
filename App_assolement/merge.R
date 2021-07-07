#------------------------------------merge df--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Date:  06/07/2021
#  Description: scrip pour faire un merge des dbf du betail
#  Documentation:
#
#
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------

mypackages<-c("foreign", "sf", "daff")
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
# source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/con_serveur_dbgardouch.R")
#-------------------------- chargement de mes fonctions ----------------------
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")

dfbl<-read.dbf("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_fauche/fauche_2021/BL/assolement_qfield/sig_2021.dbf")
dftab1<-read.dbf("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_fauche/fauche_2021/tab1/sig_2021.dbf")

####verif doublons de recno
which(table(dfbl$RECNO) != 1)
which(table(dftab1$RECNO) != 1)

####regarder les différences
render_diff(diff_data(dfbl[,c("RECNO","OS_21","GRD_CAT_21")],dftab1[,c("RECNO","OS_21","GRD_CAT_21")],id="RECNO"))

dft<-merge(dfbl[,-grep("WEEK_FAUCH",names(dfbl))],dftab1[,c("RECNO","OS_21", "WEEK_FAUCH", "REMARQUE")], by = "RECNO")

dft<-apply(dft,2,as.character)
dft[which(dft[,"OS_21.x"] == "hors zone"),"OS_21.x"]<- dft[which(dft[,"OS_21.x"] == "hors zone"),"OS_21.y"]
dft[which(dft[,"OS_21.x"] == "non renseigne"),"OS_21.x"]<- dft[which(dft[,"OS_21.x"] == "non renseigne"),"OS_21.y"]

new <- dft[,"OS_21.x"]
dft<-cbind(dft,"OS_21" = new)
dft[which(dft[,"OS_21.x"] != dft[,"OS_21.y"] & dft[,"OS_21.y"] != "hors zone"),"OS_21"] <- "verif"

length(which(dft[,"OS_21"] == "verif"))

#out<-diff_data(mydata1,mydata2,id="name")
#write_diff(out,"D:/RStudio/daff/Result.csv")

write.dbf(dft, "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_fauche/fauche_2021/merged/sig_2021.dbf")