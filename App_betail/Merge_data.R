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

mypackages<-c("foreign", "sf")
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

df30<-read.dbf("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/Outputs/app/betail_30/paturage_2021_30.dbf")
df40<-read.dbf("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/Outputs/app/betail_40/paturage_2021_40.dbf")
df50<-read.dbf("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/Outputs/app/betail_50/paturage_2021_50.dbf")

dfst<-merge(df30,df40[,append(grep("GID",names(df40)),grep("SEMAINE",names(df40)))], by = "GID")
dft<-merge(dfst,df50[,append(grep("GID",names(df50)),grep("SEMAINE",names(df50)))], by = "GID")

write.dbf(dft, "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/Outputs/app/merged/paturage_2021.dbf")