#------------------------------------integration des donnees betails--------------------------------------
# Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
# Date:  13/12/2021
# Version: 1.0.0
# Description:
# Documentation:
#
#
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------
mypackages<-c("lubridate","sf","RPostgreSQL","data.table", "foreign","reshape2")
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
#source("C:/Users/ychaval/Documents/BD_CEFS/con_localhost_dbcefs.R")
#source("C:/Users/ychaval/Documents/BD_Gardouch/Programmes/R/con_serveur_dbgardouch.R")
#-------------------------- chargement de mes fonctions ----------------------
source("C:/Users/ychaval/Documents/BD_tools/Mes_fonctions_R/fonctions.R")
#-------------------------- definition lambert93 :from https://epsg.io/2154 OGC WKT----------------------
wkt<-"PROJCS[\"RGF93 / Lambert-93\",GEOGCS[\"RGF93\",DATUM[\"Reseau_Geodesique_Francais_1993\",SPHEROID[\"GRS 1980\",6378137,298.257222101,AUTHORITY[\"EPSG\",\"7019\"]],TOWGS84[0,0,0,0,0,0,0],AUTHORITY[\"EPSG\",\"6171\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4171\"]],PROJECTION[\"Lambert_Conformal_Conic_2SP\"],PARAMETER[\"standard_parallel_1\",49],PARAMETER[\"standard_parallel_2\",44],PARAMETER[\"latitude_of_origin\",46.5],PARAMETER[\"central_meridian\",3],PARAMETER[\"false_easting\",700000],PARAMETER[\"false_northing\",6600000],UNIT[\"metre\",1,AUTHORITY[\"EPSG\",\"9001\"]],AXIS[\"X\",EAST],AXIS[\"Y\",NORTH],AUTHORITY[\"EPSG\",\"2154\"]]"
inp <-"2154"
#usage##geojsonsf::geojson_sf(file,wkt = wkt, input = inp)
mainpath<-"C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/DonneesBrutes/Betail_VCG_2010_2013/"

# shp2pgsql.exe -s 2154 -I "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/DonneesBrutes/Betail_VCG_2010_2013/betail_11/betail_11.shp" tmp_betail2011N | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h localhost -d db_chevreuils -U postgres
# shp2pgsql.exe -s 2154 -I "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/DonneesBrutes/Betail_VCG_2010_2013/betail_12/betail_12.shp" tmp_betail2012N | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h localhost -d db_chevreuils -U postgres
# shp2pgsql.exe -s 2154 -I "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/DonneesBrutes/Betail_VCG_2010_2013/betail_13/betail_13.shp" tmp_betail2013N | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h localhost -d db_chevreuils -U postgres
# shp2pgsql.exe -s 2154 -I "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/DonneesBrutes/betail_21/betail_21 tmp_betail2021N | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h localhost -d db_chevreuils -U postgres

# select recno, ST_Buffer(st_collectionextract(st_makevalid(geom),3), -0.9) geom from public.tmp_fauche2010n where geom is not null
# and ST_AsText(ST_Buffer(geom, -0.9)) ~* 'EMPTY'

#####tentativede lier tr_parcellaire avec les couches betail, procédure abandonnées pour ne pas faire de choix arbitraires.
# drop table tmp_essai;
# create table tmp_essai  as 
# select tr_parcellaire_par.*, gid, recno, areaha, areaha_1, os_10_11, grd_cat_11, gestion_1, espece_1, remarque_1, sem14_1, sem15_1, sem16_1, sem17_1, sem18_1, sem19_1, sem20_1, sem21_1, sem22_1, sem23_1, sem24_1, sem25_1, sem26_1, sem27_1, sem28_1, sem29_1, sem30_1, sem31_1, sem32_1, sem33_1, sem34_1, sem35_1, sem36_1, sem37_1, sem38_1, sem39_1, sem40_1, sem41_1, sem42_1, sem43_1, sem44_1, sem45_1, sem46_1, sem47_1, sem48_1, sem49_1
# from tr_parcellaire_par
# ,tmp_betail2010n
# --left join tmp_betail2010n on st_intersects(tr_parcellaire_par.geom, st_Buffer(st_collectionextract(st_makevalid(tmp_betail2010n.geom),3),-0.1))
# where par_annee = 2010 and st_intersects(tr_parcellaire_par.geom, st_Buffer(st_collectionextract(st_makevalid(tmp_betail2010n.geom),3),-0.1))
# and par_grd_cat in ('pature','prairie naturelle','prairie artificielle','prairie','pelouse','parc arbore', 'luzerne', 'legumineuse', 'jardin', 'jachere')
# and grd_cat_11 in  ('pature','prairie naturelle','prairie artificielle','prairie','pelouse','parc arbore', 'luzerne', 'legumineuse', 'jardin', 'jachere')
#####╣voir pour 2010 parcelle recno betail 9662 elle intersect avec 4 prairies mais la couche tr_parcellaire_par mais ces prairies intersect aussi avec une haie de la couche betail (recno 5077) d'ou la sélection attributaire supplémentaire

##montage des couhes shapefiles dans la bdd
# cd C:\Program Files\PostgreSQL\12\bin
# shp2pgsql.exe -s 2154 -I "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/DonneesBrutes/Betail_VCG_2010_2013/betail_10/betail_10.shp" tmp_betail2010N | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h localhost -d db_chevreuils -U postgres
# cd C:\Program Files\PostgreSQL\12\bin
# shp2pgsql.exe -s 2154 -I "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/DonneesBrutes/Betail_VCG_2010_2013/betail_11/betail_11.shp" tmp_betail2011N | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h localhost -d db_chevreuils -U postgres
# cd C:\Program Files\PostgreSQL\12\bin
# shp2pgsql.exe -s 2154 -I "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/DonneesBrutes/Betail_VCG_2010_2013/betail_12/betail_12.shp" tmp_betail2012N | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h localhost -d db_chevreuils -U postgres
# cd C:\Program Files\PostgreSQL\12\bin
# shp2pgsql.exe -s 2154 -I "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_domestiques/DonneesBrutes/Betail_VCG_2010_2013/betail_13/betail_13.shp" tmp_betail2013N | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h localhost -d db_chevreuils -U postgres

# Creation de la table pour accueuillr les données dans bd_cefs
# dbSendQuery(serveur,enc2utf8("
# DROP table if exists public.tr_betail_bet;
# CREATE TABLE public.tr_betail_bet (
#   bet_id serial NOT NULL,
#   bet_recno_origin float8 not NULL,
#   bet_annee integer not NULL,
#   bet_espece VARCHAR(500) NULL,
#   bet_remarque VARCHAR(20000) NULL,
#   bet_semaine integer NULL,
#   bet_charge integer NULL,
#   bet_gestion varchar(500) NULL,
#   bet_geom geometry not null,
#   bet_protocole varchar(3000) NULL DEFAULT 'https://sites.inrae.fr/site/cefs/UNITE_UR0035/Qualite/Manuel_Qualite_CEFS/Documents%20partages/Protocoles_valid%C3%A9s/suivi_terrain/Tutoriel%20saisie%20betail.docx?Web=1',
#   bet_metadata varchar(3000) NULL DEFAULT 'https://sites.inrae.fr/site/cefs/UNITE_UR0035/Qualite/Manuel_Qualite_CEFS/Documents%20partages/Protocoles_valid%C3%A9s/suivi_terrain/Tutoriel%20saisie%20betail.docx?Web=1',
#   bet_metadata_geom geometry NULL DEFAULT 
# );
# 
# -- Permissions
# 
# ALTER TABLE public.tr_betail_bet OWNER TO cefs_admin;
# GRANT ALL ON TABLE public.tr_betail_bet TO cefs;
# GRANT SELECT ON TABLE public.tr_betail_bet TO cefs_lecture;
# GRANT ALL ON TABLE public.tr_betail_bet TO cefs_ecriture;
# 
# -- Column comments
# 
# COMMENT ON COLUMN public.tr_betail_bet.bet_id IS 'identifiant automatique unique d''une observation';
# COMMENT ON COLUMN public.tr_betail_bet.bet_recno_origin IS 'Recno du shape file d''origine afin de pouvoir revenir à la donnée source. ATTENTION ne correspond pas à recno de tr_parcellaire_par';
# COMMENT ON COLUMN public.tr_betail_bet.bet_annee IS 'Annee de l''observation sur la parcelle';
# COMMENT ON COLUMN public.tr_betail_bet.bet_espece IS 'Espèce(s) de bétail contacté sur la parcelle';
# COMMENT ON COLUMN public.tr_betail_bet.bet_remarque IS 'L''attribut remarque précise les cas complexes de mélanges d''espèces en simultané : mix ou en séquentiel : seq
# Ex : seq_bxs36_39_e3sc   (séquence_ bovin pendant 36 à 39 ou équin à charge 3 pour les autres semaines renseignées)
# Ex : mix_bxsx_e3s39_48  (mélange b= bovin pour chacune des semaines x renseignées avec le chargement x renseigné et  e=équin pour les semaines 39 à 48 avec un chargement médian de 3 équins inclus dans le chargement total) 
# Ex : mix_bxsx_e3saufs18, 31, 43_47  (mélange b=bovin pour chaque chargement x et semaine x et e= équin avec charge médiane de 3 chaque semaine renseignées sauf les semaines 18, 31, et 43 à 47)
# ';
# COMMENT ON COLUMN public.tr_betail_bet.bet_charge IS ' Pour la saisie nous avons retenu pour chacune des classes une valeur médiane :
# 1_5=3 ; 6_10=8 ; 11_15=13 ; 16_20= 18 ; 21_25=23 ; 26_50=38 ; 51_75=63 ; 76_100=88 ; 101_200=150 ; 201_300=250
# Ex : Relevé terrain bovin « 1_5 », a été saisie « 3 »
# ';
# COMMENT ON COLUMN public.tr_betail_bet.bet_gestion IS 'partiel lorsque la parcelle a au moins été une fois utilisé partiellement par le bétail (ex : pâturage raisonné avec  déplacement progressif de la clôture).
# total lorsque la parcelle est utilisée en totalité. 
# ';
# COMMENT ON COLUMN public.tr_betail_bet.bet_geom IS 'Objet géométrique de la parcelle, alimenté depuis les objets géométriques des tables tr_betailAAAA avec correction de la géométrie via une instruction st_collectionextract(st_makevalid(tr_betailAAAA.geom),3)';
# COMMENT ON COLUMN public.tr_betail_bet.bet_protocole IS 'lien vers le protocole de saisie';
# COMMENT ON COLUMN public.tr_betail_bet.bet_metadata IS 'lien vers les metadonnées';
# "))
# dbSendQuery(serveur,enc2utf8("
# ALTER TABLE public.tr_betail_bet ADD CONSTRAINT cst_unique UNIQUE (bet_recno_origin, bet_annee, bet_semaine);
# ALTER TABLE public.tr_betail_bet ADD primary key (bet_id);
# CREATE INDEX tr_betail_bet_geom_gist ON public.tr_betail_bet USING gist (bet_geom);
# CREATE INDEX tr_betail_bet_bet_annee ON public.tr_betail_bet USING btree (bet_annee);"))

setwd(file.path(mainpath,"betail_10"))
#dat<-read.dbf("betail_10.dbf")
dat<-st_read("betail_10.shp")
names(dat)<-gsub("_1$","",names(dat))
names(dat)<-gsub("^SEM","",names(dat))
dat<-melt(dat, id.vars=c("RECNO","AREAHA","AREAHA","OS_10_11","GRD_CAT_11","GESTION","ESPECE","REMARQUE","geometry"))
#dat<-apply(dat,2,as.character)
dat[which(dat[,"ESPECE"] == "0"),"ESPECE"]<-NA
dat[which(dat[,"ESPECE"] == "NA"),"ESPECE"]<-NA
#dat<-as.data.frame(dat)
names(dat)<-gsub("variable","num_sem",names(dat))
names(dat)<-gsub("value","charge",names(dat))
unique(dat[,"ESPECE"])
unique(dat[,"GESTION"])
#a= aviaire ; b= bovin ; c= caprin ; e= équin ; o= ovin ; p=porcin
dat[which(tolower(dat[,"ESPECE"]) == "a"),"ESPECE"]<-"aviaire"
dat[which(tolower(dat[,"ESPECE"]) == "b"),"ESPECE"]<-"bovin"
dat[which(tolower(dat[,"ESPECE"]) == "c"),"ESPECE"]<-"caprin"
dat[which(tolower(dat[,"ESPECE"]) == "e"),"ESPECE"]<-"équin"
dat[which(tolower(dat[,"ESPECE"]) == "o"),"ESPECE"]<-"ovin"
dat[which(tolower(dat[,"ESPECE"]) == "p"),"ESPECE"]<-"porcin"
dat[which(tolower(dat[,"ESPECE"]) == "eo"),"ESPECE"]<-"équin-ovin"
dat[which(tolower(dat[,"ESPECE"]) == "be"),"ESPECE"]<-"bovin-équin"
dat[which(tolower(dat[,"ESPECE"]) == "ce"),"ESPECE"]<-"caprin-équin"
dat[which(tolower(dat[,"ESPECE"]) == "ceo"),"ESPECE"]<-"caprin-équin-ovin"
dat[which(tolower(dat[,"ESPECE"]) == "ceop"),"ESPECE"]<-"caprin-équin-ovin-porcin"
dat[which(tolower(dat[,"ESPECE"]) == "ep"),"ESPECE"]<-"équin-porcin"
dat[which(tolower(dat[,"ESPECE"]) == "bo"),"ESPECE"]<-"bovin-ovin"
dat[which(tolower(dat[,"ESPECE"]) == "bep"),"ESPECE"]<-"bovin-équin-porcin"
dat[which(tolower(dat[,"ESPECE"]) == "eop"),"ESPECE"]<-"équin-ovin-porcin"
dat[which(tolower(dat[,"ESPECE"]) == "bp"),"ESPECE"]<-"bovin-porcin"
dat[which(tolower(dat[,"GESTION"]) == "p"),"GESTION"]<-"partiel"
dat[which(tolower(dat[,"GESTION"]) == "t"),"GESTION"]<-"total"
dat$bet_annee<-"2010"

dat[which(tolower(dat[,"charge"]) == "3"),"charge"]<-"1_5"
dat[which(tolower(dat[,"charge"]) == "8"),"charge"]<-"6_10"
dat[which(tolower(dat[,"charge"]) == "13"),"charge"]<-"11_15"
dat[which(tolower(dat[,"charge"]) == "18"),"charge"]<-"16_20"
dat[which(tolower(dat[,"charge"]) == "23"),"charge"]<-"21_24"
dat[which(tolower(dat[,"charge"]) == "38"),"charge"]<-"26_50"
dat[which(tolower(dat[,"charge"]) == "63"),"charge"]<-"51_75"
dat[which(tolower(dat[,"charge"]) == "88"),"charge"]<-"76_100"
dat[which(tolower(dat[,"charge"]) == "150"),"charge"]<-"101_200"
dat[which(tolower(dat[,"charge"]) == "250"),"charge"]<-"201_300"

dat<-dat[-which(is.na(dat[,"ESPECE"])),]
dat<-dat[,c("RECNO","bet_annee","ESPECE","REMARQUE","num_sem","charge","GESTION","geometry")]
names(dat)<- c("bet_recno_origin","bet_annee","bet_espece","bet_remarque","bet_semaine","bet_charge","bet_gestion","geom")
dbSendQuery(serveur,"DROP TABLE IF EXISTS tr_betail_bett")
st_write(dat,serveur, "tr_betail_bett",append =TRUE, layer_options = "OVERWRITE=FALSE")
dbSendQuery(serveur,"update tr_betail_bett set geom = st_collectionextract(st_makevalid(geom),3)")
dbSendQuery(serveur,"delete from tr_betail_bett where bet_charge = 'NA'")
dbSendQuery(serveur,enc2utf8("
insert into tr_betail_bet (bet_recno_origin, bet_annee, bet_espece, bet_remarque, bet_semaine, bet_charge, bet_gestion, bet_geom) 
select bet_recno_origin::integer, bet_annee::integer, bet_espece::varchar, bet_remarque::varchar, bet_semaine::integer, bet_charge::integer, bet_gestion::varchar, geom 
from tr_betail_bett where not(ST_AsText(geom) ~* 'EMPTY')
"))

dat<-dat[,c("RECNO","FAUCHE1","annee")]
names(dat)<-c("par_recno","par_fauche","par_annee")

setwd(file.path(mainpath,"fauche_2010"))
datt<-read.dbf("union1.dbf")
datt$annee<-"2010"
datt<-datt[,c("RECNO","WEEK_FAUCH","annee")]
names(datt)<-c("par_recno","par_fauche","par_annee")
res<-rbind(dat,datt)

setwd(file.path(mainpath,"fauche_2011"))
datt<-read.dbf("fauche_2011.dbf")
datt$annee<-"2011"
datt<-datt[,c("RECNO","WEEK_FAUCH","annee")]
names(datt)<-c("par_recno","par_fauche","par_annee")
res<-rbind(res,datt)

setwd(file.path(mainpath,"fauche_2012"))
datt<-read.dbf("fauche_2012_lb93.dbf")
datt$annee<-"2012"
datt<-datt[,c("RECNO","WEEK_FAUCH","annee")]
names(datt)<-c("par_recno","par_fauche","par_annee")
res<-rbind(res,datt)

setwd(file.path(mainpath,"fauche_2013"))
datt<-read.dbf("fauche_2013_L93.dbf")
datt$annee<-"2013"
datt<-datt[,c("RECNO","WEEK_FAUCH","annee")]
names(datt)<-c("par_recno","par_fauche","par_annee")
res<-rbind(res,datt)

setwd(file.path(mainpath,"fauche_2014"))
datt<-read.dbf("fauche_2014_L93.dbf")
datt$annee<-"2014"
datt<-datt[,c("RECNO","week_fauch","annee")]
names(datt)<-c("par_recno","par_fauche","par_annee")
res<-rbind(res,datt)

setwd(file.path(mainpath,"fauche_2015"))
datt<-read.dbf("fauche_2015_L93.dbf")
datt$annee<-"2015"
datt<-datt[,c("RECNO","week_fauch","annee")]
names(datt)<-c("par_recno","par_fauche","par_annee")
res<-rbind(res,datt)

setwd(file.path(mainpath,"fauche_2016"))
datt<-read.dbf("fauche_2016_Lb93.dbf")
datt$annee<-"2016"
datt<-datt[,c("RECNO","week_fauch","annee")]
names(datt)<-c("par_recno","par_fauche","par_annee")
res<-rbind(res,datt)

setwd(file.path(mainpath,"fauche_2017"))
datt<-read.dbf("fauche_2017_Lb93.dbf")
datt$annee<-"2017"
datt<-datt[,c("RECNO","week_fauch","annee")]
names(datt)<-c("par_recno","par_fauche","par_annee")
res<-rbind(res,datt)

setwd(file.path(mainpath,"fauche_2018"))
datt<-read.dbf("fauche_2018_Lb93.dbf")
datt$annee<-"2018"
datt<-datt[,c("RECNO","week_fauch","annee")]
names(datt)<-c("par_recno","par_fauche","par_annee")
res<-rbind(res,datt)

# setwd(file.path(mainpath,"fauche_2019"))
# datt<-read.dbf("union1.dbf")
# datt$annee<-"2019"
# datt<-datt[,c("RECNO","week_fauch","annee")]
# names(datt)<-c("par_recno","par_fauche","par_annee")
# res<-rbind(res,datt)

setwd(file.path(mainpath,"fauche_2020/sig_fauch_2020_version_finale_BL_a_integrer_bd_cefs"))
datt<-read.dbf("sig_fauch_2020.dbf")
datt$annee<-"2020"
datt<-datt[,c("RECNO","WEEK_FAUCH","annee")]
names(datt)<-c("par_recno","par_fauche","par_annee")
res<-rbind(res,datt)
res[which(res[,"par_fauche"] == 0),"par_fauche"]<- as.integer(NA)



dbSendQuery(serveur,"update tr_betail_bet set bet_metadata_geom =
select st_transform(st_setsrid(st_collectionextract(st_makevalid(
  ST_MakePolygon(ST_MakeLine( ARRAY[ 
    point(0.9490299390938661,43.27527600380583)
    , point(0.9113180803444765,43.29531657290312)
    , point(0.9288543628838442,43.321571749030205)
    , point(0.9154915975716493,43.3328338737768)
    , point(0.879893319525995,43.329431262045766)
    , point(0.8766746687081239,43.3111350546682)
    , point(0.8475780653145693,43.292021188571965)
    , point(0.8429432081368349,43.2636203849535)
    , point(0.8036327528145693,43.25321281262639)
    , point(0.8273220228341005,43.23483113391217)
    , point(0.8522129224923036,43.247711395172665)
    , point(0.8647442030098818,43.26237032012422) 
    , point(0.9490299390938661,43.27527600380583)
    , point(0.9113180803444765,43.29531657290312)
    , point(0.9288543628838442,43.321571749030205)
    , point(0.9154915975716493,43.3328338737768)
    , point(0.879893319525995,43.329431262045766)
    , point(0.8766746687081239,43.3111350546682)
    , point(0.8475780653145693,43.292021188571965)
    , point(0.8429432081368349,43.2636203849535)
    , point(0.8036327528145693,43.25321281262639)
    , point(0.9490299390938661,43.27527600380583)]::geometry[]))),3),4326),2154)
")


dbWriteTable(serveur,"tmp_fauches",res, row.names = FALSE, overwrite =TRUE, field.types = c(par_recno = "integer",par_fauche = "integer",par_annee = "integer"))
# dbSendQuery(serveur, paste0("ALTER TABLE tmp_fauches ALTER COLUMN par_recno TYPE integer"))
# dbSendQuery(serveur, paste0("ALTER TABLE tmp_fauches ALTER COLUMN par_fauche TYPE integer"))
# dbSendQuery(serveur, paste0("ALTER TABLE tmp_fauches ALTER COLUMN par_annee TYPE integer"))
dbSendQuery(serveur, paste0("UPDATE tr_parcellaire_par set par_fauche = NULL"))
dbSendQuery(serveur, paste0("UPDATE tr_parcellaire_par set par_fauche = tmp_fauches.par_fauche from tmp_fauches where tmp_fauches.par_recno =  tr_parcellaire_par.par_recno and tr_parcellaire_par.par_annee = tmp_fauches.par_annee"))
dbSendQuery(serveur, paste0("DROP TABLE IF EXISTS tmp_fauches"))



dbSendQuery(serveur, paste0("update public.tr_parcellaire_par set par_fauche = null where par_grd_cat = 'route' and par_fauche = 99"))
dbSendQuery(serveur, paste0("update tr_parcellaire_par set par_fauche = 99 where par_fauche = 100"))
dbSendQuery(serveur, paste0("update tr_parcellaire_par set par_fauche = 29 where par_fauche = 296"))

####recharger les couches à partir des shape file, corriger les geométries et faire une requête spatiale pour lier les couches de fauche avec tr_parcellaire_par
shp2pgsql.exe -s 2154 -I "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_fauche/DonneesBrutes/fauche_2010/fauche2010N.shp" tmp_fauche2010N | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h pggeodb.nancy.inra.fr -d db_cefs -U ychaval

shp2pgsql.exe -s 2154 -I "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_fauche/DonneesBrutes/fauche_2010/fauche2010N.shp" tmp_fauche2010N | "C:/Program Files/PostgreSQL/12/bin/psql.exe" -p 5432 -h localhost -d db_chevreuils -U postgres

create table tmp_buffer  as 
select gid, recno, aire_ha, os_10, grd_cat_10, perimetre, week_fauch, ST_Buffer(st_collectionextract(st_makevalid(geom),3), -0.9) geom from public.tmp_fauche2010n where geom is not null
and not (ST_AsText(ST_Buffer(geom, -0.9)) ~* 'EMPTY')


# select recno, ST_Buffer(st_collectionextract(st_makevalid(geom),3), -0.9) geom from public.tmp_fauche2010n where geom is not null
# and ST_AsText(ST_Buffer(geom, -0.9)) ~* 'EMPTY'

select tr_parcellaire_par.par_recno, tmp_buffer.recno
from tr_parcellaire_par,
tmp_buffer
where st_contains(tr_parcellaire_par.geom, tmp_buffer.geom) and not (tr_parcellaire_par.par_recno = tmp_buffer.recno) and par_annee = 2010

update tr_parcellaire_par set par_fauche = null where par_annee = 2010
update tr_parcellaire_par set par_fauche = week_fauch from tmp_buffer where st_contains(tr_parcellaire_par.geom, tmp_buffer.geom) and par_annee = 2010