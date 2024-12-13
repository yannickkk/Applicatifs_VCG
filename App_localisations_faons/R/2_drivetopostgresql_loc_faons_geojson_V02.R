#------------------------------------integration donnees faons--------------------------------------
#  Auteur: Yannick Chaval, INRAE (French National Research Institute for Agriculture, Food and Environment), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#  Version: V2.0.0
#  Date:  23/08/20201
#  Description: ce fichier permet de reccuperer les donnees de localisation de telemetrie des faons sur le drive du CEFS (avec lequel est synchronise la tablette de terrain) de les importer dans la base de données du CEFS (bd_cefs)
#  un fois l'import réalisé, les données des trois derniers jours des faons sont re-extrait de la base afin de régénérer le geojson et d'avoir le lundi les locs du vendredi encore présentent sur l'app
#  Documentation:
#  Si la colonne evenement = "cadavre" pour une loclaisation alors l'animal est déclaré mort dans la table t_animal_ani et les coordonnées du cadavre sont importées dans les champs ani_mort_x et ani_mort_y
#  L'appli a été modifiée pour prendre en compte la date de mort et la cause_mort_classe des animaux
#  de plus quand un collier est au sol ou que le collier est récupéré sur un cadavre les champs de la table d'association animaux/colliers (eqa) est mise à jour (eqa_date_fin, eqa_date_fin_suivi, eqa_cause_fin_suivi)
#  Si lfa_evenement = "Cadavre" et que la date de mort est renseignée alors ani_date_mort=  eqa_date_fin_suivi = date_mort de l'app sinon ani_date_mort= eqa_date_fin_suivi = date de l'app
#  Si lfa_evenement = "collier au sol" et que date_mort est null alors eqa_date_fin_suivi = date de l'app
#Si lfa_evenement = "collier au sol"  et que date_mort n'est pas null alors ani_date_mort = eqa_date_fin_suivi =date_mort app 
#  ####https://googledrive.tidyverse.org/reference/drive_download.html
#
#
#
#------------------------------------------------------------------------------
#-------------------------- environnement de travail --------------------------
#####auto install packages

mypackages<-c("stringr","daff","googledrive", "foreign", "RPostgreSQL","uuid","lubridate","rgdal","sf", "dplyr", "as_tibble")
for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}



             drive_download(as_id(drive_find(pattern = "saisie_localisation_faons.geojson")$id), overwrite = TRUE)
             2
            #drive_download(as_id(drive_find(pattern = "saisie_localisation_faons.geojson")$id), overwrite = TRUE)
            # drive_download(as_id(drive_find(pattern = "localisations_faons.shx")$id), overwrite = TRUE)
            # drive_download(as_id(drive_find(pattern = "localisations_faons.cpg")$id), overwrite = TRUE)
            # drive_download(as_id(drive_find(pattern = "localisations_faons.proj")$id), overwrite = TRUE)
            # drive_download(as_id(drive_find(pattern = "localisations_faons.qpj")$id), overwrite = TRUE)
            # drive_download(as_id(drive_find(pattern = "localisation_faons.shp")$id), overwrite = TRUE)
            # drive_download(as_id(drive_find(pattern = "localisation_faons.shx")$id), overwrite = TRUE)
            
            #dat <- geojsonio::geojson_read("saisie_localisation_faons.geojson", what = "sp",  stringsAsFactors = TRUE)
            dat<-st_read("saisie_localisation_faons.geojson")
            
            
            dat<-as_Spatial(dat)
            #dat<-apply(read.dbf("localisations_faons.dbf"),2,as.character)
            dat$lfa_x_coor<-coordinates(dat)[,"coords.x1"]
            dat$lfa_y_coor<-coordinates(dat)[,"coords.x2"]
            dat<-as.data.frame(dat)
            dat<-dat[,c("lfa_id","lfa_ani_id","lfa_date","lfa_time","lfa_x_coor","lfa_y_coor","lfa_habita","lfa_os","lfa_grd_ca","lfa_remarq","lfa_evenem")] 
            dat<-apply(dat,2,as.character)
            
            colnames(dat)<-tolower(colnames(dat))
            Encoding(dat) <- "UTF-8"
            dat<- dat[which(as.numeric(dat[,"lfa_ani_id"]) != 0),] ####on vire les valeurs nulles
            
            dat<-as.data.frame(dat)
            dat[,"lfa_cause_mort_classes"]<- rep(NA, dim(dat)[1])
            dat[,"lfa_date_mort"]<- rep(NA, dim(dat)[1])

     
      ##############################################################
          
            dat[,"lfa_remarq"]<-gsub("'","''",lfa_remarque<-dat[,"lfa_remarq"])
            
            dat<-as.data.frame(dat)
            
            for (i in 1:(dim(dat)[1])){
              source("C:/Users/ychaval/Documents/BD_CEFS/con_serveur_dbcefs.R")
 
              con<- serveur
              
              lfa_ani_id<-dat[i,"lfa_ani_id"]
              lfa_date<- dmy(dat[i,"lfa_date"])
              lfa_time<- dat[i,"lfa_time"]
              lfa_habitat_obs<-dat[i,"lfa_habita"]
              lfa_os<-dat[i,"lfa_os"]
              lfa_grd_cat<-dat[i,"lfa_grd_ca"]
              lfa_remarque<-enc2utf8(gsub("'","''",enc2utf8(dat[i,"lfa_remarq"])))
              lfa_evenement<-enc2utf8(dat[i,"lfa_evenem"])
              
              val<-paste0("('",lfa_ani_id,"','",lfa_date,"','",lfa_time,"','",dat[i,"lfa_x_coor"],"','",dat[i,"lfa_y_coor"],"','",lfa_habitat_obs,"','",lfa_os,"','",lfa_grd_cat,"','",lfa_remarque,"','",lfa_evenement,"')")
              val <- gsub("NA","NULL", val)
              val <- gsub("'NULL'","NULL", val)
              
              
              updat<- "(lfa_x_coord,lfa_y_coord,lfa_habitat_obs,lfa_os,lfa_grd_cat,lfa_remarque,lfa_evenement)"
              ex.updat<-"(EXCLUDED.lfa_x_coord, EXCLUDED.lfa_y_coord, EXCLUDED.lfa_habitat_obs, EXCLUDED.lfa_os, EXCLUDED.lfa_grd_cat, EXCLUDED.lfa_remarque, EXCLUDED.lfa_evenement)"
              #iconv(, "UTF-8", "UTF8")
              dbSendQuery(con,enc2utf8(paste0("INSERT INTO public.t_locfaons_lfa (lfa_ani_id,lfa_date,lfa_time,lfa_x_coord,lfa_y_coord,lfa_habitat_obs,lfa_os,lfa_grd_cat,lfa_remarque,lfa_evenement) VALUES ",val," ON CONFLICT (lfa_ani_id,lfa_date,lfa_time) DO update SET ",updat," = ",ex.updat,";"))
                          )
              dbSendQuery(con,paste0("UPDATE public.t_locfaons_lfa SET lfa_geom_lb93 = ST_SetSRID(ST_MakePoint(lfa_x_coord,lfa_y_coord),2154) where lfa_geom_lb93 is null;")) ## WHERE oe_geom_lb93_pt IS NULL
             ######prise en compte de la date de mort et comparaison avec la date de pointage
              if (!is.na(dat[i,"lfa_evenem"]) && dat[i,"lfa_evenem"] == "cadavre"){
                if (is.na(dat[i,"lfa_date_mort"])){
                dbSendQuery(con,paste0("UPDATE public.t_animal_ani SET ani_date_mort = '",lfa_date,"',
                                                                       ani_date_mort_arrondi = 'FALSE'
                                                where ani_id =",lfa_ani_id,";"))
                } else if (!is.na(dat[i,"lfa_date_mort"]) && dat[i,"lfa_date_mort"] == dat[i,"lfa_date"]) {
                dbSendQuery(con,enc2utf8(paste0("UPDATE public.t_animal_ani SET ani_date_mort =  '",lfa_date,"',
                                                                       ani_date_mort_arrondi = 'FALSE'
                                                    WHERE ani_id =",lfa_ani_id,";")))} else { 
                dbSendQuery(con,enc2utf8(paste0("UPDATE public.t_animal_ani SET ani_date_mort =  '",dat[i,"lfa_date_mort"],"',
                                                                       ani_date_mort_arrondi = 'TRUE'
                                                    WHERE ani_id =",lfa_ani_id,";")))                                                      
                                                    }
                dbSendQuery(con,enc2utf8(paste0("UPDATE public.t_animal_ani SET ani_cause_mort_classes =  '",dat[i,"lfa_cause_mort_classes"],"',
                                                                       ani_mortalite = 'TRUE'
                                                    WHERE ani_id =",lfa_ani_id,";")))
                dbSendQuery(con,enc2utf8(paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET
                                                        eqa_date_fin = ani_date_mort,
                                                        eqa_date_fin_text = ani_date_mort,
                                                        eqa_cause_fin_suivi = 'Mort récupération du collier sur cadavre ou au sol avec signe de prédation'
                                       from t_animal_ani where  eqa_ani_id = ani_id and eqa_ani_id = ",lfa_ani_id," ")))
              }
              if (!is.na(dat[i,"lfa_evenem"]) && dat[i,"lfa_evenem"] == "collier au sol"){
                if (is.na(dat[i,"lfa_date_mort"])){
              dbSendQuery(con,enc2utf8(paste0("
              UPDATE tj_equipement_animal_eqt_ani_eqa SET
              eqa_date_fin_text = '",lfa_date,"',
              eqa_date_fin = '",lfa_date,"',
              eqa_date_fin_suivi = '",lfa_date,"',
              eqa_date_fin_suivi_text = '",lfa_date,"',
              eqa_remarque_suivi = '",lfa_remarque,"',
              eqa_cause_fin_suivi = 'collier au sol'
              WHERE  eqa_ani_id= ",lfa_ani_id,";")))}
                if (!is.na(dat[i,"lfa_date_mort"])){
                  dbSendQuery(con,paste0("UPDATE public.t_animal_ani SET ani_date_mort = '",lfa_date,"',
                                                                       ani_date_mort_arrondi = 'FALSE'
                                                where ani_id =",lfa_ani_id,";"))
                } else if (!is.na(dat[i,"lfa_date_mort"]) && dat[i,"lfa_date_mort"] == dat[i,"lfa_date"]) {
                  dbSendQuery(con,enc2utf8(paste0("UPDATE public.t_animal_ani SET ani_date_mort =  '",lfa_date,"',
                                                                       ani_date_mort_arrondi = 'FALSE'
                                                    WHERE ani_id =",lfa_ani_id,";")))} else { 
                  dbSendQuery(con,enc2utf8(paste0("UPDATE public.t_animal_ani SET ani_date_mort =  '",dat[i,"lfa_date_mort"],"',
                                                                       ani_date_mort_arrondi = 'TRUE'
                                                    WHERE ani_id =",lfa_ani_id,";")))                                                      
                                                    }
               if (!is.na(dat[i,"lfa_cause_mort_classes"])) {
                dbSendQuery(con,enc2utf8(paste0("UPDATE public.t_animal_ani SET ani_cause_mort_classes =  '",dat[i,"lfa_cause_mort_classes"],"',
                                                                       ani_mortalite = 'TRUE'
                                                    WHERE ani_id =",lfa_ani_id,";")))
              }
                dbSendQuery(con,enc2utf8(paste0("UPDATE tj_equipement_animal_eqt_ani_eqa SET
                                                        eqa_date_fin = ani_date_mort,
                                                        eqa_date_fin_text = ani_date_mort,
                                                        eqa_cause_fin_suivi = 'Mort récupération du collier sur cadavre ou au sol avec signe de prédation'
                                       from t_animal_ani where  eqa_ani_id = ani_id and eqa_ani_id = ",lfa_ani_id," ")))
              }}
              dbDisconnect(con)
              cat(i)
            }
          
mortauxcons()           
             
######♣je laisse dans le dbf les données de 7 derniers jours ou si il n'y en a pas les 5 dernières données
if (dim(dbGetQuery(con,"select * from public.t_locfaons_lfa where lfa_date > date(now()) -3 "))[1] != 0){
  back<-utf8(dbGetQuery(con,"select * from public.t_locfaons_lfa where lfa_date > date(now()) -3;"))} else {back<-utf8(dbGetQuery(con,"select * from public.t_locfaons_lfa order by lfa_date DESC limit 14"))}

#st_read(con, "select * from public.t_locfaons_lfa where lfa_date > date(now()) -3;") 
#st_read(con, "select * from public.t_locfaons_lfa order by lfa_date DESC limit 5;")

#back[,"oe_remarque"]<-iconv(back[,"oe_remarque"],"UTF-8", "UTF8")
back<-back[, -which(colnames(back)=="lfa_geom_lb93")]
back$lfa_cause_mort_classes<- rep(NA, dim(bac)[1])
back$lfa_date_mort<- rep(NA, dim(bac)[1])
names(back)<-names(dat)
#write.dbf(back,"observation_especes.dbf")
bac<-back

bac$lfa_date<-format(bac$lfa_date, "%d/%m/%Y")
bac$lfa_time<-format(strptime(bac$lfa_time, "%H:%M:%S"), "%H:%M:%S")

#back[is.na(back[,"oe_uuid"]),"oe_uuid"]<-replicate(length(back[is.na(back[,"oe_uuid"]),"oe_uuid"]), uuid::UUIDgenerate(TRUE))
#names(bac)<-names(dat)
bac<- as_tibble(bac)

bac<-st_as_sf(bac, coords = c("lfa_x_coor","lfa_y_coor"), crs = 2154)


st_write(bac, dsn = "C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_saisie_tablette/saisie_localisation_faons.geojson", layer = "saisie_localisation_faons2", driver = "GeoJSON")
setwd("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_saisie_tablette/")
drive_upload("saisie_localisation_faons.geojson", path ="VCG_suivi_tab1", name = "saisie_localisation_faons.geojson",verbose = TRUE)

#  d_coords <- unlist(st_geometry(bac[,c("lfa_x_coor","lfa_y_coor")])) %>% 
#    matrix(ncol=2,byrow=TRUE) %>% 
#    as_tibble() %>% 
#    setNames(c("lon","lat"))
#  
#  bac<-bind_cols(bac,d_coords)
# 
# coordinates(back) <- c("lfa_x_coor","lfa_y_coor")
# crs.lb93<-CRS("+init=epsg:2154")
# crs.lb93<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
# proj4string(back) <- crs.lb93
######%a finir
#setwd("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/save_suivi_especes_shape/")
# setwd("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_saisie_tablette/VCG_suivi")
# writeOGR(back,"localisations_faons.shp",layer= "observation_especes.shp",overwrite_layer=T, driver="ESRI Shapefile")
# write.dbf(bac, "localisations_faons.dbf", max_nchar = 200)
# dat2<-apply(read.dbf("localisations_faons.dbf"),2,as.character)
# Encoding(dat2) <- "UTF-8"
# #dat[,"oe_date"]<-as.character(as.Date(dat[,"oe_date"], format='%Y-%m-%d'), format='%d/%m/%y')
# dat2[,"lfa_time"]<-substring(dat2[,"lfa_time"],1,5)
# 
# #####le package foreign cree un dbf en prenant le champ le plus long comme limite pour les characters, je simule donc une longueur de 254 vides sur les champs character
# if(is.na(dat2[1,"lfa_remarq"])) {dat2[1,"lfa_remarq"] <- paste0(rep(" ",253),collapse="")} else {
#   dat2[1,"lfa_remarq"]<-paste0(append(dat2[1,"lfa_remarq"],rep(" ",253-nchar(dat2[1,"lfa_remarq"]))),collapse="")}
# if(is.na(dat2[1,"lfa_evenem"])) {dat2[1,"lfa_evenem"] <- paste0(rep(" ",253),collapse="")} else {
#   dat2[1,"lfa_evenem"]<-paste0(append(dat2[1,"lfa_evenem"],rep(" ",253-nchar(dat2[1,"lfa_evenem"]))),collapse="")}
# if(is.na(dat2[1,"lfa_habita"])) {dat2[1,"lfa_habita"] <- paste0(rep(" ",253),collapse="")} else {
#   dat2[1,"lfa_habita"]<-paste0(append(dat2[1,"lfa_habita"],rep(" ",253-nchar(dat2[1,"lfa_habita"]))),collapse="")}
# dat2[,"lfa_date"]<-format(ymd(dat2[,"lfa_date"]),"%d/%m/%Y")
# 
# write.dbf(dat2, "localisations_faons.dbf")
# 
# drive_rm(as_id(drive_find(pattern = "localisations_faons.dbf")$id), verbose = TRUE)
# drive_upload("localisations_faons.dbf", path ="VCG_suivi", name = "localisations_faons.dbf",verbose = TRUE)
# drive_rm(as_id(drive_find(pattern = "localisations_faons.shp")$id), verbose = TRUE)
# drive_upload("localisations_faons.shp", path ="VCG_suivi", name = "localisations_faons.shp",verbose = TRUE)
# drive_rm(as_id(drive_find(pattern = "localisations_faons.shx")$id), verbose = TRUE)
# drive_upload("localisations_faons.shx", path ="VCG_suivi", name = "localisations_faons.shx",verbose = TRUE)
# drive_rm(as_id(drive_find(pattern = "localisations_faons.prj")$id), verbose = TRUE)
# drive_upload("localisations_faons.prj", path ="VCG_suivi", name = "localisations_faons.prj",verbose = TRUE)

# setwd("C:/Users/ychaval/Documents/BD_CEFS/data/VCG/data_suivi/Outputs/save_suivi_especes/")
# write.csv2(dat2,paste0("sauv_suivi_especes_",gsub(" ","_",gsub(":","_",gsub("-","_",trimws(Sys.time())))),".csv"), row.names = FALSE)

dbDisconnect(con)
