if (dim(input_data)[2] != 1){
  #for (i in 1:nrow(input_data)) {
  if (rows != formers_rows){ ####je regarde si la ligne sélectionnée est diférente de la précédente
    rows<-as.numeric(rows)
    if (exists("action")) { if (action == "insert") {
     if (length(grep("ani_etiq",names(input_data))) != 0){
       input_data[rows,"ani_etiq"] <- as.character(textInput(paste0("ani_etiq",rows), "", value = input_data[rows,"ani_etiq"]))
     }}}
    if (length(grep("ani_sexe",names(input_data))) != 0){
      input_data[rows,"ani_sexe"] <- as.character(selectInput(paste0("ani_sexe",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (ani_sexe) from public.t_animal_ani"))[,1], selected = input_data[rows,"ani_sexe"]))
    }
    if (length(grep("ani_name",names(input_data))) != 0){
      input_data[rows,"ani_name"] <- as.character(textInput(paste0("ani_name",rows), "", value= input_data[rows,"ani_name"]))
    }
    if (length(grep("ani_mere_observee",names(input_data))) != 0){
      input_data[rows,"ani_mere_observee"] <- as.character(textInput(paste0("ani_mere_observee",rows), "", value = input_data[rows,"ani_mere_observee"]))
    }
    if (length(grep("ani_fratrie",names(input_data))) != 0){
      input_data[rows,"ani_fratrie"] <- as.character(numericInput(paste0("ani_fratrie",rows), "", value = input_data[rows,"ani_fratrie"], min = 0, max = 4, step =1))
    }
    if (length(grep("sit_nom_court",names(input_data))) != 0){
      input_data[rows,"sit_nom_court"] <- as.character(selectInput(paste0("sit_nom_court",rows), "", choices = append("", utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM sit_nom_court) from public.tr_site_capture_sit where sit_actif =TRUE"))[,1]), selected = input_data[rows,"sit_nom_court"]))
    }
    if (exists("action")) { if (action == "insert") {
     if (length(grep("cap_date",names(input_data))) != 0){
       input_data[,"cap_date"]<-as.character(input_data[,"cap_date"])
       input_data[rows,"cap_date"] <- as.character(textInput(paste0("cap_date",rows), "", value = as.character(Sys.Date())))
     }}}
    if (length(grep("ani_date_mort",names(input_data))) != 0){
      input_data[,"ani_date_mort"]<-as.character(input_data[,"ani_date_mort"])
      input_data[rows,"ani_date_mort"] <- as.character(textInput(paste0("ani_date_mort",rows), "", value = input_data[rows,"ani_date_mort"]))
    }  
    if (length(grep("cap_poids",names(input_data))) != 0){
      input_data[rows,"cap_poids"] <- as.character(numericInput(paste0("cap_poids",rows), "", value =input_data[rows,"cap_poids"], min = 0, max = 10, step =0.01))
    }
    if (length(grep("cap_lpa",names(input_data))) != 0){
      input_data[rows,"cap_lpa"] <- as.character(numericInput(paste0("cap_lpa",rows), "", value = input_data[rows,"cap_lpa"], min = 0, max = 30, step =0.1))
    }
    if (length(grep("cap_etat_sante",names(input_data))) != 0){
      input_data[rows,"cap_etat_sante"] <- as.character(textInput(paste0("cap_etat_sante",rows), "", value = input_data[rows,"cap_etat_sante"]))
    }
    if (length(grep("cap_heure_lacher",names(input_data))) != 0){
      input_data[rows,"cap_heure_lacher"] <- as.character(textInput(paste0("cap_heure_lacher",rows), "", value = strftime(as.POSIXct(input_data[rows,"cap_heure_lacher"],format="%H:%M:%S"),format="%H:%M:%S")))
    }
    if (length(grep("cap_tag_droit",names(input_data))) != 0){
      input_data[rows,"cap_tag_droit"] <- as.character(numericInput(paste0("cap_tag_droit",rows), "", value = input_data[rows,"cap_tag_droit"], min = 0, max = 5000, step =1))
    }
    if (length(grep("cap_tag_gauche",names(input_data))) != 0){
      input_data[rows,"cap_tag_gauche"] <- as.character(numericInput(paste0("cap_tag_gauche",rows), "", value = input_data[rows,"cap_tag_gauche"], min = 0, max = 5000, step =1))
    }
    if (length(grep("cap_age_faon",names(input_data))) != 0){
      input_data[,"cap_age_faon"]<-as.character(input_data[,"cap_age_faon"]) 
      input_data[rows,"cap_age_faon"] <- as.character(textInput(paste0("cap_age_faon",rows), "", value = as.character(input_data[rows,"cap_age_faon"])))
    }
    if (exists("action")) { if (action == "insert") {
    if (length(grep("eqc_memoire",names(input_data))) != 0){
    input_data[rows,"eqc_memoire"] <- as.character(selectInput(paste0("eqc_memoire",rows), "", choices = append("",utf8(dbGetQuery(con, paste0("SELECT DISTINCT cconf_eqc_memoire FROM public.v_colliers_confannuelle_cconf where cconf_eqc_annee_suivi = '",substring(Sys.Date(),1,4),"' and cconf_eqc_couleur_boitier = 'faon' and cconf_eqc_pose = FALSE")))[,1])))
    }}}
  }
  ##https://stackoverflow.com/questions/58248102/date-input-dt-r-shiny
  selected <- sel #####lorsque les lignes précédentes sont excécuter la sélection est perdue donc on la force
  #}
  former_rows<- rows ###j'incrémente la ligne précédente
}


