if (dim(input_data)[2] != 1){
  #for (i in 1:nrow(input_data)) {
  if (rows != formers_rows){ ####je regarde si la ligne sélectionnée est diférente de la précédente
    rows<-as.numeric(rows)
    if (length(grep("avant_capture_actif",names(input_data))) != 0){
      input_data[rows,"avant_capture_actif"] <- as.character(selectInput(paste0("avant_capture_actif",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM avant_capture_actif) from v_individus_total"))[,1], selected = input_data[rows,"avant_capture_actif"]))
    }
    if (length(grep("avant_capture_cri",names(input_data))) != 0){
      input_data[rows,"avant_capture_cri"] <- as.character(selectInput(paste0("avant_capture_cri",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM avant_capture_cri) from v_individus_total"))[,1], selected = input_data[rows,"avant_capture_cri"]))
    }
    if (length(grep("avant_capture_etat_faon",names(input_data))) != 0){
      input_data[rows,"avant_capture_etat_faon"] <- as.character(selectInput(paste0("avant_capture_etat_faon",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM avant_capture_etat_faon) from v_individus_total"))[,1], selected = input_data[rows,"avant_capture_etat_faon"]))
    }
    if (length(grep("avant_capture_faon_visible",names(input_data))) != 0){
      input_data[rows,"avant_capture_faon_visible"] <- as.character(selectInput(paste0("avant_capture_faon_visible",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM avant_capture_faon_visible) from v_individus_total"))[,1], selected = input_data[rows,"avant_capture_faon_visible"]))
    }
    if (length(grep("avant_capture_gite",names(input_data))) != 0){
      input_data[rows,"avant_capture_gite"] <- as.character(selectInput(paste0("avant_capture_gite",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM avant_capture_gite) from v_individus_total"))[,1], selected = input_data[rows,"avant_capture_gite"]))
    }
    if (length(grep("avant_capture_cri_detresse",names(input_data))) != 0){
      input_data[rows,"avant_capture_cri_detresse"] <- as.character(selectInput(paste0("avant_capture_cri_detresse",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM avant_capture_cri_detresse) from v_individus_total"))[,1], selected = input_data[rows,"avant_capture_cri_detresse"]))
    }
    if (length(grep("avant_capture_mere_gite",names(input_data))) != 0){
      input_data[rows,"avant_capture_mere_gite"] <- as.character(selectInput(paste0("avant_capture_mere_gite",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM avant_capture_mere_gite) from v_individus_total"))[,1], selected = input_data[rows,"avant_capture_mere_gite"]))
    }
    if (length(grep("capture_cri",names(input_data))) != 0){
      input_data[rows,"capture_cri"] <- as.character(selectInput(paste0("capture_cri",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM capture_cri) from v_individus_total"))[,1], selected = input_data[rows,"capture_cri"]))
    }
    if (length(grep("avant_capture_mere_vis",names(input_data))) != 0){
      input_data[rows,"avant_capture_mere_vis"] <- as.character(selectInput(paste0("avant_capture_mere_vis",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM avant_capture_mere_vis) from v_individus_total"))[,1], selected = input_data[rows,"avant_capture_mere_vis"]))
    }
    if (length(grep("avant_capture_position_init",names(input_data))) != 0){
      input_data[rows,"avant_capture_position_init"] <- as.character(selectInput(paste0("avant_capture_position_init",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM avant_capture_position_init) from v_individus_total"))[,1], selected = input_data[rows,"avant_capture_position_init"]))
    }
    if (length(grep("avant_capture_reaction",names(input_data))) != 0){
      input_data[rows,"avant_capture_reaction"] <- as.character(selectInput(paste0("avant_capture_reaction",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM avant_capture_reaction) from v_individus_total"))[,1], selected = input_data[rows,"avant_capture_reaction"]))
    }
    if (length(grep("capture",names(input_data))) != 0){
      input_data[rows,"capture"] <- as.character(selectInput(paste0("capture",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM capture) from v_individus_total"))[,1], selected = input_data[rows,"capture"]))
    }
    if (length(grep("pesee_agitation",names(input_data))) != 0){
      input_data[rows,"pesee_agitation"] <- as.character(selectInput(paste0("pesee_agitation",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM pesee_agitation) from v_individus_total"))[,1], selected = input_data[rows,"pesee_agitation"]))
    }
    if (length(grep("entre_manip_agitation",names(input_data))) != 0){
      input_data[rows,"entre_manip_agitation"] <- as.character(selectInput(paste0("entre_manip_agitation",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM entre_manip_agitation) from v_individus_total"))[,1], selected = input_data[rows,"entre_manip_agitation"]))
    }
    if (length(grep("entre_manip_cri",names(input_data))) != 0){
      input_data[rows,"entre_manip_cri"] <- as.character(selectInput(paste0("entre_manip_cri",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM entre_manip_cri) from v_individus_total"))[,1], selected = input_data[rows,"entre_manip_cri"]))
    }
    if (length(grep("apres_lacher_latence",names(input_data))) != 0){
      input_data[rows,"apres_lacher_latence"] <- as.character(numericInput(paste0("apres_lacher_latence",rows), "", value = input_data[rows,"apres_lacher_latence"], min = 0, max = 3000, step =1))
    }
    if (length(grep("apres_lacher_activite",names(input_data))) != 0){
      input_data[rows,"apres_lacher_activite"] <- as.character(selectInput(paste0("apres_lacher_activite",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM apres_lacher_activite) from v_individus_total"))[,1], selected = input_data[rows,"apres_lacher_activite"]))
    }
    if (length(grep("apres_lacher_comportement",names(input_data))) != 0){
      input_data[rows,"apres_lacher_comportement"] <- as.character(selectInput(paste0("apres_lacher_comportement",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM apres_lacher_comportement) from v_individus_total"))[,1], selected = input_data[rows,"apres_lacher_comportement"]))
    }
    if (length(grep("apres_lacher_parti",names(input_data))) != 0){
      input_data[rows,"apres_lacher_parti"] <- as.character(selectInput(paste0("apres_lacher_parti",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM apres_lacher_parti) from v_individus_total"))[,1], selected = input_data[rows,"apres_lacher_parti"]))
    }
    if (length(grep("apres_lacher_vocalise",names(input_data))) != 0){
      input_data[rows,"apres_lacher_vocalise"] <- as.character(selectInput(paste0("apres_lacher_vocalise",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM apres_lacher_vocalise) from v_individus_total"))[,1], selected = input_data[rows,"apres_lacher_vocalise"]))
    }
    if (length(grep("capture_heure",names(input_data))) != 0){
      input_data[rows,"capture_heure"] <- as.character(textInput(paste0("capture_heure",rows), "", value = input_data[rows,"capture_heure"]))
    }
    
   }
  ##https://stackoverflow.com/questions/58248102/date-input-dt-r-shiny
  selected <- sel #####lorsque les lignes précédentes sont excécuter la sélection est perdue donc on la force
  #}
  former_rows<- rows ###j'incrémente la ligne précédente
}


