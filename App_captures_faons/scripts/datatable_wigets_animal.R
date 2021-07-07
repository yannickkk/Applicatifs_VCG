 if (dim(input_data)[2] != 1){
    #for (i in 1:nrow(input_data)) {
   if (rows != formers_rows){ ####je regarde si la ligne sélectionnée est diférente de la précédente
      rows<-as.numeric(rows)
      if (length(grep("ani_sexe",names(input_data))) != 0){
        input_data[rows,"ani_sexe"] <- as.character(selectInput(paste0("ani_sexe",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM ani_sexe) from public.t_animal_ani"))[,1], selected = input_data[rows,"ani_sexe"]))
      }
      if (length(grep("ani_name",names(input_data))) != 0){
        input_data[rows,"ani_name"] <- as.character(textInput(paste0("ani_name",rows), "", value= input_data[rows,"ani_name"]))
      }
      if (length(grep("ani_date_mort",names(input_data))) != 0){
        input_data[,"ani_date_mort"]<-as.character(input_data[,"ani_date_mort"])
        input_data[rows,"ani_date_mort"] <- as.character(textInput(paste0("ani_date_mort",rows), "", value= input_data[rows,"ani_date_mort"]))
      }
      if (length(grep("ani_date_mort_arrondi",names(input_data))) != 0){
        input_data[rows,"ani_date_mort_arrondi"] <- as.character(selectInput(paste0("ani_date_mort_arrondi",rows), "", choices = c(FALSE, TRUE), selected = input_data[rows,"ani_date_mort_arrondi"]))
      }
      if (length(grep("ani_date_mort_text",names(input_data))) != 0){
        input_data[rows,"ani_date_mort_text"] <- as.character(textInput(paste0("ani_date_mort_text",rows), "", value= input_data[rows,"ani_date_mort_text"]))
      }
      if (length(grep("ani_poids_mort",names(input_data))) != 0){
        input_data[rows,"ani_poids_mort"] <- as.character(numericInput(paste0("ani_poids_mort",rows), "", value = input_data[rows,"ani_poids_mort"], min = 0, max = 40, step =1))
      }
      if (length(grep("ani_cause_mort",names(input_data))) != 0){
        input_data[rows,"ani_cause_mort"] <- as.character(textInput(paste0("ani_cause_mort",rows), "", value= input_data[rows,"ani_cause_mort"]))
      }
      if (length(grep("ani_remarque",names(input_data))) != 0){
        input_data[rows,"ani_remarque"] <- as.character(textInput(paste0("ani_remarque",rows), "", value= input_data[rows,"ani_remarque"]))
      }
      if (length(grep("ani_mort_x",names(input_data))) != 0){
        input_data[rows,"ani_mort_x"] <- as.character(numericInput(paste0("ani_mort_x",rows), "", value = input_data[rows,"ani_mort_x"], min = 474773, max = 551208, step =1))
      }
      if (length(grep("ani_mort_y",names(input_data))) != 0){
        input_data[rows,"ani_mort_y"] <- as.character(numericInput(paste0("ani_mort_y",rows), "", value = input_data[rows,"ani_mort_y"], min = 6221044, max = 6289419, step =1))
      }
      if (length(grep("ani_mere_observee",names(input_data))) != 0){
        input_data[rows,"ani_mere_observee"] <- as.character(textInput(paste0("ani_mere_observee",rows), "", value = input_data[rows,"ani_mere_observee"]))
      }
      if (length(grep("ani_fratrie",names(input_data))) != 0){
        input_data[rows,"ani_fratrie"] <- as.character(numericInput(paste0("ani_fratrie",rows), "", value = input_data[rows,"ani_fratrie"], min = 1, max = 4, step =1))
      }
      if (length(grep("ani_fratrie_oct",names(input_data))) != 0){
        input_data[rows,"ani_fratrie_oct"] <- as.character(numericInput(paste0("ani_fratrie_oct",rows), "", value = input_data[rows,"ani_fratrie_oct"], min = 1, max = 4, step =1))
      }
      if (length(grep("ani_surv_faon_marque_oct",names(input_data))) != 0){
        input_data[rows,"ani_surv_faon_marque_oct"] <- as.character(selectInput(paste0("ani_surv_faon_marque_oct",rows), "", choices = c(NA,FALSE, TRUE), selected = input_data[rows,"ani_surv_faon_marque_oct"]))
      }
      if (length(grep("ani_agres_faon_marque",names(input_data))) != 0){
        input_data[rows,"ani_agres_faon_marque"] <- as.character(selectInput(paste0("ani_agres_faon_marque",rows), "", choices = c(NA,FALSE, TRUE), selected = input_data[rows,"ani_agres_faon_marque"]))
      }
      if (length(grep("ani_cause_fin_suivi",names(input_data))) != 0){
        input_data[rows,"ani_cause_fin_suivi"] <- as.character(textInput(paste0("ani_cause_fin_suivi",rows), "", value= input_data[rows,"ani_cause_fin_suivi"]))
      }
      if (length(grep("ani_cause_fin_suivi_remarque",names(input_data))) != 0){
        input_data[rows,"ani_cause_fin_suivi_remarque"] <- as.character(textInput(paste0("ani_cause_fin_suivi_remarque",rows), "", value= input_data[rows,"ani_cause_fin_suivi_remarque"]))
      }
      if (length(grep("ani_date_fin_suivi",names(input_data))) != 0){
        input_data[,"ani_date_fin_suivi"]<-as.character(input_data[,"ani_date_fin_suivi"])
        input_data[rows,"ani_date_fin_suivi"] <- as.character(textInput(paste0("ani_date_fin_suivi",rows), "", value= input_data[rows,"ani_date_fin_suivi"]))
      }
      if (length(grep("ani_cause_mort_classes",names(input_data))) != 0){
        input_data[rows,"ani_cause_mort_classes"] <- as.character(selectInput(paste0("ani_cause_mort_classes",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT TRIM(BOTH FROM ani_cause_mort_classes) from v_individus_total"))[,1], selected = input_data[rows,"ani_cause_mort_classes"]))
      }
      if (length(grep("ani_necropsie",names(input_data))) != 0){
        input_data[rows,"ani_necropsie"] <- as.character(selectInput(paste0("ani_necropsie",rows), "", choices = c(NA,FALSE, TRUE), selected = input_data[rows,"ani_necropsie"]))
      }
      if (length(grep("ani_crane",names(input_data))) != 0){
        input_data[rows,"ani_crane"] <- as.character(selectInput(paste0("ani_crane",rows), "", choices = c(NA,FALSE, TRUE), selected = input_data[rows,"ani_crane"]))
      }
      if (length(grep("ani_lpa_mort",names(input_data))) != 0){
        input_data[rows,"ani_lpa_mort"] <- as.character(numericInput(paste0("ani_lpa_mort",rows), "", value = input_data[rows,"ani_lpa_mort"], min = 1, max = 40, step =1))
      }
      if (length(grep("ani_remarque_suivi",names(input_data))) != 0){
        input_data[rows,"ani_remarque_suivi"] <- as.character(textInput(paste0("ani_remarque_suivi",rows), "", value= input_data[rows,"ani_remarque_suivi"]))
      }
      if (length(grep("ani_dernier_contact",names(input_data))) != 0){
        input_data[rows,"ani_dernier_contact"] <- as.character(selectInput(paste0("ani_dernier_contact",rows), "", choices = c(NA,1:54), selected = input_data[rows,"ani_dernier_contact"]))
      }
      if (length(grep("ani_date_fin_suivi_text",names(input_data))) != 0){
        input_data[rows,"ani_date_fin_suivi_text"] <- as.character(textInput(paste0("ani_date_fin_suivi_text",rows), "", value= input_data[rows,"ani_date_fin_suivi_text"]))
      }
     }
      ##https://stackoverflow.com/questions/58248102/date-input-dt-r-shiny
      selected <- sel #####lorsque les lignes précédentes sont excécuter la sélection est perdue donc on la force
#}
      former_rows<- rows ###j'incrémente la ligne précédente
      }
   

   