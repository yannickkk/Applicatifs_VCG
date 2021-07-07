 
  if (dim(input_data)[2] != 1){
    if (rows != formers_rows){ ####je regarde si la ligne sélectionnée est diférente de la précédente
      rows<-as.numeric(rows)
      if (length(grep("sit_nom_court",names(input_data))) != 0){
      input_data[rows,"sit_nom_court"] <- as.character(selectInput(paste0("sit_nom_court",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (sit_nom_court) from public.tr_site_capture_sit"))[,1], selected = input_data[rows,"sit_nom_court"]))
  }
  if (length(grep("ani_sexe",names(input_data))) != 0){
      input_data[rows,"ani_sexe"] <- as.character(selectInput(paste0("ani_sexe",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (ani_sexe) from public.t_animal_ani"))[,1], selected = input_data[rows,"ani_sexe"]))
  }
  if (length(grep("ani_cause_mort_classes",names(input_data))) != 0){
      input_data[rows,"ani_cause_mort_classes"] <- as.character(selectInput(paste0("ani_cause_mort_classes",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (ani_cause_mort_classes) from public.t_animal_ani"))[,1], selected = input_data[rows,"ani_cause_mort_classes"]))
  }
  if (length(grep("cap_age",names(input_data))) != 0 & dim(input_data)[2] != 1){
      input_data[rows,"cap_age"] <- as.character(selectInput(paste0("cap_age",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (cap_age) from public.t_capture_cap"))[,1], selected = input_data[rows,"cap_age"]))
  }
  if (length(grep("cap_age_corrige",names(input_data))) != 0 & dim(input_data)[2] != 1){
      input_data[rows,"cap_age_corrige"] <- as.character(selectInput(paste0("cap_age_corrige",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (cap_age_corrige) from public.t_capture_cap"))[,1], selected = input_data[rows,"cap_age_corrige"]))
  }
  if (length(grep("cap_age_classe",names(input_data))) != 0 & dim(input_data)[2] != 1){
    input_data[rows,"cap_age_classe"] <- as.character(selectInput(paste0("cap_age_classe",rows), "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (cap_age_classe) from public.t_capture_cap"))[,1], selected = input_data[rows,"cap_age_classe"]))
  }
  if (length(grep("cap_annee_suivi",names(input_data))) != 0 & dim(input_data)[2] != 1){
    input_data[rows,"cap_annee_suivi"] <- as.character(numericInput(paste0("cap_annee_suivi",rows), "",value = input_data[rows,"cap_annee_suivi"], max = 2050, step = 1))
  }
  if (length(grep("cap_date",names(input_data))) != 0 & dim(input_data)[2] != 1){
    class(input_data[,"cap_date"]) <- class(input_data[,"ani_etiq"])
    input_data[rows,"cap_date"] <- paste0("<div id=\"",paste0("cap_age_classe",rows),"\" class=\"shiny-date-input form-group shiny-input-container\">\n  <label class=\"control-label\" for=\"date\"></label>\n  <input type=\"date\" class=\"form-control datepicker\" data-date-language=\"en\" data-date-week-start=\"0\" format=\"yyyy-mm-dd\" data-date-start-view=\"month\" value=\"",as.Date(as.numeric(input_data[rows,"cap_date"]), origin ='1970-01-01'),"\" data-date-autoclose=\"true\" data-date-dates-disabled=\"null\" data-date-days-of-week-disabled=\"null\"/>\n</div>")
  }
  if (length(grep("cap_poids",names(input_data))) != 0 & dim(input_data)[2] != 1){
    input_data[rows,"cap_poids"] <- as.character(numericInput(paste0("cap_poids",rows), "",value = input_data[rows,"cap_poids"], max = 40, step = 0.1))
  }
  if (length(grep("cap_circou",names(input_data))) != 0 & dim(input_data)[2] != 1){
  input_data[rows,"cap_circou"] <- as.character(numericInput(paste0("cap_circou",rows), "",value = input_data[rows,"cap_circou"], max = 40, step = 0.1))
  }
  if (length(grep("cap_lpa",names(input_data))) != 0 & dim(input_data)[2] != 1){
  input_data[rows,"cap_lpa"] <- as.character(numericInput(paste0("cap_lpa",rows), "",value = input_data[rows,"cap_lpa"], max = 40, step = 0.1))
  }
  if (length(grep("cap_num_sabot",names(input_data))) != 0 & dim(input_data)[2] != 1){
  input_data[rows,"cap_num_sabot"] <- as.character(numericInput(paste0("cap_num_sabot",rows), "",value = input_data[rows,"cap_num_sabot"], min =1 ,max = 28, step = 1))
  }
  if (length(grep("cap_tag_droit",names(input_data))) != 0 & dim(input_data)[2] != 1){
  input_data[rows,"cap_tag_droit"] <- as.character(numericInput(paste0("cap_tag_droit",rows), "",value = input_data[rows,"cap_tag_droit"], min =1 ,max = 7000, step = 1))
  }
  if (length(grep("cap_tag_gauche",names(input_data))) != 0 & dim(input_data)[2] != 1){
  input_data[rows,"cap_tag_gauche"] <- as.character(numericInput("cap_tag_gauche", "",value = input_data[rows,"cap_tag_gauche"], min =1 ,max = 7000, step = 1))
  }
  selected = sel    
  }
    former_rows<- rows ###j'incrémente la ligne précédente
}
