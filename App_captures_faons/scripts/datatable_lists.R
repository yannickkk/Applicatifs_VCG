if (autorisation == TRUE){
  
  if (dim(input_data)[2] != 1){
    for (i in 1:nrow(input_data)) {
  if (length(grep("ani_sexe",names(input_data))) != 0){
    input_data[i,"ani_sexe"] <- as.character(selectInput("sit_nom_court", "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (ani_sexe) from public.t_animal_ani"))[,1], selected = input_data[i,"ani_sexe"]))
  }
  if (length(grep("ani_cause_mort_classes",names(input_data))) != 0){
    input_data[i,"ani_cause_mort_classes"] <- as.character(selectInput("sit_nom_court", "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (ani_cause_mort_classes) from public.t_animal_ani"))[,1], selected = input_data[i,"ani_cause_mort_classes"]))
  }
  }}
  
  
  
  
  if (dim(input_data)[2] != 1){
    for (i in 1:nrow(input_data)) {
  if (length(grep("sit_nom_court",names(input_data))) != 0){
      input_data[i,"sit_nom_court"] <- as.character(selectInput("sit_nom_court", "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (sit_nom_court) from public.tr_site_capture_sit"))[,1], selected = input_data[i,"sit_nom_court"]))
  }
  if (length(grep("ani_sexe",names(input_data))) != 0){
      input_data[i,"ani_sexe"] <- as.character(selectInput("sit_nom_court", "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (ani_sexe) from public.t_animal_ani"))[,1], selected = input_data[i,"ani_sexe"]))
  }
  if (length(grep("ani_cause_mort_classes",names(input_data))) != 0){
      input_data[i,"ani_cause_mort_classes"] <- as.character(selectInput("sit_nom_court", "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (ani_cause_mort_classes) from public.t_animal_ani"))[,1], selected = input_data[i,"ani_cause_mort_classes"]))
  }
  if (length(grep("cap_age",names(input_data))) != 0 & dim(input_data)[2] != 1){
      input_data[i,"cap_age"] <- as.character(selectInput("sit_nom_court", "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (cap_age) from public.t_capture_cap"))[,1], selected = input_data[i,"cap_age"]))
  }
  if (length(grep("cap_age_corrige",names(input_data))) != 0 & dim(input_data)[2] != 1){
      input_data[i,"cap_age_corrige"] <- as.character(selectInput("sit_nom_court", "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (cap_age_corrige) from public.t_capture_cap"))[,1], selected = input_data[i,"cap_age_corrige"]))
  }
  if (length(grep("cap_age_classe",names(input_data))) != 0 & dim(input_data)[2] != 1){
    input_data[i,"cap_age_classe"] <- as.character(selectInput("sit_nom_court", "", choices = utf8(dbGetQuery(con, "SELECT DISTINCT (cap_age_classe) from public.t_capture_cap"))[,1], selected = input_data[i,"cap_age_classe"]))
  }

  #######je n'arrive pas à faire un inpuDate qui affiche un calendrier en sortie ############
  # if (length(grep("ani_date_mort",names(input_data))) != 0 & dim(input_data)[2] != 1){
  #   for (i in 1:nrow(input_data)) {
  #     class(input_data[,"ani_date_mort"]) <- class(input_data[,"ani_etiq"])
  #     #øas.Date(as.numeric(input_data[i,"ani_date_mort"]), origin ='1970-01-01')
  #     input_data[i,"ani_date_mort"] <-as.character(dateInput("date", "", value = as.Date(as.numeric(input_data[i,"ani_date_mort"]), origin ='1970-01-01')))
  #    }
  # }

  
  }
}
}
# "cap_annee_suivi"
# 
# "cap_age"
# "cap_age_corrige"
# "cap_age_classe"
# "cap_poids"
# "cap_circou"
# "cap_lpa"
# "cap_num_sabot","cap_tag_droit","cap_tag_gauche"
