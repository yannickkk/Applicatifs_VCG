

shinyServer(function(input, output, session) {

  close_session<-function(){
    mortauxcons()
    stopApp()
  }
  session$onSessionEnded(close_session)
  
  load<- function(champs, tab){
    if (length(grep("tous", champs)) != 0) {champs <- "*"} else {champs <- paste0(champs, collapse = ", ")} 
    input_data <<- utf8(dbGetQuery(con,enc2utf8(paste0(" SELECT ",champs," from v_individus_total where cap_age_classe in ",v2db(age)," order by cap_date DESC"))))
    ani_id_tous <<- utf8(dbGetQuery(con,paste0(" SELECT ani_id from v_individus_total order by cap_date, ani_id")))[,1]
    cap_date_tous <<- utf8(dbGetQuery(con,paste0(" SELECT cap_date from v_individus_total order by cap_date, ani_id")))[,1]
    ani_mort_tous <<- utf8(dbGetQuery(con,paste0(" SELECT ani_date_mort from v_individus_total order by cap_date, ani_id")))[,1]
    ###On donne les droits d'affichage, de correction ou de création en fonction des droits de la bdd voir droits_bdd.R
    ###noneditable est différent pour cefs_ecriture et cefs_admin voir droits_bdd.R
    if (autorisation == TRUE & role != "cefs_admin"){editable<<- list(target = "row", disable = list(columns = match(noneditable_ecriture,names(input_data))-1))}
    if (autorisation == TRUE & role == "cefs_admin" ){editable<<- list(target = "row", disable = list(columns = match(noneditable_base,names(input_data))-1))}
    input_data
    saved_input_data<<-input_data
    selectedchamps<<-champs
    this_table<<- reactiveVal(input_data)#reactiveVal(as.data.frame(input_data[subset,]))
    # print(this_table)
    # print(names(this_table))
    newdatatable()
  }
  
  newdatatable<- function(subset){
    output$mod_table <-  DT:: renderDataTable(

      input_data,  selection = list(mode = 'single', selected = selected, target = 'row'), escape = FALSE, server = FALSE,filter = "top",rownames = F,
                options = list(dom = 't', paging = FALSE, ordering = FALSE),
callback = JS("table.rows().every(function(i, tab, row) { 
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());
              ") #) #####le callback est la partie qui permet de renvoyer les données utilisateur à partir des wigjets html insérer dans le data table
     )#}
  }
  
##############selection des champs consernés et des individus (classes d'age) et des configurations de champs prédéfinies  ######################
  observeEvent(input$configuration, {
    if (input$configuration != "aucune") {
      if (input$configuration == "animal") {
        champs<<- unique(append(animal,input$champs))
        age<<- c("adulte","jeune","yearling","faon")
      }
      if (input$configuration == "capture") {
        champs<<- unique(append(capture,input$champs))
        age<<- c("adulte","jeune","yearling","faon")
      }
      if (input$configuration == "comportement_adulte") {
        champs<<- unique(append(comportement_adulte,input$champs))
        age<<- c("adulte","jeune","yearling")
      }
      if (input$configuration == "comportement_faon") {
        champs<<- unique(append(comportement_faon,input$champs))
        age<<- c("faon")
      }
      if (input$configuration == "capture_faon") {
        champs<<- unique(append(capture_faon,input$champs))
        age<<- c("faon")
      }
      
    }  else {champs<<-input$champs}
    load(champs,"v_individus_total")
  })
  
#####################################ajout de champs  
  observeEvent(input$champs, {
    if (input$configuration != "aucune") {
      if (input$configuration == "animal") {
        champs<<- unique(append(animal,input$champs))
      }
      if (input$configuration == "capture") {
        champs<<- unique(append(capture,input$champs))
      }
      if (input$configuration == "comportement_adulte") {
        champs<<- unique(append(comportement_adulte,input$champs))
      }
      if (input$configuration == "comportement_faon") {
        champs<<- unique(append(comportement_faon,input$champs))
      }
      if (input$configuration == "capture_faon") {
        champs<<- unique(append(capture_faon,input$champs))
      }
    }  else {champs<<-input$champs}
    load(champs,"v_individus_total")
  })
#######################################################################################
  
#############si l'utilisateur est membre de cefs_admin alors il peut créer un nouvelle individu et/ou une nouvelle capture (fonctionnalité utile pour les captures de faons) ######### 
  observeEvent(input$add_btn, {
    shinyalert("INFORMATION", "Pour pouvoir utiliser cette fonctionnalité, vous devez faire partie de cefs_admin", type = "info")
    if (role == "cefs_admin"){
    newdat<-rep(NA,dim(input_data)[2])
    names(newdat)<- names(input_data)
    input_data <<- rbind(newdat, this_table())
    newdatatable()
    action<<-"insert" ####permet de savoir si c'est une insertion (dans ce cas c'est toujours la ligne 1 de input_data) ou une mise à jour
    }
    #conf<<-input$configuration
    #fwrite(input_data, file="toto.csv")
  })
###########################maj de la base de données  
  
  observeEvent(input$val_btn, {
    login<<- input$login
    password<<- input$password
    
    if (exists("action")) {if (action == "insert") { ##############c'est une nouvelle capture

        ani<<-v2dbn(grep("ani_",names(input_data), value = TRUE)) ####noms des champs à mettre à jour dans la base de données
        cap<<-v2dbn(append(c("cap_ani_id","cap_sit_id","cap_bague","cap_faon","cap_annee_suivi","cap_age_classe","cap_pertinent","cap_tag_droit_metal","cap_tag_gauche_metal"),grep("cap_",names(input_data), value = TRUE)))
        pres<<-dbGetQuery(con,"SELECT ani_etiq from t_animal_ani")[,1]

        if (input$configuration == "capture_faon") {
         
        if (is.na(match(to_upper_first(input$ani_etiq1),pres))) { ####si le faon n'existe pas je le crée
        #print(input$configuration == "capture_faon")

        requ<<-paste0("INSERT INTO t_animal_ani ",ani," values ('",to_upper_first(trimws(input$ani_etiq1)),"','",trimws(input$ani_sexe1),"','",to_upper_first(trimws(input$ani_name1)),"','",to_upper_first(trimws(input$ani_mere_observee1)),"','",trimws(input$ani_fratrie1),"',NULL)")
        #requ <<-paste0("INSERT INTO t_animal_ani ",ani," values ",anival,"")
        requ <<- gsub("'NA'","NULL", requ)
        requ <<- gsub("''","NULL", requ)
        dbSendQuery(con,requ)

        ###################################Pour le comportement des faons il faut alimenter le schéma listes qui reprend la structure de db_Gardouch
        ani_id <<- utf8(dbGetQuery(con,paste0(" SELECT ani_id from t_animal_ani where ani_etiq = '",to_upper_first(trimws(input$ani_etiq1)),"'")))[,1]
        requ<<-paste0("INSERT INTO listes.t_individu_ind (ind_id,ind_obj_id,ind_name, ind_metadata, ind_insert_timestamp,ind_insert_source,ind_update_timestamp,ind_update_source) values (",ani_id,",1,'",trimws(input$ani_etiq1),"','ind_id = ani_id et ind_name = ani_etiq','",Sys.time(),"','Appli_DT-editable_V01','",Sys.time(),"','Appli_DT-editable_V01')")

        requ <<- gsub("'NA'","NULL", requ)
        requ <<- gsub("''","NULL", requ)
        dbSendQuery(con,requ)
        } ##### end si l'animal n'est pas présent
        ###########################insert de la capture et de l'observation du schema listes
        ani_id <<- utf8(dbGetQuery(con,paste0(" SELECT ani_id from t_animal_ani where ani_etiq = '",to_upper_first(trimws(input$ani_etiq1)),"'")))[,1]
        sit_id <<- utf8(dbGetQuery(con,paste0(" SELECT sit_id from public.tr_site_capture_sit where sit_nom_court = '",trimws(input$sit_nom_court1),"'")))[,1]

        requ<<-paste0("INSERT INTO t_capture_cap ",cap," values (",ani_id,",",sit_id,",'",paste0(trimws(to_upper_first(input$ani_etiq1)),"_",substring(Sys.Date(),3,4)),"',TRUE,",substring(Sys.Date(),1,4),",'faon',TRUE,TRUE,TRUE,'",trimws(input$cap_date1),"','",trimws(input$cap_poids1),"','",trimws(input$cap_circou1),"','",trimws(input$cap_lpa1),"','",input$cap_etat_sante1,"','",trimws(input$cap_heure_lacher1),"','",trimws(input$cap_tag_droit1),"','",trimws(input$cap_tag_gauche1),"','",trimws(input$cap_age_faon1),"')")

        requ <<- gsub("'NA'","NULL", requ)
        requ <<- gsub("''","NULL", requ)

        dbSendQuery(con, requ)
       ###################################Pour le comportement des faons il faut alimenter le schéma listes qui reprend la structure de db_Gardouch
        cap_id <<- utf8(dbGetQuery(con,paste0(" SELECT cap_id from t_capture_cap where cap_ani_id = ",ani_id," and cap_date = '",trimws(input$cap_date1),"'")))[,1]
        requ<<-paste0("INSERT INTO listes.t_observation_obs (obs_id,obs_ind_id,obs_date,obs_metadata, obs_insert_timestamp,obs_insert_source,obs_update_timestamp,obs_update_source) values (",cap_id,",",ani_id,",'",substring(Sys.time(),1,20),"','obs_id = cap_id et obs_ind_id = ani_id','",Sys.time(),"','Appli_DT_editable_V01','",Sys.time(),"','Appli_DT_editable_V01')")

        requ <<- gsub("'NA'","NULL", requ)
        requ <<- gsub("''","NULL", requ)
        dbSendQuery(con,requ)

        ###################################Pour le comportement des faons il faut alimenter le schéma listes qui reprend la structure de db_Gardouch
        if (!is.null(trimws(input$eqc_memoire1)) & trimws(input$eqc_memoire1) != ""){ #####si l'animal a un collier
        eqt_id <<- utf8(dbGetQuery(con,paste0("SELECT cconf_eqt_id FROM public.v_colliers_confannuelle_cconf where cconf_eqc_annee_suivi = ",substring(Sys.Date(),1,4)," and cconf_eqc_couleur_boitier = 'faon' and cconf_eqc_memoire = ",trimws(input$eqc_memoire1)," ")))
        requ<<-paste0("INSERT INTO public.tj_equipement_animal_eqt_ani_eqa (eqa_ani_id,eqa_eqt_id,eqa_date_debut, eqa_date_fin_arrondi, eqa_annee_suivi) values (",ani_id,",",eqt_id,",'",trimws(input$cap_date1),"',FALSE,",substring(Sys.Date(),1,4),")")
        requ <<- gsub("'NA'","NULL", requ)
        requ <<- gsub("''","NULL", requ)
        dbSendQuery(con,requ)} ###end si l'animal a un collier
        
        action <<- NULL ###on stoppe l'insert
        load(champs,"v_individus_total") ####on recharge le jeu de donnéesd
      }####end if configuration == "capture_faon"
      }######end insert
      } else { #################si c'est une mise à jour des champs existants (pas de nouvelle entrée) on va mettre chaque table à jour en fonction:
              #####des lignes sélectionnés (allsel)
              #####des champs contenus dans la vue avec comme préfixes ani, cap, sit
      for (i in allsel){
      if (is.na(i) | i == 0) {next}  
          ani<<-grep("ani_",names(input_data), value = TRUE)
          cap<<-grep("cap_",names(input_data), value = TRUE)
          sit<<-grep("sit_",names(input_data), value = TRUE) ####ici ce n'est pas la table tr_capture_site_sit que l'on va mettre à jour mais t_capture_cap et le champ cap_sit_id
          eqc<<-grep("eqc_",names(input_data), value = TRUE)
          cpt_faon <<-c("avant_capture_actif","avant_capture_cri","avant_capture_etat_faon","avant_capture_faon_visible","avant_capture_gite","avant_capture_cri_detresse","avant_capture_mere_gite","avant_capture_mere_vis","avant_capture_position_init","avant_capture_reaction","capture_heure","capture","capture_cri","pesee_agitation","entre_manip_agitation","entre_manip_cri","apres_lacher_latence","apres_lacher_activite","apres_lacher_comportement","apres_lacher_parti","apres_lacher_vocalise")
          cpt_faon_v <<-c("actif","cri_av_capt","etat_faon","faon_visible","gite","cri_detresse","mere_gite","mere_vis","position_init","reaction","heure_capture","capture","cri_capt","agitation_capt","agitation_manip","cri_manip","latence","activite","cprt","parti","vocalise_apres_lache")
          ####cpt_faon_v est creer car les noms des variables dans la table des variables ne sont pas les noms des variables dans la vue de façon a concerver les noms des fichiers source
          if (length(ani) != 0) {
          for (j in 1:length(ani)){ #####mise à jour des champs de la table animal
          ani_id <<- utf8(dbGetQuery(con,paste0(" SELECT ani_id from t_animal_ani where ani_etiq = '",input_data[i,"ani_etiq"],"'")))[,1]
          if (!is.null(input[[paste0(ani[j],i)]])){ ###input[[paste0(ani[j],i)]] rappel les valeurs des variables réactives si elles ne sont pas nulles. Si elles sont nulles (i.e. l'utilisateur n'a pas mis à jour le champ), on ne remet pas à jour
          requ <<- paste0("UPDATE t_animal_ani SET ",ani[j]," = '",input[[paste0(ani[j],i)]],"' where ani_id = ",ani_id," ")
          requ <<- gsub("'NA'","NULL", requ)
          requ <<- gsub("''","NULL", requ)
          dbSendQuery(con,requ)
          if (paste0(ani[j],i) == paste0("ani_date_mort",i)){          
          requ <<- paste0("UPDATE t_animal_ani SET ani_mortalite = TRUE where ani_id = ",ani_id," ")
          requ <<- gsub("'NA'","NULL", requ)
          requ <<- gsub("''","NULL", requ)
          dbSendQuery(con,requ)
          if (input$configuration == "capture_faon") {
          mem<<- utf8(dbGetQuery(con,paste0("SELECT distinct(eqc_memoire) from v_individus_total where ani_id = ",ani_id," and cap_annee_suivi =",substring(Sys.Date(),1,4),"")))
          eqt_id <<- utf8(dbGetQuery(con,paste0("SELECT cconf_eqt_id FROM public.v_colliers_confannuelle_cconf where cconf_eqc_annee_suivi = ",substring(Sys.Date(),1,4)," and cconf_eqc_couleur_boitier = 'faon' and cconf_eqc_memoire = ",trimws(mem)," ")))
          requ<<-paste0("UPDATE public.tj_equipement_animal_eqt_ani_eqa SET eqa_date_fin = '",input[[paste0(ani[j],i)]],"' where eqa_ani_id =",ani_id," and eqa_eqt_id =",eqt_id," and eqa_annee_suivi = ",substring(Sys.Date(),1,4)," ")
          requ <<- gsub("'NA'","NULL", requ)
          requ <<- gsub("''","NULL", requ)
          dbSendQuery(con,requ)
          } ####si l'animal est un faon et qu'il est associé à un collier (input$eqc_memoirei not null) alors la date de fin d'équipement est la date de mort de l'animal
          }####si ani_date_mort a été modifié par l'utilisateur alors ani_mortalite est vrai
          if (paste0(ani[j],i) == paste0("ani_poids_mort",i)) {          
          requ <<- paste0("UPDATE t_animal_ani SET ani_poids_mort_na = FALSE where ani_id = ",ani_id," ")
          requ <<- gsub("'NA'","NULL", requ)
          requ <<- gsub("''","NULL", requ)
          dbSendQuery(con,requ)} ### si ani_poids mort est modifié par l'utilisateur alors ani_poids_mort_na est false
          
          }###end condition not null reactive value
          } ###end loop for j 
          }###end test longueur ani
          if (length(cap) != 0) {
          for (j in 1:length(cap)){#####mise à jour des champs de la table capture
          ani_id <<- utf8(dbGetQuery(con,paste0(" SELECT ani_id from t_animal_ani where ani_etiq = '",input_data[i,"ani_etiq"],"'")))[,1]
          cap_id <<- utf8(dbGetQuery(con,paste0(" SELECT cap_id from t_capture_cap where cap_ani_id = ",ani_id," and cap_date = '",input_data[i,"cap_date"],"'")))[,1]
          if (!is.null(input[[paste0(cap[j],i)]])){
          requ<<-paste0("UPDATE t_capture_cap SET ",cap[j]," = '",input[[paste0(cap[j],i)]],"' where cap_id =",cap_id," ")
          requ <<- gsub("'NA'","NULL", requ)
          requ <<- gsub("''","NULL", requ)
          dbSendQuery(con,requ)
          }###end condition not null reactive value
          }###end loop for j
          }###end test longueur cap
          if (length(sit) != 0) {
          for (j in 1:length(sit)){#####mise à jour du champ site de capture (cap_sit_id) de la table site de capture
          ani_id <<- utf8(dbGetQuery(con,paste0("SELECT ani_id from t_animal_ani where ani_etiq = '",input_data[i,"ani_etiq"],"'")))[,1]
          cap_id <<- utf8(dbGetQuery(con,paste0("SELECT cap_id from t_capture_cap where cap_ani_id = ",ani_id," and cap_date = '",input_data[i,"cap_date"],"'")))[,1]
          if (!is.null(input[[paste0(sit[j],i)]])){
          sit_id <<-  utf8(dbGetQuery(con,paste0("SELECT sit_id from tr_site_capture_sit where TRIM(BOTH FROM sit_nom_court) = '",trimws(input[[paste0(sit[j],i)]]),"'"))) 
          requ<<-paste0("UPDATE t_capture_cap SET cap_sit_id = '",sit_id,"' where cap_id =",cap_id," ")
          requ <<- gsub("'NA'","NULL", requ)
          requ <<- gsub("''","NULL", requ)
          dbSendQuery(con,requ)
          }###end condition not null reactive value
          }###end loop for j
          }###end test longueur sit
          if (length(eqc) != 0) {
          for (j in 1:length(eqc)){#####mise à jour du champ site de capture (cap_sit_id) de la table site de capture
          ani_id <<- utf8(dbGetQuery(con,paste0("SELECT ani_id from t_animal_ani where ani_etiq = '",input_data[i,"ani_etiq"],"'")))[,1]
          if (!is.null(input[[paste0(eqc[j],i)]])){
          eqt_id <<- utf8(dbGetQuery(con,paste0("SELECT cconf_eqt_id FROM public.v_colliers_confannuelle_cconf where cconf_eqc_annee_suivi = ",substring(Sys.Date(),1,4)," and cconf_eqc_couleur_boitier = 'faon' and cconf_eqc_memoire = ",trimws(input[[paste0(eqc[j],i)]])," ")))
          requ<<-paste0("UPDATE public.tj_equipement_animal_eqt_ani_eqa SET eqa_eqt_id = '",eqt_id,"' where eqa_ani_id =",ani_id," and eqa_annee_suivi = ",substring(Sys.Date(),1,4)," ")
          requ <<- gsub("'NA'","NULL", requ)
          requ <<- gsub("''","NULL", requ)
          dbSendQuery(con,requ)
          }##end condition not null reactive value
          }###end loop for j
          }###end test longueur eqc
          if (input$configuration == "comportement_faon"){
          for (j in 1:length(cpt_faon)){#####mise à jour des champs des tables de comportement des faons
          ani_id <<- utf8(dbGetQuery(con,paste0(" SELECT ani_id from t_animal_ani where ani_etiq = '",input_data[i,"ani_etiq"],"'")))[,1]
          cap_id <<- utf8(dbGetQuery(con,paste0(" SELECT cap_id from t_capture_cap where cap_ani_id = ",ani_id," and cap_date = '",input_data[i,"cap_date"],"'")))[,1]
          #print(input_data)
          print(input[[paste0(cpt_faon[j],i)]])
          if (!is.null(input[[paste0(cpt_faon[j],i)]])){
          if (cpt_faon[j] %in% c("avant_capture_actif","avant_capture_cri","avant_capture_etat_faon","avant_capture_faon_visible","avant_capture_cri_detresse","avant_capture_mere_gite","avant_capture_mere_vis","avant_capture_position_init","avant_capture_reaction","capture","capture_cri","pesee_agitation","entre_manip_agitation","entre_manip_cri","apres_lacher_activite","apres_lacher_comportement","apres_lacher_parti","apres_lacher_vocalise")) {
          var_id <<- utf8(dbGetQuery(con,paste0(" SELECT var_id from listes.tr_variable_var where var_name_short = '",cpt_faon_v[j],"' ")))
          requ<<-paste0("INSERT INTO cmpt.tj_observation_alpha_aob (aob_var_id,aob_obs_id,aob_value, aob_insert_timestamp,aob_insert_source) Values (",var_id,",",cap_id,",'",input[[paste0(cpt_faon[j],i)]],"','",Sys.time(),"','Appli_DT-editable_V01')
          ON CONFLICT (aob_obs_id, aob_var_id) do update set (aob_value, aob_update_timestamp,aob_update_source) = (EXCLUDED.aob_value,'",Sys.time(),"','Appli_DT-editable_V01')")
          requ <<- gsub("'NA'","NULL", requ)
          requ <<- gsub("''","NULL", requ)
          dbSendQuery(con,requ)
          }###end appartenance variable a table des alphanum
          if (cpt_faon[j] %in% c("avant_capture_gite","apres_lacher_latence")){
          var_id <<- utf8(dbGetQuery(con,paste0(" SELECT var_id from listes.tr_variable_var where var_name_short = '",cpt_faon_v[j],"' ")))
          requ<<-paste0("INSERT INTO cmpt.tj_observation_integ_iob (iob_var_id,iob_obs_id,iob_value, iob_insert_timestamp,iob_insert_source) Values (",var_id,",",cap_id,",'",input[[paste0(cpt_faon[j],i)]],"','",Sys.time(),"','Appli_DT-editable_V01')
          ON CONFLICT (iob_obs_id, iob_var_id) do update set (iob_value, iob_update_timestamp,iob_update_source) = (EXCLUDED.iob_value,'",Sys.time(),"','Appli_DT-editable_V01')")
          requ <<- gsub("'NA'","NULL", requ)
          requ <<- gsub("''","NULL", requ)
          dbSendQuery(con,requ)
          }###end appartenance variable a table des entiers
          if (cpt_faon[j] %in% c("capture_heure")){
          var_id <<- utf8(dbGetQuery(con,paste0(" SELECT var_id from listes.tr_variable_var where var_name_short = '",cpt_faon_v[j],"' ")))
          requ<<-paste0("INSERT INTO cmpt.tj_observation_time_tob  (tob_var_id,tob_obs_id,tob_local_time_cest, tob_insert_timestamp,tob_insert_source) Values (",var_id,",",cap_id,",'",input[[paste0(cpt_faon[j],i)]],"','",Sys.time(),"','Appli_DT-editable_V01')
          ON CONFLICT (tob_obs_id, tob_var_id) do update set (tob_local_time_cest, tob_update_timestamp,tob_update_source) = (EXCLUDED.tob_local_time_cest,'",Sys.time(),"','Appli_DT-editable_V01')")
          requ <<- gsub("'NA'","NULL", requ)
          requ <<- gsub("''","NULL", requ)
          dbSendQuery(con,requ)
          }###end appartenance variable a table des temps
          }###end test input$configuration == "capture_faon"
          
          }###end condition not null reactive value
          } ###end loop for j
        
      
      } #####end parcours selection utilisateur allset
      load(champs,"v_individus_total")
      } #####end si action != "insert" alors update
  })  
  
  
#######quand on soumet les données de connection on transmet les droits de la bd_cefs aux utilisateurs de l'app  
observeEvent(input$soumettre, {
  login<<- input$login
  password<<- input$password
 source("scripts/droits_bdd.R")
####Remise à zéro des champs de saisie login/password
 updateTextInput(session,"login", value = "")
 updateTextInput(session,"password", value = "")
 load(selectedchamps,"v_individus_total") 
  })



#######integration des widgets de saisie dans le data_table
observeEvent(input$mod_table_rows_selected, {###on observe la ligne selectionnee
            sel<<- input$mod_table_rows_selected
            allsel<<- unique(append(allsel,sel)) #####collecte l'ensemble des id des lignes sélectionner pour les mettre à jour
            #print(allsel)
            #print(input$mod_table_rows_selected)
            #print(rows)
            print(input$mod_table_rows_selected != rows)
            if (input$mod_table_rows_selected != rows){ ####si la valeur de la ligne est différente de la valeur précédente (au debut rows = 0 voir global)
            rows<<-input$mod_table_rows_selected ####alors rows prend la valeur de la ligne sélectionnées
            if (autorisation == TRUE & dim(input_data)[2]>1){ ###si on a le droit de modifier les cellules et si il y a plus de colonne que juste ani_etiq
                                                             ###alors les scripts produisent les widgets html pour la ligne sélectionnée
            if (input$configuration == "animal") {source("scripts/datatable_wigets_animal.R")}
            if (input$configuration == "capture_faon") {source("scripts/datatable_wigets_capture_faon.R")}
            if (input$configuration == "comportement_faon") {source("scripts/datatable_wigets_comportement_faon.R")}
            if (input$configuration == "capture") {source("scripts/datatable_wigets_capture.R")}
            newdatatable() ###on rafraichi l'affichage pour charger les widgets
            }
        }
  
})

#######validation/intégration des modifications
# observeEvent(input$val_btn, {
#         if (action == "insert") {
#         source("scripts/insert_individu_capture.R");action <<- NULL; load(champs,"v_individus_total")} #####creation de l'individu et de la capture
#   
#             for (i in allsel){
#             if (is.na(allsel) | allsel == 0) {next}  
#             paste0(names(input_data),i)
# 
#             if (is.null(action)) {
#               source("scripts/insert_individu_capture.R");action <<- NULL; load(champs,"v_individus_total")}
#             
#             
#             }
#   })
              



  #v <- reactiveValues(data = input_data)
  
  ####conserver le trie voir la section 2.2 DataTables Information https://rstudio.github.io/DT/shiny.html
  proxy <- dataTableProxy("mod_table")
  
  
#
#####################permet d'afficher les valeurs prises par value
  # observe({
  # if (exists("sel")){
  #    output$sel = renderPrint({
  #    str(sapply(1:nrow(input_data), function(i) input[[paste0(names(input_data)[sel+1],i)]]))
  #    })}
  # })

# ##############lorsque l'utilisateur change la valeur d'une cellule du tableau si il en a le droit
#   
#    observeEvent(input$mod_table_cells_selected, { ###on observe la cellule selectionnee
#      
#       if (dim(input$mod_table_cells_selected)[1] != 0) { ### si il y a une ligne selectionne alors ...
#         sel<<- input$mod_table_cells_selected
#         if (input$mod_table_cells_selected[1,1] != rows){ ####si la valeur de la ligne est différente de la valeur précédente (au debut rows = 0 voir global)
#         rows<<-input$mod_table_cells_selected[1,1] ####alors rows prend la valeur de la ligne sélectionnées
#          if (autorisation == TRUE & dim(input_data)[2]>1){ ###si on a le droit de modifier les cellules et si il y a plus de colonne que juste ani_etiq
#                                                            ###alors les scripts produisent les widgets html pour la ligne sélectionnée
#           if (input$configuration == "animal") {source("scripts/datatable_wigets_animal.R")}
#           if (input$configuration == "faon") {source("scripts/datatable_wigets_faon.R")}
#           if (input$configuration == "capture") {source("scripts/datatable_wigets_capture.R")}
#             newdatatable() ###on rafraichi l'affichage pour charger les widgets
#           }
#       }
#       if (input$mod_table_cells_selected[1,2] != cols){ ####si la valeur de la colonne est différente de la valeur précédente (au debut cols = 0 voir global)
#          cols<<-input$mod_table_cells_selected[1,2]####alors cols prend la valeur de la colonne sélectionnées
#       }
#       }####fin du test de sélection, value est incrémenté après donc même si la case est désélectionnée, la valeur est quand même mise à jour (value dépend de sel et pas de input$mod_table_cells_selected sauf si cette dernière change)
#         print("sel")
#         print(sel)
#       ########value prend la valeur de la ligne et de la colonne sélectionnees
#       value<<- reactiveVal(sapply(1:nrow(input_data), function(i) input[[paste0(names(input_data)[sel[,2]+1],i)]])[sel[,1]])
#       print(isolate(value))
#       print("mod_table_cells_selected")
#       print(input$mod_table_cells_selected)
#       # if (exists("value")) {
#       # if (length(isolate(value())) != 0){
#       # if (isolate(value()) != saved_input_data[rows,cols+1]) {
#       # if (rows == 1 & names(input_data)[cols+1] == "ani_etiq"){
#       # if (length(grep(isolate(value()), input_data$ani_etiq)) != 0) {}#source("scripts/insert_capture.R")} else {source("scripts/insert_individu_capture.R")}
#       # } else {
#       # if (!is.null(isolate(value())[[1]])) {
#       # if (autorisation == TRUE & dim(input_data)[2]>1 & isolate(value())[[1]] != saved_input_data[rows,cols+1]){
#       # print(paste0("update row ",rows,", col ",names(input_data)[cols+1]," with ",isolate(value())," "))
#       # #source("scripts/update_field.R")
#       # }}}}
#       # #load(selectedchamps,"v_individus_total")
#       # #newdatatable()
#       # }}
#    })
   
  ########value prend la valeur de la ligne et de la colonne sélectionnees

  newdatatable()

  output$csv <- downloadHandler('donnees-filtrees.csv', content = function(file) {
    s <<- input$mod_table_rows_all
    write.csv2(utf8(input_data[s, , drop = FALSE]), file, fileEncoding = "UTF-8", row.names = FALSE)
  })
  
  observeEvent(input$reset, {
    load(champs,"v_individus_total")
  })

})