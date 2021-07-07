
# Define server
shinyServer(function(input, output, session) {


###########################################################################
#########observe if mandatory fields are filled or not#####################
###########################################################################
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)####check if mandatories field are true
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "subcol", condition = mandatoryFilled & input$eqt_id_usuel_conf == "") ###if ref and frequence of a collar are integrated then allow submission of a new collar
  })

  observe({
    # check if all mandatory fields have a value 
   
    mandatoryFilledconf <-
      vapply(fieldsMandatoryconf,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]]) 
             },
             logical(1))
    mandatoryFilledconf <- all(mandatoryFilledconf)####check if mandatories field are true
    # enable/disable the submit button
    shinyjs::toggleState(id = "subcolconf", condition = mandatoryFilledconf)###if ref and frequence of the collar are integrated then allow submission of a new configuration of the collar

  })
  
#  observe({ if (input$eqt_id_usuel_conf != "") {updateTextInput(session, "eqt_id_usuel", value = input$eqt_id_usuel_conf)}})
    
######if no gps/gsm/vhf selected then no frequence needed
  observeEvent({input$eqt_frequence
    input$eqt_teq_id 
  },{ 
    if (input$eqt_teq_id == 'NA'){
  updateTextInput(session, "eqt_frequence", value = "")}
  }
  )
######if gps/gsm/vhf selected then you need to enter a frequence  
  observeEvent({
                input$eqt_frequence
                input$eqt_teq_id
               },{
                 if (exists("input$eqt_id_usuel")){
                 if ((input$eqt_teq_id != "" & input$eqt_teq_id != "NA") & (is.null(input$eqt_frequence) | input$eqt_frequence == "")) {
                 shinyalert("Oops!", "le champ fréquence doit être rempli", type = "error")} else {return()}}
               }
)
  
##############################################################################################
###########Gather input informations from fields defined in fieldsAll vector##################
##############################################################################################
  
   output$colliers_existant<- renderUI({
    selectInput("eqt_id_usuel_conf", labelMandatory("N° du collier"), choices = append("",sort(dbGetQuery(con, "select distinct(eqt_id_usuel) from public.t_equipement_eqt")[,1])), selected = "")
       })
  
  formData <- reactive({
    dat <- sapply(fieldsAll, function(x) input[[x]])
    dat
  })

  formDataconf <- reactive({
    datconf <- sapply(fieldsAllconf, function(x) input[[x]])
    datconf
  })
  
#########################################  
#############Saving function#############
#########################################

#############Saving new collar ######### 
   
  saveData <- function(dat) {
    fileName <- "nouveau_collier.csv"
    dat <- as.data.frame(t(dat))
    ########check if eqt_id_usuel already exist inside the db and then give the choice to update existing data or delete entering data 
    verif<- dbGetQuery(con,"Select distinct(eqt_id_usuel) from public.t_equipement_eqt")
    if (length(grep(dat[,"eqt_id_usuel"],as.character(verif[,1])))!= 0) {shinyalert("Attention !!!", "ce N° de collier existe déjà",showCancelButton = TRUE, confirmButtonText = "Mettre à jour le collier existant à partir de la nouvelle entrée",cancelButtonText = "Ne pas tenir compte de la nouvelle entrée", type = "error", callbackR = function (va) {


#shinyalert choice I: update (confirm) or no duplacated entry#
##############################################################
      
        if (va == TRUE) {
        if (exists("responses")) {
          mat<-grep(paste0("^",dat$eqt_id_usuel,"$"),responses$eqt_id_usuel)
          if (length(mat) !=0) {responses <- rbind(responses[-mat,], dat);
          names(responses)<-names(dat);
          responses[,c("eqt_id_usuel", "eqt_frequence")] <- apply(responses[,c("eqt_id_usuel", "eqt_frequence")],2,noblank);
          responses<<-responses
          } else {
            responses <- rbind(responses, dat)
            names(responses)<-names(dat);
            responses[,c("eqt_id_usuel", "eqt_frequence")] <- apply(responses[,c("eqt_id_usuel", "eqt_frequence")],2,noblank)
            responses<<-responses
          }} else {
            dat[c("eqt_id_usuel", "eqt_frequence")]<- noblank(dat[c("eqt_id_usuel", "eqt_frequence")])
            responses <<- dat
          }
        
        # write.csv2(x = responses, file = file.path(responsesDir, fileName),
        #            row.names = FALSE, quote = TRUE)
        
        mod_id<-dbGetQuery(con, "SELECT * FROM public.tr_eqtmodel_mod")
        db<-responses
        ma<-match(db$eqt_mod_libelle, as.character(mod_id$mod_libelle))
        db$eqt_teq_id<-mod_id[ma,"mod_teq_id"]
        names(db)<-c("eqt_id_usuel","eqt_frequence","eqt_teq_id","eqt_mar_id","eqt_mod_id")
        db$eqt_mar_id<-mod_id[ma,"mod_mar_id"]
        db$eqt_mod_id<-mod_id[ma,"mod_id"]
        
        for (i in 1:dim(db)[1]){
          dbSendQuery(con,paste0("insert into public.t_equipement_eqt
                                 (eqt_teq_id, eqt_id_usuel, eqt_mar_id, eqt_mod_id, eqt_accelerometrie, eqt_proximite, eqt_frequence)
                                 values (",db[i,3],",'", db[i,1],"',", db[i,4],",", db[i,5],",NULL,NULL,", if(is.na(as.numeric(db[i,2]))) {'NULL'} else{db[i,2]},") ON CONFLICT ON CONSTRAINT c_uni_eqt_id_usuel DO UPDATE SET eqt_teq_id = EXCLUDED.eqt_teq_id, eqt_mar_id = EXCLUDED.eqt_mar_id, eqt_mod_id =EXCLUDED.eqt_mod_id, eqt_frequence =EXCLUDED.eqt_frequence;"))
        }
        output$responses <- DT::renderDataTable({
          loadData()})
        } else {}
        }
    )} else { 
        
########no entry duplicated
###########################
        
        if (exists("responses")) {
          responses <- rbind(responses, dat)
          names(responses)<-names(dat);
          responses[,c("eqt_id_usuel", "eqt_frequence")] <- apply(responses[,c("eqt_id_usuel", "eqt_frequence")],2,noblank)
          responses <<- responses
        }
        else {
          dat[,c("eqt_id_usuel", "eqt_frequence")]<- apply(dat[,c("eqt_id_usuel", "eqt_frequence")],2,noblank)
          responses <<- dat
        }
        
        # write.csv2(x = responses, file = file.path(responsesDir, fileName),
        #            row.names = FALSE, quote = TRUE)
        
       
        mod_id<-dbGetQuery(con, "SELECT * FROM public.tr_eqtmodel_mod")
        db<-responses
        ma<-match(db$eqt_mod_libelle, as.character(mod_id$mod_libelle))
        #if (tolower(trimws(input$eqt_id_usuel)) == "vhf"){eqt_id_usuel<- paste0(input$eqt_id_usuel,"_", dbGetQuery(con,paste0("SELECT MAX(eqt_id) from public.t_equipement_eqt"))+1," ")}
        db$eqt_teq_id<-mod_id[ma,"mod_teq_id"]
        db$eqt_mar_libelle<-mod_id[ma,"mod_mar_id"]
        db$eqt_mod_libelle<-mod_id[ma,"mod_id"]
        
        for (i in 1:dim(db)[1]){
          dbSendQuery(con,paste0("insert into public.t_equipement_eqt
    (eqt_teq_id, eqt_id_usuel, eqt_mar_id, eqt_mod_id, eqt_accelerometrie, eqt_proximite, eqt_frequence)
    values (",db[i,3],",'", db[i,1],"',", db[i,4],",", db[i,5],",NULL,NULL,", if(is.na(as.numeric(db[i,2]))) {'NULL'} else{as.numeric(db[i,2])},") ON CONFLICT ON CONSTRAINT c_uni_eqt_id_usuel DO NOTHING;"))
        }
        output$responses <- DT::renderDataTable({
          loadData()})
      }
    }
#############end saving new collar ########## 

#############Saving new configuration ######### 
  
  saveDataconf <- function(datconf) { 
    
    fileNamec <- "nouvelle_configuration.csv"
    datconf <<- as.data.frame(t(datconf))
    ########check if eqt_id_usuel already exist inside the db and then give the choice to update existing data or delete entering data 
    verif<<- dbGetQuery(con,
                       paste0("SELECT eqt_id_usuel
      FROM public.t_equipement_conf_eqc 
      LEFT JOIN  t_equipement_eqt ON eqt_id = eqc_eqt_id WHERE eqc_annee_suivi = ",input$eqc_annee_suivi,";"))
  if (dim(verif)[1] != 0){verif<-verif[,1]}else{verif<-"NA"}

  if (length(grep(datconf["eqt_id_usuel_conf"],as.character(verif)))!= 0) {
    shinyalert("Attention !!!", "ce  collier a déjà une configuration pour cette année",showCancelButton = TRUE, confirmButtonText = "Mettre à jour la configuration existante à partir de la nouvelle entrée",cancelButtonText = "Ne pas tenir compte de la nouvelle entrée", type = "error", callbackR = function (va2) {
    
      #shinyalert choice I: update (confirm) or duplacated entry#
      ##############################################################
    
      if (va2 == TRUE) { ####
        if (exists("responsesconf")) {
          mat<-grep(paste0("^",datconf["eqt_id_usuel_conf"],"$"),responsesconf$eqt_id_usuel_conf)
          responsesconf <- rbind(responsesconf[-mat,], datconf);
          responsesconf[,c("eqt_id_usuel_conf", "eqc_drop_off", "eqc_memoire")] <- apply(responsesconf[,c("eqt_id_usuel_conf", "eqc_drop_off", "eqc_memoire")],2,noblank);
          responsesconf<<-responsesconf
          } else {
            responsesconf <-  as.data.frame(t(datconf))
            responsesconf[c("eqt_id_usuel_conf", "eqc_drop_off", "eqc_memoire")] <- noblank(responsesconf[c("eqt_id_usuel_conf", "eqc_drop_off", "eqc_memoire")])
            responsesconf<<-responsesconf
          }
        # write.csv2(x = responsesconf, file = file.path(responsesDir, fileNamec),
        #            row.names = FALSE, quote = TRUE)
        db<<-responsesconf
        for (i in 1:dim(db)[1]){
        db[i,"eqt_id_usuel_conf"]<-dbGetQuery(con, paste0("SELECT eqt_id FROM public.t_equipement_eqt where eqt_id_usuel = '",as.character(db[i,"eqt_id_usuel_conf"]),"' "))
        }
        
        
        for (i in 1:dim(db)[1]){
          
          if (i/dim(db)[1] > 0.5) {update_modal_progress(i/dim(db)[1])}
          db[i,"sen_association"]<-dbGetQuery(con, paste0("SELECT sen_id FROM listes.tr_sensors_sen where sen_association = '",db[i,"sen_association"],"' "))
          req <- paste0("insert into public.t_equipement_conf_eqc
                        (eqc_eqt_id, eqc_annee_suivi, eqc_drop_off, eqc_couleur_boitier, eqc_couleur_collier, eqc_memoire, eqc_sen_id, eqc_remarque, eqc_pose)
                        values (",db[i,1],",'", db[i,3],"','", db[i,2],"','", db[i,4],"','", db[i,5],"','", db[i,7],"','", db[i,6],"','",db[i,8],"',FALSE) ON CONFLICT (eqc_eqt_id, eqc_annee_suivi) DO UPDATE SET eqc_drop_off = EXCLUDED.eqc_drop_off, eqc_couleur_boitier = EXCLUDED.eqc_couleur_boitier, eqc_couleur_collier = EXCLUDED.eqc_couleur_collier, eqc_sen_id =EXCLUDED.eqc_sen_id, eqc_memoire =EXCLUDED.eqc_memoire, eqc_remarque = EXCLUDED.eqc_remarque;")
          req <- gsub("'NA'","NULL", req)
          req <- gsub("''","NULL", req)
          
          dbSendQuery(con,req)
        }
        remove_modal_progress()
        output$responsesconf <- DT::renderDataTable({
          loadDataconf()
        })
        output$colliers_existant<- renderUI({
          selectInput("eqt_id_usuel_conf", labelMandatory("N° du collier"), choices = append("",sort(dbGetQuery(con, "select distinct(eqt_id_usuel) from public.t_equipement_eqt")[,1])), selected = "")
        })
      } else {
        output$colliers_existant<- renderUI({
        selectInput("eqt_id_usuel_conf", labelMandatory("N° du collier"), choices = append("",sort(dbGetQuery(con, "select distinct(eqt_id_usuel) from public.t_equipement_eqt")[,1])), selected = "")
      })}
        }
    )} else { 
      
      ########pas d'entree dupliquee
      ###########################
      
      if (exists("responsesconf")) {
        responsesconf <- rbind(responsesconf, datconf)
        names(responsesconf)<-names(datconf);
        responsesconf[,c("eqt_id_usuel_conf", "eqc_drop_off", "eqc_memoire")] <- apply(responsesconf[,c("eqt_id_usuel_conf", "eqc_drop_off", "eqc_memoire")],2,noblank);
        responsesconf<<-responsesconf
      } else {
        datconf[c("eqt_id_usuel_conf", "eqc_drop_off", "eqc_memoire")]<- noblank(datconf[c("eqt_id_usuel_conf", "eqc_drop_off", "eqc_memoire")]);
        responsesconf <<- as.data.frame(t(datconf))
      }
    
      # write.csv2(x = responsesconf, file = file.path(responsesDir, fileNamec),
      #            row.names = FALSE, quote = TRUE)
      
      db<-responsesconf
      for (i in 1:dim(db)[1]){
        db[i,"eqt_id_usuel_conf"]<-dbGetQuery(con, paste0("SELECT eqt_id FROM public.t_equipement_eqt where eqt_id_usuel = '",as.character(db[i,"eqt_id_usuel_conf"]),"' "))
      }
      
      
      for (i in 1:dim(db)[1]){
        if (i/dim(db)[1] > 0.5) {update_modal_progress(i/dim(db)[1])}
        db[i,"sen_association"]<-dbGetQuery(con, paste0("SELECT sen_id FROM listes.tr_sensors_sen where sen_association = '",db[i,"sen_association"],"' "))
        req <- paste0("insert into public.t_equipement_conf_eqc
                                 (eqc_eqt_id, eqc_annee_suivi, eqc_drop_off, eqc_couleur_boitier, eqc_couleur_collier, eqc_memoire, eqc_sen_id, eqc_remarque, eqc_pose)
               values (",db[i,1],",'", db[i,3],"','", db[i,2],"','", db[i,4],"','", db[i,5],"','", db[i,7],"','", db[i,6],"','",db[i,8],"',FALSE) ON CONFLICT (eqc_eqt_id, eqc_annee_suivi) DO UPDATE SET eqc_couleur_boitier = EXCLUDED.eqc_couleur_boitier, eqc_couleur_collier = EXCLUDED.eqc_couleur_collier, eqc_sen_id =EXCLUDED.eqc_sen_id, eqc_memoire =EXCLUDED.eqc_memoire, eqc_remarque = EXCLUDED.eqc_remarque;")
        req <- gsub("'NA'","NULL", req)
        req <- gsub("''","NULL", req)
        
        dbSendQuery(con,req)

      }
      remove_modal_progress()
      output$responsesconf <- DT::renderDataTable({
        loadDataconf()
      })
      output$colliers_existant<- renderUI({
        selectInput("eqt_id_usuel_conf", labelMandatory("N° du collier"), choices = append("",sort(dbGetQuery(con, "select distinct(eqt_id_usuel) from public.t_equipement_eqt")[,1])), selected = "")
      })
    }
  }

#############end saving new configuration ##############

#########################################
#########END OF THE SAVING FUNCTION######
#########################################
  
#########################################
#############Loading function############
#########################################
                
  loadData <- function() {
    if (file.exists("Donnees/nouveau_collier.csv")) {
      #autoInvalidate() ###when commented browser have to be refresh manualy when data are updated, when active html table refresh every 5 sec
      #responses <<-  fread(paste0("Données/",fileName,".csv"), data.table = FALSE)
      responses <<-  fread("Donnees/nouveau_collier.csv", data.table = FALSE)
      responses
    }
  }

  loadDataconf <- function() {
    responsesconf <<- utf8(as.data.frame(dbGetQuery(con,paste0("SELECT eqt_id_usuel eqt_id_usuel_conf, eqc_drop_off eqc_drop_off, eqc_annee_suivi eqc_annee_suivi, eqc_couleur_collier eqc_couleur_collier, eqc_couleur_boitier eqc_couleur_boitier, sen_association sen_association, eqc_memoire eqc_memoire, eqc_remarque eqc_remarque
                                              FROM public.t_equipement_conf_eqc 
                                              LEFT JOIN  t_equipement_eqt ON eqt_id = eqc_eqt_id 
                                              LEFT JOIN  listes.tr_sensors_sen ON sen_id = eqc_sen_id
                                              WHERE eqc_annee_suivi = ",input$eqc_annee_suivi,";"))))
    if (exists("responsesconf")) {responsesconf}
  }
#####################################################
############Action button to submit a new collar#####
#####################################################

  # action to take when submit button is pressed
  observeEvent(input$subcol, {
    saveData(formData()); updateTextInput(session, "eqt_id_usuel", value = "");updateTextInput(session, "eqt_id_usuel_user", value =  cat(tail(responses[,1],1)));updateTextInput(session, "eqt_frequence", value = "");updateSelectInput(session, "eqt_teq_id",selected = "");updateSelectInput(session, "eqt_mar_libelle",selected = "");updateSelectInput(session,"eqt_mod_libelle",selected = "")
  } )

  observeEvent(input$subcolconf, { #show_modal_progress_line();update_modal_progress(0.1);update_modal_progress(0.2);update_modal_progress(0.3);update_modal_progress(0.4);update_modal_progress(0.5)
    saveDataconf(formDataconf()); updateSelectInput(session, "eqc_id_usuel_conf", selected = "");updateSelectInput(session, "sen_association", selected = "rien"); updateTextInput(session, "eqc_drop_off", value = "");updateSelectInput(session, "eqc_couleur_boitier",selected = "");updateSelectInput(session, "eqc_couleur_collier",selected = "");updateSelectInput(session,"eqc_sen_id",selected = "rien");updateTextInput(session,"eqc_remarque", value = NULL);updateNumericInput(session, "eqc_memoire", value = '');updateSelectInput(session, "eqc_freq",selected = "") #;updateTextInput(session, "eqc_id_usuel_conf_user", value =  cat(tail(responsesconf[,1],1)))
  } )
#####################################################
############Display HTML result in the ui interface##
#####################################################
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
    input$subcol
    loadData()
  })

  output$responsesconf <- DT::renderDataTable({
    input$subcolcon
    loadDataconf()
  })
##########################################################################################
############Make conditional choices in selectinput according to the previous choices#####
##########################################################################################    
     
      output$control1 <- renderUI({
        selectInput("eqt_teq_id", labelMandatory("Type de collier"), choices = df$option1)
      })
      
      output$control2 <- renderUI({
        x <- input$eqt_teq_id
        if (any(
          is.null(x)
        ))
          return("Select")
     choice2 <- df[df$option1 == x, 
                      "option2"]
       selectInput("eqt_mar_libelle", labelMandatory("Marque du collier"), choices = choice2, selected =head(unique(choice2),1))
      })
      
      output$control3 <- renderUI({
        x <- input$eqt_teq_id
        y <- input$eqt_mar_libelle
        if (any(
          is.null(x),
          is.null(y)
        )) 
          return("Select")
        
        choice3 <- df[df$option1 == x & df$option2 == y,
                      "option3"]
        
       selectInput("eqt_mod_libelle", labelMandatory("Model du collier"), choices = choice3)
       })
      
#######Si boitier est faon, alors collier est faons
      
      observeEvent(input$eqc_couleur_boitier, {
        if (input$eqc_couleur_boitier == "faon") {updateSelectInput(session, "eqc_couleur_collier",selected = "faon")}
      })
      observeEvent(input$eqc_couleur_boitier, {
        if (input$eqc_couleur_boitier != "faon" & input$eqc_couleur_collier == "faon") {updateSelectInput(session, "eqc_couleur_collier",selected = "")}
      })
      observeEvent(input$eqc_couleur_collier, {
        if (input$eqc_couleur_collier == "faon") {updateSelectInput(session, "eqc_couleur_boitier",selected = "faon")}
      })
      observeEvent(input$eqc_couleur_collier, {
        if (input$eqc_couleur_collier != "faon" & input$eqc_couleur_boitier == "faon") {updateSelectInput(session, "eqc_couleur_boitier",selected = "")}
      })
######si mémoire déjà utilisé cette année on annule
      if (exists("responsesconf")){
      observeEvent(input$eqc_memoire,{
       if (input$eqc_memoire %in% (responsesconf$eqc_memoire)) {shinyalert("Oops!", "Cette fréquence est déjà attribuée pour cette année",showCancelButton = TRUE, confirmButtonText = "Conserver cette mémoire",cancelButtonText = "Changer la mémoire", type = "error", callbackR = function (va3) { if (va3 == FALSE) {updateNumericInput(session, "eqc_memoire",value = '')} else {}})}
      })
      }
######si c'est un collier vhf alors l'identifiant du collier est vhf_eqt_id
      observeEvent(input$eqt_teq_id, {
        if (input$eqt_teq_id == "VHF") {updateTextInput(session, "eqt_id_usuel", value = paste0("vhf_",dbGetQuery(con,"SELECT  max(eqt_id) FROM public.t_equipement_eqt left join  public.tr_type_equipement_teq ON eqt_teq_id =teq_id where teq_nom_court = 'VHF'")))}
})
######recherche des colliers whf à l'aide de la fréquence
      observeEvent(input$eqc_freq, { print(as.numeric(input$eqc_freq))
        if (input$eqc_freq != ""){updateTextInput(session, "eqt_id_usuel_conf", value = as.character(dbGetQuery(con,paste0("SELECT  eqt_id_usuel FROM public.t_equipement_eqt where eqt_frequence = ",as.numeric(input$eqc_freq)," and eqt_mar_id = 4 and eqt_mod_id = 8 and eqt_teq_id = 1 order by eqt_id DESC"))[1,]))}
})
######quand on change l'année de suivi, on recharche des données pour l'année consernée1
})
                                                                                                                                                    