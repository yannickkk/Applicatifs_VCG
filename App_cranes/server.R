############################mETTRE LES CRANES EN COLLECTION###########################################
#### author: Yannick Chaval, INRAE (French National Institute for Agricultural Research), CEFS (Wildlife, Behaviour and Ecology Research Unit)
#### date: 16-03-2022
#### target db: db_CEFS
#### objet: permet de rentrer les cranes préparés en collection et exporte le fichier d'import de masse pour collec sciences
#### 
#### 
#### 
#### 
#### 
#########################################################################################################
#con<-local_gardouch
host<<-"pggeodb.nancy.inra.fr"
con<<- dbConnect(PostgreSQL(), host= host, dbname="db_chevreuils", user="ychaval",password="bvta;814")


# Define server
shinyServer(function(input, output, session) {
  
  
  close_session <- function() {
    mortauxcons()
    stopApp()
  }
  session$onSessionEnded(close_session)

  
  ###########################################################################
  #########observe if mandatory fields are filled or not#####################
  ###########################################################################
  
  observe({
    # check if all mandatory fields have a value
    #####problème avec la date d'entrée qui doit être mandataire mais il ne reconnait pas le format nulle date print(input$ani_date_arrivee)
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(as.character(input[[x]])) && as.character(input[[x]]) != "" && !is.na(as.character(input[[x]])) && isTruthy(as.character(input[[x]]))
             },
             logical(1))
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "save", condition = all(mandatoryFilled)) ###if ref and frequence of a collar are integrated then allow submission of a new collar
  })
  
   observeEvent(input$ani_etiq, { restDisplay()
      next_skull<<-max(as.numeric(str_sub(wsr(enc2utf8(dbGetQuery(con,"select sample_identifier FROM listes.t_osteo_ost;")[,1]))[!is.na(wsr(enc2utf8(dbGetQuery(con,"select sample_identifier FROM listes.t_osteo_ost;")[,1])))],1,3)))+1
      output$container_parent_identifier <- renderUI({div(textInput("container_parent_identifier", label= labelMandatory("Etagère (douchette)")),id="text1input")})
      output$sample_line <- renderUI({div(numericInput("sample_line", label= labelMandatory("ligne:1 devant, 2 derrière"),value = 0 , step = 1),id="num1input")})
      output$sample_column <- renderUI({div(numericInput("sample_column", label= labelMandatory("colonne:gauche à droite"),value = 0 , step = 1),id="num2input")})
            if (input$ani_etiq != ""){
              loadData()
            if (input$ani_etiq == "inconnu") {
      output$ani_crane <- renderText({ paste0("Code collection: ",as.character(next_skull)," 000A")})
      output$md_remark <- renderUI({div(textInput("md_remark", label= "remarque"),id="text2input")})
      output$sampling_date <- renderUI({div(dateInput("sampling_date", label= "date de collecte"),id="dateinput")})
     } else {
      #####si animal vivan, alerte
      if(!utf8(as.data.frame(dbGetQuery(con,paste0("SELECT ani_mortalite from v_individus_total where ani_etiq ='",input$ani_etiq,"'"))))[,1][1]){
      shinyalert("Animal vivant !!", "Cette animal n'est pas déclaré comme mort dans la base de données: contactez votre gestionnaire de base de données !", type = "error")}
      output$ani_crane <- renderText({paste0("Code collection: ",as.character(next_skull)," ",input$ani_etiq,"A")})
      # removeUI(selector = "#text1input")
      # removeUI(selector = "#num1input")
      # removeUI(selector = "#num2input")
      removeUI(selector = "#dateinput")
      #removeUI(selector = "#text2input")
     }}
    })

   
   #########################################  
   #############Saving function#############
   #########################################
  
   #############Saving new individual or update an existing individual ######### 
   
  saveData <- function() {
    if(input$ani_etiq == "inconnu") { 
    sampling_date<<-format(as.Date(input$sampling_date), "%Y-%m-%d");
    cont<<-input$container_parent_identifier
    spl<<-input$sample_line
    spc<<-input$sample_column
    rk<<-input$md_remark
    print(sampling_date)
    dbSendQuery(con,paste0("insert into listes.t_osteo_ost (sample_identifier,container_parent_identifier,sample_line,sample_column,sampling_date,md_remark, date) values
   ('",paste0("",as.character(next_skull)," 000A"),"',
     '",cont,"',
     ",ifelse(spl ==0,"NULL",spl),",
     ",ifelse(spc ==0,"NULL",spc),",
     ",ifelse(sampling_date == ""||is.na(sampling_date)||length(sampling_date) == 0,"NULL",paste0("'",sampling_date,"'")),",
     '",rk,"',
     '",Sys.Date(),"') ON CONFLICT (sample_identifier) DO UPDATE SET (container_parent_identifier,sample_line,sample_column,sampling_date,md_remark, date)=(EXCLUDED.container_parent_identifier,EXCLUDED.sample_line,EXCLUDED.sample_column,EXCLUDED.sampling_date,EXCLUDED.md_remark,EXCLUDED.date)")
    )
    if (length(sampling_date) != 0){
      dbSendQuery(con,paste0("update listes.t_osteo_ost set campaign_name = extract(year from sampling_date) where sample_identifier = '",paste0("",as.character(next_skull)," 000A"),"' "))
    }
    
    } else { 
    crane<- dbGetQuery(con,paste0("SELECT ani_crane from t_animal_ani where ani_etiq= '",wsr(input$ani_etiq),"' "))[,1]

    if (!is.na(crane)){
    shinyalert("Crâne déjà en collection !!", "Le crâne de cet animal est déjà en Collection", type = "error", confirmButtonText = "Annuler la mise à jour")
    } else {
    dbSendQuery(con,paste0("update t_animal_ani set ani_crane = '",paste0("",as.character(next_skull)," ",input$ani_etiq,""),"' where ani_etiq = '",input$ani_etiq,"' "))

    date_mort<<-format(as.Date(dbGetQuery(con,paste0("SELECT ani_date_mort from t_animal_ani where ani_etiq= '",input$ani_etiq,"' "))[,1]), "%Y-%m-%d")
    dbSendQuery(con,paste0("insert into listes.t_osteo_ost (sample_identifier,container_parent_identifier,sample_line,sample_column,sampling_date,md_remark, date) values
                           ('",paste0("",as.character(next_skull)," ",input$ani_etiq,"A"),"',
                            '",input$container_parent_identifier,"',
                            ",ifelse(input$sample_line ==0,"NULL",input$sample_line),",
                            ",ifelse(input$sample_column ==0,"NULL",input$sample_line),",
                            ",ifelse(date_mort == ""||is.na(date_mort)||length(date_mort) == 0,"NULL",paste0("'",date_mort,"'")),",
                            '",input$md_remark,"',
                            '",Sys.Date(),"'
                            ) ON CONFLICT (sample_identifier) DO UPDATE SET (container_parent_identifier,sample_line,sample_column,sampling_date,md_remark, date)=(EXCLUDED.container_parent_identifier,EXCLUDED.sample_line,EXCLUDED.sample_column,EXCLUDED.sampling_date,EXCLUDED.md_remark,EXCLUDED.date)
                           "))
    if (length(sampling_date) != 0){
    dbSendQuery(con,paste0("update listes.t_osteo_ost set campaign_name = extract(year from sampling_date) where sample_identifier = '",paste0("",as.character(next_skull)," ",input$ani_etiq,""),"' "))
    }
    restDisplay()
    }}
    }

   
   #########################################
   #########END OF THE SAVING FUNCTION######
   #########################################
   
   #########################################
   #############Loading function############
   #########################################
   
  loadData <- function() {
    if(input$ani_etiq != "" & input$ani_etiq != "inconnu"){
    registre<<-utf8(as.data.frame(dbGetQuery(con,paste0("SELECT ani_crane, ani_etiq, ani_name, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal, ani_sexe, ani_mortalite, ani_date_mort, ani_cause_mort
    FROM public.v_individus_total where ani_etiq = '",input$ani_etiq,"'"))))} else {
      registre<<-utf8(as.data.frame(dbGetQuery(con,paste0("SELECT ani_crane, ani_etiq, ani_name, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal, ani_sexe, ani_mortalite, ani_date_mort, ani_cause_mort
    FROM public.v_individus_total;"))))  
    }
    registre
  }
  
   restDisplay <- function() {
     output$registre <- DT::renderDataTable({
       loadData()
       DT::datatable(registre, filter = "top",rownames = F, options = list(lengthMenu = c(10, 50, 100, 200, 300), pageLength = 250))
     })
   }
  ###############################################################################################
  ############Lance l'enregistrement des données qd submit est cliqué si le controle est ok #####
  ###############################################################################################
  

  observeEvent(input$save, {
      saveData();
      updateSelectizeInput(session, "ani_crane", append("inconnu",enc2utf8(dbGetQuery(con,"select distinct(ani_etiq), ani_id FROM public.v_individus_total order by ani_id;")[,1])),selected ="inconnu")
      next_skull<-max(as.numeric(str_sub(wsr(enc2utf8(dbGetQuery(con,"select sample_identifier FROM listes.t_osteo_ost;")[,1]))[!is.na(wsr(enc2utf8(dbGetQuery(con,"select sample_identifier FROM listes.t_osteo_ost;")[,1])))],1,3)))+1
      output$ani_crane <- renderText({ paste0("Code collection: ",as.character(next_skull)," 000A")})
      output$container_parent_identifier <- renderUI({div(textInput("container_parent_identifier", label= "Etagère (douchette)"),id="text1input")})
      output$sample_line <- renderUI({div(numericInput("sample_line", label= "1 devant, 2 derrière",value = 0 , step = 1),id="num1input")})
      output$sample_column <- renderUI({div(numericInput("sample_column", label= "colonne de gauche à droite",value = 0 , step = 1),id="num2input")})
      output$sampling_date <- renderUI({div(dateInput("sampling_date", label= "date de collecte"),id="dateinput")})
      output$md_remark <- renderUI({div(textInput("md_remark", label= "remarque"),id="text2input")})
  })
#   
  observeEvent(input$new, { 
    updateSelectizeInput(session, "ani_crane", append("inconnu",enc2utf8(dbGetQuery(con,"select distinct(ani_etiq), ani_id FROM public.v_individus_total order by ani_id;")[,1])),selected ="inconnu")
    next_skull<-max(as.numeric(str_sub(wsr(enc2utf8(dbGetQuery(con,"select sample_identifier FROM listes.t_osteo_ost;")[,1]))[!is.na(wsr(enc2utf8(dbGetQuery(con,"select sample_identifier FROM listes.t_osteo_ost;")[,1])))],1,3)))+1
    output$ani_crane <- renderText({ paste0("Code collection: ",as.character(next_skull)," 000A")})
    output$container_parent_identifier <- renderUI({div(textInput("container_parent_identifier", label= "Etagère (douchette)"),id="text1input")})
    output$sample_line <- renderUI({div(numericInput("sample_line", label= "1 devant, 2 derrière",value = 0 , step = 1),id="num1input")})
    output$sample_column <- renderUI({div(numericInput("sample_column", label= "colonne de gauche à droite",value = 0 , step = 1),id="num2input")})
    output$sampling_date <- renderUI({div(dateInput("sampling_date", label= "date de collecte"),id="dateinput")})
    output$md_remark <- renderUI({div(textInput("md_remark", label= "remarque"),id="text2input")})
    })
  
   
   #########export des données      
   
  output$downloadcsv <-  downloadHandler(
    filename = function() {
      paste0("Crane_Collec_", gsub("-","_",Sys.Date()),".csv")
    },
    content = function(file) {
      csv<-dbGetQuery(con,paste0("select sample_identifier, sample_multiple_value, container_parent_identifier, sample_line, sample_column, md_remark, sample_type_name, collection_name, sample_status_name, campaign_name, country_code, sampling_place_name, referent_name, wgs84_x, wgs84_y, sampling_date, expiration_date, md_location_accuracy, md_species from listes.t_osteo_ost where date = '",Sys.Date(),"' "))
      write.csv(csv, file, row.names = FALSE,na = "",fileEncoding = "UTF-8")
    }
  )
})

