source("global.R")
source("connect.R")

##################                  SERVER                 ################# 
################## creation des fichiers de sauvegarde csv vides #################
noms_colonnes<- c("N°Animal","ani_nom","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_pertinent","cap_lactation","RFID","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit","glucose","T°C_ext","TIQUES FIXES","autres parasites", "Peau","poils","sang","feces","tiques","vaginal","Nasal","remarque","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)","num_sabot","couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2","ventilation","remarque_table","localisation sonde temperature","eurodeer","titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque_generale","bague","autre","stop","habitat lacher","habitat perte vue","visibilite","nb_public","eurodeer_lacher","remise sabot","hre_lacher_2")
d<-data.frame(t(rep(NA,length(noms_colonnes))))
names(d)<- noms_colonnes; d<-na.omit(d)
if (length(grep(paste0("captures_",gsub("-","_",Sys.Date()), ".csv"),list.files())) == 0) {setwd(tousb); write.table(d , file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), append = TRUE, col.names= TRUE, na="", row.names = F, sep=";"); setwd(tosd)}
if (length(grep(paste0("captures_",gsub("-","_",Sys.Date()), ".csv"),list.files())) == 0) {setwd(tosd); write.table(d , file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), append = TRUE, col.names= TRUE, na="", row.names = F, sep=";")}


df_prelevement <- data.frame(choix[["prel"]])

colnames(df_prelevement)<-c("prel_type","prel_local","prel_condi", "prel_solv")


## Dataframe pour les blessures :

df_blessure <- data.frame(dbGetQuery(con,"select bll_localisation from listes.tr_blessure_localisation_bll, listes.tr_blessure_gravite_blg where blg_bll_id=bll_id"),
                          dbGetQuery(con, "select blg_gravite from listes.tr_blessure_localisation_bll, listes.tr_blessure_gravite_blg where blg_bll_id=bll_id"))

colnames(df_blessure)<-c("ble_local","ble_gravite")



server <- function(input, output,session) {
  
  ##################              RUBRIQUE ANIMAL                                   #################
  
  ##stop server when browser is closed
  # close_session <- function() {
     #dbDisconnect(con)
  #   stopApp()
  # }
  # session$onSessionEnded(close_session)
  ######si ça plante shiny se relance
  options(shiny.autoreload = TRUE)

  ###mise à jour des numeric input
  updateNumericInput(session, "cirCou", max = choix[["cirCou"]])
  updateNumericInput(session, "lPattArriere", max = choix[["lPattArriere"]])
  updateNumericInput(session, "lBoisGauche", max = choix[["lBoisGauche"]])
  updateNumericInput(session, "lBoisDroit", max = choix[["lBoisDroit"]])

  #####→remise a jour dynamique des champs (si cela est placé dans global.R alors ce n'est lu qu'une fois en début de lancement de serveur)
  updateSelectizeInput(session, "idRFID2", choices = c(choisir = "", dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id")[,1]), selected = "")
  updateSelectInput(session, "idRFID", choices = c(choisir = "", dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null")[,1]), selected = "")
  updateSelectInput(session, "idSite", choices = choix[["idSite"]], selected = "")
  updateSelectizeInput(session, "nAnimal2", choices =c(choisir = "", dbGetQuery(con,"select ani_etiq from public.t_animal_ani order by ani_id DESC")[,1]), selected = "")
  updateSelectInput(session, "etatBois", choices = choix[["etatBois"]], selected = "velours")
  updateSelectInput(session, "idSite2", choices = choix[["idSite2"]], selected = "")
  updateSelectInput(session, "numSabot", choices = choix[["numSabot"]], selected = "") 
  
  #########          Sélection site/RFID/tag à partir du n°animal                   #########
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_gauche from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      idTagOrG2 <- resres[1,1]
      updateSelectizeInput(session, "idTagOrG2",  selected = (idTagOrG2))
    }
  })
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_droit from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and  ani_etiq = '", input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      idTagOrD2 <- resres[1,1]
      updateSelectizeInput(session, "idTagOrD2", selected = idTagOrD2)
    }
  })
  
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id and ani_etiq='",input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      idTagRfid <- resres[1, 1]
      if (!is.null(idTagRfid)){
        updateSelectizeInput(session, "idRFID2", selected = idTagRfid)
      }
      else {(updateSelectizeInput(session, "idRFID2", selected = ""))} #, options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'))
    }
  })
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select sit_nom_court from public.tr_site_capture_sit where (sit_id in (select cap_sit_id from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and ani_etiq = '", input$nAnimal2, "' order by cap_date DESC))")
      resres = dbGetQuery(con,str)
      idSite2 <- resres[1, 1]
      updateSelectInput(session, "idSite2", selected = idSite2)
    }
  })
  
  observeEvent(input$nAnimal2, {
    if ((input$nAnimal2)!="") {
      str = paste0("select ani_sexe from public.t_animal_ani where ani_etiq ='", input$nAnimal2, "'")
      resres = dbGetQuery(con,str)
      sexe <- resres[1, 1]
      updateAwesomeRadio(session, "sexe", selected = sexe)
    }
  })
  
  observeEvent(input$nAnimal2, {
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_poids from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and ani_etiq = '", input$nAnimal2, "' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      poids <- resres[1, 1]
      output$poids_ani_anc <- renderText({poids})
    }
  })
  
  testNouvelAnimal = observeEvent(input$estNouvelAnimal, {
    if (input$estNouvelAnimal=="non"){
      updateAwesomeRadio(session, "identifie", choices = c("oui","non"), selected = "oui")
    }
    if (input$estNouvelAnimal=="oui"){
      updateAwesomeRadio(session, "identifie", choices = c("oui","non"), selected = "non")
    }  
  })
  
  #########          Sélection nAnimal/RFID/site/tagG à partir du tagD              #########
  
  observeEvent(input$idTagOrD2,{
    if ((input$idTagOrD2)!="") {
      str = paste0("select ani_etiq from public.t_animal_ani, public.t_capture_cap where cap_ani_id = ani_id and cap_tag_droit ='", input$idTagOrD2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      nAnimalFound <- resres[1,1]
      updateSelectizeInput(session, "nAnimal2", selected = nAnimalFound)
    }
  })
  
  #########          Sélection nAnimal/RFID/site/tagD à partir du tagG              #########
  
  observeEvent(input$idTagOrG2,{
    if ((input$idTagOrG2)!="") {
      str = paste0("select ani_etiq from public.t_animal_ani, public.t_capture_cap where cap_ani_id = ani_id and cap_tag_gauche ='", input$idTagOrG2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      nAnimalFound <- resres[1,1]
      updateSelectizeInput(session, "nAnimal2", selected = nAnimalFound)
    }
  })
  
  #########          Sélection nAnimal/site/tagD/tagG à partir du RFID              #########
  
  observeEvent(input$idRFID2,{
    if ((input$idRFID2)!="") {
      str = paste0("select ani_etiq from public.t_animal_ani, public.t_capture_cap, public.t_rfid_rfi where cap_id = rfi_cap_id and cap_ani_id = ani_id and rfi_tag_code ='", input$idRFID2,"'")
      resres = dbGetQuery(con,str)
      nAnimalFound <- resres[1,1]
      updateSelectizeInput(session, "nAnimal2", selected = nAnimalFound)
    }
  })
  
  #########          Vérification des tags  (metal ou non)                          ######### 
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_droit_metal from public.t_capture_cap, public.t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"'")
      resres = dbGetQuery(con,str)
      tag_droit_metal <- resres[1,1]
      updateCheckboxInput(session, "metal_tag_d2", value = tag_droit_metal)
    }
  })
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_gauche_metal from public.t_capture_cap, public.t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"'")
      resres = dbGetQuery(con,str)
      tag_gauche_metal <- resres[1,1]
      updateCheckboxInput(session, "metal_tag_g2", value = tag_gauche_metal)
    }
  })
  
  listTagD = dbGetQuery(con,"select distinct cap_tag_droit from public.t_capture_cap")
  names(listTagD)<-c("tag_droit")
  listTagG = dbGetQuery(con,"select distinct cap_tag_gauche from public.t_capture_cap")
  names(listTagG)<-c("tag_gauche")
  #listTag = cbind(listTagD,listTagG)
  
  reactive_tagG <- reactive({ 
    if (input$idTagOrG != "") {stock_tagG <- input$idTagOrG}
  })
  
  slow_tagG <- debounce(reactive_tagG, 1500)
  
  reactive_tagD <- reactive({ 
    if (input$idTagOrD !=""){stock_tagD <- input$idTagOrD}
  })
  
  slow_tagD <- debounce(reactive_tagD, 1500)
  
  output$tagDroitExiste <- renderUI({
    if (!is.null(slow_tagD())) {
      for (i in listTagD) {
        if (slow_tagD() %in% i)
        {shinyalert("TAG DROIT DEJA EXISTANT!", "Vérifier le numéro du tag", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
          updateTextInput(session, "idTagOrD", value = "")} 
      } 
    }  })
  
  output$tagGaucheExiste <- renderUI({
    if (!is.null(slow_tagG())) {
      for (i in listTagG) {
        if (slow_tagG() %in% i)
        {shinyalert("TAG GAUCHE DEJA EXISTANT!", "Vérifier le numéro du tag", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
          updateTextInput(session, "idTagOrG", value = "")} }
    }
  })
  
  reactive_tagG2 <- reactive({ 
    if (!is.null(input$idTagOrG3) && input$idTagOrG3 != "") {stock_tagG2 <- input$idTagOrG3}
  })
  
  slow_tagG2 <- debounce(reactive_tagG2, 1500)
  
  reactive_tagD2 <- reactive({ 
    if (!is.null(input$idTagOrD3) && input$idTagOrD3 !="")  {stock_tagD2 <- input$idTagOrD3}
  })
  
  slow_tagD2 <- debounce(reactive_tagD2, 1500)
  
  output$tagDroitExiste2 <- renderUI({
    if (!is.null(slow_tagD2())) {
      for (i in listTag) {
        if (slow_tagD2() %in% i)
        {shinyalert("TAG DROIT DEJA EXISTANT!", "Vérifier le numéro du tag", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
          updateTextInput(session, "idTagOrD3", value = "")} 
      } 
    }  })
  
  output$tagGaucheExiste2 <- renderUI({
    if (!is.null(slow_tagG2())) {
      for (i in listTag) {
        if (slow_tagG2() %in% i)
        {shinyalert("TAG GAUCHE DEJA EXISTANT!", "Vérifier le numéro du tag", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
          updateTextInput(session, "idTagOrG3", value = "")} }
    }
  })
  

  #########          ALERTES: nouvel ANIMAL existant+containtes code+sexe +creation ##########   
  
  listAnimal = dbGetQuery(con,"select distinct ani_etiq from public.t_animal_ani")
  
  reactive_nAnimal <- reactive({ 
    stock_nAnimal <- input$nAnimal
  })
  
  slow_nAnimal <- debounce(reactive_nAnimal, 1500)
  
  output$animalExiste <- renderUI({
    if (!is.null(slow_nAnimal())){
      for (i in listAnimal) {
        if (toupper(slow_nAnimal() %in% i))
        {shinyalert("ANIMAL DEJA EXISTANT!", "Décocher '1ere capture' ou choisir un autre 'ani_etiq'", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
          updateTextInput(session, "nAnimal", value = "")} }
    }
  })
  
  
  output$mauvais_identifiant <- renderUI({
    observeEvent(input$cre_ind,{
      wait(1)
    if (nchar(as.character(isolate(input$nAnimal))) < 4 || nchar(as.character(isolate(input$nAnimal))) > 5) {###as.character()
      {shinyalert("N° ANIMAL ERRONE!!", "le nom de l'animal doit avoir 4 caractères ou 5", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
        updateTextInput(session, "nAnimal", value = "")}}
  })
  })
  

 output$sexe_inexistant <- renderUI({
   if (!is.null(input$cre_ind) && input$cre_ind != 0 && input$nAnimal != ""){
    if (input$sexe == "") {
      {shinyalert("IMPOSSIBLE DE CREER L'ANIMAL !!", "il manque le sexe", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
        updateTextInput(session, "nAnimal", value = "")
      }
    }
                                                     }
                                      })
  
  output$animal_inexistant <- renderUI({
    if (!is.na(input$cirCou) && !input$create_ind && (input$estNouvelAnimal == 'oui' || input$estNouvelAnimal == 'non' && input$identifie == 'non') || !is.na(input$lPattArriere) && !input$create_ind && (input$estNouvelAnimal == 'oui' || input$estNouvelAnimal == 'non' && input$identifie == 'non') || input$idTagOrD != "" && !input$create_ind && (input$estNouvelAnimal == 'oui' || input$estNouvelAnimal == 'non' && input$identifie == 'non')  || input$idTagOrG != ""  && !input$create_ind && (input$estNouvelAnimal == 'oui' || input$estNouvelAnimal == 'non' && input$identifie == 'non') ) {
      {shinyalert("VOUS DEVEZ CREER L'ANIMAL AVANT !!", "Entrez un N° Animal et son Sexe puis appuyez sur le bouton <Créer ?>", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
        updateNumericInput (session, "cirCou", value = NA)
        updateNumericInput (session, "lPattArriere", value = NA)
        updateTextInput (session, "idTagOrD", value = "")
        updateTextInput (session, "idTagOrG", value = "")
      }
    }
  })
  

  output$checklist1_invalid <- renderUI({
     if (input$time != "" && !exists("test_checklist1_validated")){
       {shinyalert("ATTENTION", "CHECKLIST1 doit ête validé avant de rentrer des données ici", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
          updateTextInput(session, "time", value = "")
      }
    } 
  })
  #########          Test données: poids, num sabot , tour de cou, lg patte, bois   ######### 
  
  ### Poids
  
  output$poids_ani = renderText({input$pSabotPlein-input$pSabotVide})
  
  reactive_pSabotPlein <- reactive({ 
    stock_pSabotPlein <- input$pSabotPlein
  })
  
  slow_pSabotPlein <- debounce(reactive_pSabotPlein, 1500)
  
  reactive_pSabotVide <- reactive({ 
    stock_pSabotVide <- input$pSabotVide
  })
  
  slow_pSabotVide <- debounce(reactive_pSabotVide, 1500)
  
  output$alert_poids <- renderUI({
    if (!is.na(slow_pSabotPlein()) && !is.na(slow_pSabotVide())) {
      if ((slow_pSabotPlein()-slow_pSabotVide())>40) {
        shinyalert("STOP!", "Poids supérieur à 40kgs!", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
      }} })

  modalCallback_num_sabot <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "numSabot" , value = 0)}
    else (dbSendQuery(con,sprintf("INSERT INTO listes.tr_sabots_sab (sab_valeur) VALUES ('%s')", input$numSabot))) }
  
  output$out_sabot_plein <- renderUI({
    if (!is.na(input$pSabotPlein)) {
      if (input$pSabotPlein>65) {
        shinyalert("STOP!", " Poids Sabot plein elevé!", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_sabot_plein )
      }} })  
  
  modalCallback_sabot_plein <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "pSabotPlein" , value = NA)}}
  
  output$out_sabot_vide <- renderUI({
    if (!is.na(input$pSabotVide)) {
      if (input$pSabotVide>50) {
        shinyalert("STOP!", " Poids Sabot vide elevé!", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_sabot_vide )
      }} }) 
  
  modalCallback_sabot_vide <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "pSabotVide" , value = NA)}}

  ### Cou
  
  output$out_cirCou <- renderUI({
    if (!is.na(input$cirCou)) {
      if (input$cirCou > dbGetQuery(con,"select max(cap_circou) from public.t_capture_cap")) {
        shinyalert("STOP!", "Circonference élevée", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_circou)
      }}})
  
  modalCallback_circou <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "cirCou" , value = NA)}}
  
  ### Patte
  
  output$out_lPattArriere <- renderUI({
    if(!is.na(input$lPattArriere)) {
      if (input$lPattArriere > dbGetQuery(con,"select max(cap_lpa) from t_capture_cap")) {
        shinyalert("STOP!", "Longueur patte élevée", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_lg_patte)
      }}})
  
  modalCallback_lg_patte <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "lPattArriere" , value = NA)}}
  
  ### Bois
  
  output$out_lBoisGauche <- renderUI({
    if (!is.na(input$lBoisGauche)) {
      if (input$lBoisGauche > dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")) {
        shinyalert("STOP!", "Longueur bois gauche elevee", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
      }}})
  
  output$out_lBoisDroit <- renderUI({
    if (!is.na(input$lBoisDroit)) {
      if (input$lBoisDroit > dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")) {
        shinyalert("STOP!", "Longueur bois droit elevee", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
      }}})
  
  liste_etatbois = dbGetQuery(con,"select distinct etb_description from listes.tr_etat_bois_etb order by etb_description")
  
  observeEvent(input$etatBois, {
    for (i in liste_etatbois) {
      if (!(input$etatBois %in% i)) {
        if (input$etatBois != "")
        {shinyalert("WAIT!", "Est-ce un nouvel état de bois ?", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_new_etatbois)} 
      }}
  })
  
  modalCallback_new_etatbois <- function(value) {
    if (value == TRUE) {
      (dbSendQuery(con,sprintf("INSERT INTO listes.tr_etat_bois_etb (etb_description) VALUES ('%s')", input$etatBois))) }}
  
  #########          Récupération de l'heure                                        #########    

  observeEvent(input$to_current_time_caract, {
    gettime_caract_posix <<- Sys.time()
    gettime_caract= as.character(Sys.time())
    gettime_caract=strsplit(gettime_caract, " ")[[1]]
    gettime_caract=gettime_caract[2]
    
    #output$time_caract <- renderText({gettime})
    
    updateTextInput(session, "time_caract", value = gettime_caract)
  })
  
  #########          Récupération du site                                           #########    
  
  liste_date <- dbGetQuery(con,"select cap_date from t_capture_cap")
  
  observeEvent(input$nAnimal2, {
    for (i in liste_date) {
      if (input$date_caract %in% i) {
        str = paste0("select distinct sit_nom_court from public.tr_site_capture_sit, public.t_capture_cap where sit_id=cap_sit_id and cap_date = '", input$date_caract,"'")
        resres = dbGetQuery(con,str)
        same_date <- resres[1,1]
        updateSelectizeInput(session, "idSite2", selected = same_date)
      }
    }
  })
  
  observeEvent(input$nAnimal, {
    for (i in liste_date) {
      if (input$date_caract %in% i) {
        str = paste0("select distinct sit_nom_court from public.tr_site_capture_sit, public.t_capture_cap where sit_id=cap_sit_id and cap_date = '", input$date_caract,"'")
        resres = dbGetQuery(con,str)
        same_date <- resres[1,1]
        updateSelectizeInput(session, "idSite", selected = same_date)
      }
    }
  })
  
  #########          Alerte perte de poids                                          #########
  
  output$perte_poids <- renderUI({
    if ((input$nAnimal2) != "") {
      str = paste0("select cap_poids from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      verif_poids <- resres[1,1]
      if (is.null(verif_poids)) {verif_poids <- 0}
      if (!is.na(input$pSabotPlein) && !is.na(input$pSabotVide)) {
        if ((verif_poids - (input$pSabotPlein - input$pSabotVide)) > 1) 
        {shinyalert("PERDU PLUS D'UN KILO!", "L'animal a perdu du poids par rapport à la capture précédente", type = "warning", showCancelButton=F, showConfirmButton = T)}
      }}
  })
  
  #########          Panneau conditionnel                                           #########
  
  output$conditionalInput1 <- renderUI({
    if(input$newTagG){
      textInput("idTagOrG3", h4("New Tag Gauche"),value="")}
    else if (input$newTagG == F) {
      selectizeInput("idTagOrG2", h4("Tag Or.Gauche"), choices = choix[["idTagOrG2"]],options=list(create=TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
  })
  
  output$conditionalInput2 <- renderUI({
    if(input$newTagD){
      textInput("idTagOrD3", h4("New Tag Droit"),value="")}
    else if (input$newTagD == F) {
      selectizeInput("idTagOrD2", h4("Tag Or.Droite"), choices = choix[["idTagOrD2"]],options=list(create=TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
  })
  
  output$conditionalInput3 <- renderUI({
    if(input$newRFIDbox){
      selectizeInput("idRFID_new", h4("RFID_new"), choices = choix[["idRFID_new"]], options=list(onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
    else if (input$newRFIDbox == F) {selectInput("idRFID2", h4("RFID"), choices = choix[["idRFID2"]], selectize = FALSE, selected = NULL)}
  })
  
  output$conditionalInput4 <- renderUI({
    if(input$newTagG){
      checkboxInput("metal_tag_g3", "New Tag G. métal", value = FALSE ) }
    else if (input$newTagG == F) {checkboxInput("metal_tag_g2", "Tag G. métal", value = FALSE )}
  })
  
  output$conditionalInput5 <- renderUI({
    if(input$newTagD){
      checkboxInput("metal_tag_d3", "New Tag D. métal", value = FALSE )}
    else if (input$newTagD == F) {checkboxInput("metal_tag_d2", "Tag D. métal", value = FALSE )}
  })
  
  
  #########          Lecture RFID                                                   #########
  
  # observeEvent(input$rfid_read, {
  #   source(file = "/home/pi/Desktop/App_capture_ad/read_RFID.R")
  #   updateSelectizeInput(session, "idRFID2", selected = rfid)
  #   updateSelectizeInput(session, "idRFID", selected = rfid)
  # })
  
  onclick("rfid_read", {    source(file = "/home/pi/Desktop/App_capture_ad/read_RFID.R")
    if (input$estNouvelAnimal == 'non' && input$identifie == 'oui') {updateSelectizeInput(session, "idRFID2", selected = rfid)}
    if (input$estNouvelAnimal == 'oui' || (input$estNouvelAnimal == 'non' && input$identifie == 'non')) {updateSelectInput(session, "idRFID", selected = rfid)}
    if (input$newRFIDbox) {updateSelectInput(session, "idRFID_new", selected = rfid)}
     })
  #########          effacer mémoire RFID prend 40 secondes                         ##########
  
  observeEvent(input$rfid_clear, {
    source(file = "/home/pi/Desktop/App_capture_ad/clear_RFID.R")
    updateTextInput(session, "rfid_erase", value = resultat)
  })
  
  ##################           RUBRIQUE BLESSURES                                   #################
  
  blessure = data.frame()
  row.names(blessure) = NULL
  
  output$tableblessure = DT::renderDT(expr = blessure,server = F)
  
  sup_Ligne = observeEvent(input$sup_Bles, {
    if (!is.null(input$tableblessure_rows_selected)) {
      blessuredb <- blessure
      blessure <<- blessure[-as.numeric(input$tableblessure_rows_selected),]
      output$tableblessure = DT::renderDT(blessure,server = F)
      
      
      liste_trait = blessuredb[as.numeric(input$tableblessure_rows_selected),"Traitement"]
      liste_trait =  strsplit(as.character(liste_trait), split = "_")[[1]]
      
      if ((length(liste_trait))>1){
        for (j in 1:length(liste_trait)) {
          ble_loc = blessuredb[as.numeric(input$tableblessure_rows_selected),"Localisation"]
          ble_grav = blessuredb[as.numeric(input$tableblessure_rows_selected),"Gravite"]
          ble_trait = liste_trait[j]
          id_ble_loc <- dbGetQuery(con, paste0("SELECT bll_id from listes.tr_blessure_localisation_bll where bll_localisation = '",ble_loc,"'"))
          id_ble_grav <- dbGetQuery(con, paste0("SELECT blg_id from listes.tr_blessure_gravite_blg where blg_gravite = '",ble_grav,"' and blg_bll_id = '",id_ble_loc,"'"))
          id_ble_trait <- dbGetQuery(con, paste0("SELECT blt_id from listes.tr_blessure_traitement_blt where blt_traitement = '",ble_trait,"'"))[1,1]
          req<- paste0("DELETE FROM public.t_blessure_capture_blc WHERE  blc_cap_id = ",cap_id," AND blc_bll_id ='",id_ble_loc,"' AND blc_blg_id ='",id_ble_grav,"'AND blc_blt_id= '",id_ble_trait,"'")
          dbSendQuery(con, req)
        }}
      if ((length(liste_trait))==1){
        ble_loc = blessuredb[as.numeric(input$tableblessure_rows_selected),"Localisation"]
        ble_grav = blessuredb[as.numeric(input$tableblessure_rows_selected),"Gravite"]
        ble_trait = blessuredb[as.numeric(input$tableblessure_rows_selected),"Traitement"]
        id_ble_loc <- dbGetQuery(con, paste0("SELECT bll_id from listes.tr_blessure_localisation_bll where bll_localisation = '",ble_loc,"'"))
        id_ble_grav <- dbGetQuery(con, paste0("SELECT blg_id from listes.tr_blessure_gravite_blg where blg_gravite = '",ble_grav,"' and blg_bll_id = '",id_ble_loc,"'"))
        id_ble_trait <- dbGetQuery(con, paste0("SELECT blt_id from listes.tr_blessure_traitement_blt where blt_traitement = '",ble_trait,"'"))
        req<- paste0("DELETE FROM public.t_blessure_capture_blc WHERE  blc_cap_id = ",cap_id," AND blc_bll_id ='",id_ble_loc,"' AND blc_blg_id ='",id_ble_grav,"'AND blc_blt_id= '",id_ble_trait,"'")
        dbSendQuery(con, req)
      }
    }
  })
  
  observeEvent(input$ajout_Bles, {
    if ((length(input$traitement))>1)
    {
      list_ble = ""
      for (u in input$traitement) {
        list_ble = paste(u,list_ble,sep="_")
      }
      if (input$remarques_ble=="")
      {blessure <<- rbind(blessure,data.frame("Localisation" = c(input$locali), "Gravite" =c(input$grave), "Traitement" = c(list_ble), "Liste" = paste(c(input$locali),c(input$grave), c(list_ble), sep = "-")))} 
      else { 
        remarque_blessure<- input$remarques_ble
        remarque_blessure<-gsub("'","''",remarque_blessure)
        Encoding(remarque_blessure) <- "UTF-8"
        remarque_blessure <- iconv(remarque_blessure, "UTF-8", "UTF-8",sub=' ')
        blessure <<- rbind(blessure,data.frame("Localisation" = c(input$locali), "Gravite" =c(input$grave), "Traitement" = c(list_ble), "Liste" = paste(c(input$locali),c(input$grave), c(list_ble),remarque_blessure, sep = "-")))}
      updateSelectInput(session,"locali", selected = NULL)
      updateSelectInput(session,"traitement", selected = "rien")
      
      liste_trait = blessure[nrow(blessure),"Traitement"]
      liste_trait =  strsplit(as.character(liste_trait), split = "_")
      for (j in 1:length(liste_trait[[1]])) {
        ble_trait = liste_trait[[1]][j]
        id_ble_loc <- dbGetQuery(con, paste0("SELECT bll_id from listes.tr_blessure_localisation_bll where bll_localisation = '",input$locali,"'"))
        id_ble_grav <- dbGetQuery(con, paste0("SELECT blg_id from listes.tr_blessure_gravite_blg where blg_gravite = '",input$grave,"' and blg_bll_id = '",id_ble_loc,"'"))
        id_ble_trait <- dbGetQuery(con, paste0("SELECT blt_id from listes.tr_blessure_traitement_blt where blt_traitement = '",ble_trait,"'"))[1,1]
        remarque_blessure<- input$remarques_ble
        remarque_blessure<-gsub("'","''",remarque_blessure)
        Encoding(remarque_blessure) <- "UTF-8"
        remarque_blessure <- iconv(remarque_blessure, "UTF-8", "UTF-8",sub=' ')
        req<- paste0("INSERT INTO public.t_blessure_capture_blc (blc_cap_id, blc_bll_id, blc_blg_id, blc_blt_id, blc_remarque) values (",cap_id,", '",id_ble_loc,"', '",id_ble_grav,"', ",as.numeric(id_ble_trait),", '",remarque_blessure,"')")
        dbSendQuery(con, req)
      }
    }
    
    if ((length(input$traitement))==1)
    {
      if (input$remarques_ble=="")
      {blessure <<- rbind(blessure,data.frame("Localisation" = c(input$locali), "Gravite" =c(input$grave), "Traitement" = c(input$traitement), "Liste" = paste(c(input$locali),c(input$grave),c(input$traitement), sep = "-")))}
      else {
        remarque_blessure<- input$remarques_ble
        remarque_blessure<-gsub("'","''",remarque_blessure)
        Encoding(remarque_blessure) <- "UTF-8"
        remarque_blessure <- iconv(remarque_blessure, "UTF-8", "UTF-8",sub=' ')
        blessure <<- rbind(blessure,data.frame("Localisation" = c(input$locali), "Gravite" =c(input$grave), "Traitement" = c(input$traitement), "Liste" = paste(c(input$locali),c(input$grave), c(input$traitement), remarque_blessure, sep = "-")))}
      
      id_ble_loc <- dbGetQuery(con, paste0("SELECT bll_id from listes.tr_blessure_localisation_bll where bll_localisation = '",input$locali,"'"))
      id_ble_grav <- dbGetQuery(con, paste0("SELECT blg_id from listes.tr_blessure_gravite_blg where blg_gravite = '",input$grave,"' and blg_bll_id = '",id_ble_loc,"'"))
      id_ble_trait <- dbGetQuery(con, paste0("SELECT blt_id from listes.tr_blessure_traitement_blt where blt_traitement = '",input$traitement,"'"))
      remarque_blessure<- input$remarques_ble
      remarque_blessure<-gsub("'","''",remarque_blessure)
      Encoding(remarque_blessure) <- "UTF-8"
      remarque_blessure <- iconv(remarque_blessure, "UTF-8", "UTF-8",sub=' ')
      req<- paste0("INSERT INTO public.t_blessure_capture_blc (blc_cap_id, blc_bll_id, blc_blg_id, blc_blt_id, blc_remarque) values (",cap_id,", '",id_ble_loc,"', '",id_ble_grav,"', '",id_ble_trait,"', '",remarque_blessure,"')")
      dbSendQuery(con, req)
    }
    
    output$tableblessure = DT::renderDT(blessure,server = F)
    updateTextInput(session, "remarques_ble", value = "", placeholder = "Remarque")
    output$casc_ble1 <- renderUI({
      selectInput("locali", h4("Localisation"), choices = c(choisir="", unique(df_blessure$ble_local)), selectize = FALSE, selected = NULL)
    })
    
    output$casc_ble2 <- renderUI({
      x <- input$locali
      if (any(
        is.null(x)
      ))
        return("Select")
      choice2 <- df_blessure[df_blessure$ble_local == x,  "ble_gravite"]
       selectInput("grave", h4("gravité"), choices = choice2, selectize = FALSE, selected = NULL)
      
    })
    
    updateSelectInput(session,"traitement", choices = dbGetQuery(con,"select blt_traitement from listes.tr_blessure_traitement_blt "))
    
  })
  
  observeEvent(input$ajout_Bles, {
    i=1
    liste_blessures =""
    while (i <= nrow(blessure)) {
      liste_blessures <- paste0(liste_blessures, blessure[i,]$Liste, "~")
      i=i+1
      updateTextInput(session, "liste_blessures", value = liste_blessures)
      
    }
    
  })
  
  ### Mise en forme des blessures en cascade :
  
  output$casc_ble1 <- renderUI({
    selectInput("locali", h4("Localisation"), choices = c(choisir="", unique(df_blessure$ble_local)), selectize = FALSE, selected = NULL)
  })
  
  output$casc_ble2 <- renderUI({
    x <- input$locali
    if (any(
      is.null(x)
    ))
      return("Select")
    choice2 <- df_blessure[df_blessure$ble_local == x,  "ble_gravite"]
    selectInput("grave", h4("gravité"), choices = choice2, selectize = FALSE, selected = NULL)
    
  })
  
  updateSelectInput(session,"traitement", choices = dbGetQuery(con,"select blt_traitement from listes.tr_blessure_traitement_blt "))
  
  
  ##################           Ajout d'un nouveau traitement                        ########
  
  liste_traitement = dbGetQuery(con,"select blt_traitement from listes.tr_blessure_traitement_blt")
  
  observeEvent(input$traitement, {
    for (i in liste_traitement) {
      if (!(input$traitement %in% i)) {
        if (input$traitement != "")
        {shinyalert("WAIT!", "Est-ce un nouveau type de traitement ?", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_traitement)} 
      }}
  })
  
  modalCallback_traitement <- function(value) {
    if (value == TRUE) {
      (dbSendQuery(con,sprintf("INSERT INTO listes.tr_blessure_traitement_blt (blt_traitement) VALUES ('%s')", input$traitement))) }}
  
  ##################           RUBRIQUE PRELEVEMENTS                                #################
  
  prelevement = data.frame()
  row.names(prelevement) = NULL
  
  output$tableprelevement = DT::renderDT(expr = prelevement,server = F)
  
  sup_Ligne_prelev = observeEvent(input$sup_prelev, {
    if (!is.null(input$tableprelevement_rows_selected)) {
      prelevement <<- prelevement[-as.numeric(input$tableprelevement_rows_selected),]
      output$tableprelevement = DT::renderDT(prelevement,server = F)
      id_prel_type = dbGetQuery(con, paste0("SELECT sat_id from listes.tr_samples_types_sat where sat_type = '",input$typetype,"'"))
      id_prel_loc = dbGetQuery(con, paste0("SELECT sal_id from listes.tr_samples_localisation_sal where sal_localisation = '",input$localoca,"' AND sal_sat_id = '",id_prel_type,"'"))
      id_prel_cont = dbGetQuery(con, paste0("SELECT sac_id from listes.tr_samples_contenant_sac where sac_contenant = '",input$condi,"' AND sac_sat_id = '",id_prel_type,"'"))
      id_prel_solv = dbGetQuery(con, paste0("SELECT sas_id from listes.tr_samples_solvant_sas where sas_solvant = '",input$solsol,"' AND sas_sac_id = '",id_prel_cont,"' "))
      req = paste0("DELETE FROM public.t_sample_capture_sca WHERE sca_cap_id ='",cap_id,"' AND sca_sat_id ='",id_prel_type,"'AND sca_sal_id = '",id_prel_loc,"'AND sca_sac_id ='",id_prel_cont,"'AND sca_sas_id = '",id_prel_solv,"' AND sca_value='",input$nbre_echant,"'AND sca_remarque ='",input$remarques_prel,"'")
      dbSendQuery(con, req)
    }
  })
  
  observeEvent(input$ajout_prelev, {
    wait (0.5)
    remarque_prelevement<- input$remarques_prel
    remarque_prelevement<-gsub("'","''",remarque_prelevement)
    Encoding(remarque_prelevement) <- "UTF-8"
    remarque_prelevement <- iconv(remarque_prelevement, "UTF-8", "UTF-8",sub=' ')
    prelevement <<- rbind(prelevement, data.frame("Type" = c(input$typetype), "Localisation" =c(input$localoca), "Contenant" = c(input$condi),"Solvant" = c(input$solsol),"Nombre d'echantillons" = c(input$nbre_echant),  "Remarques" = c(remarque_prelevement)))
    output$tableprelevement = DT::renderDT(prelevement,server = F)
    updateSelectInput(session,"typetype",selected=NULL)
    updateSelectInput(session,"solsol", selected=NULL)
    updateTextInput(session, "remarques_prel", value = "", placeholder = "Remarque")
     updateSelectInput(session,"nbre_echant", choices = choix[["nbre_echant"]], selected = as.character(1))
    
    id_prel_type = dbGetQuery(con, paste0("SELECT sat_id from listes.tr_samples_types_sat where sat_type = '",input$typetype,"'"))
    id_prel_loc = dbGetQuery(con, paste0("SELECT sal_id from listes.tr_samples_localisation_sal where sal_localisation = '",input$localoca,"' AND sal_sat_id = '",id_prel_type,"'"))
    id_prel_cont = dbGetQuery(con, paste0("SELECT sac_id from listes.tr_samples_contenant_sac where sac_contenant = '",input$condi,"' AND sac_sat_id = '",id_prel_type,"'"))
    id_prel_solv = dbGetQuery(con, paste0("SELECT sas_id from listes.tr_samples_solvant_sas where sas_solvant = '",input$solsol,"' AND sas_sac_id = '",id_prel_cont,"'"))
    req = paste0("INSERT INTO public.t_sample_capture_sca (sca_cap_id, sca_sat_id, sca_sal_id, sca_sac_id, sca_sas_id, sca_value, sca_remarque) values (",cap_id,", '",id_prel_type,"', '",id_prel_loc,"', '",id_prel_cont,"', '",id_prel_solv,"', '",input$nbre_echant,"', '",remarque_prelevement,"')")
    dbSendQuery(con, req)
  })
  
  ### Mise en forme des prélevements en cascade :
  
  output$table_prel <- renderTable({df_prelevement})
  
  output$control1 <- renderUI({
    selectInput("typetype", h4("Type"), choices = unique(df_prelevement$prel_type),selectize =FALSE)
  })
  
  output$control2 <- renderUI({
    x <- input$typetype
    if (any(
      is.null(x)
    ))
      return("Select")
    choice2 <- unique(df_prelevement[df_prelevement$prel_type == x,  "prel_local"])
    selectInput("localoca", h4("Localisation"), choices = (choice2),selectize =FALSE)
  })
  

  output$control3 <- renderUI({
    x <- input$typetype
    y <- input$localoca
    if (any(
      is.null(x),
      is.null(y)
    ))
      return("Select")
    
    choice3 <- unique(df_prelevement[df_prelevement$prel_type == x & df_prelevement$prel_local == y, "prel_condi"])
    selectInput("condi", h4("Conditionnement"), choices = choice3, selectize =FALSE)
    
  })
  
  output$control4 <- renderUI({
    x <- input$typetype
    y <- input$localoca
    z <- input$condi
    if (any(
      is.null(x),
      is.null(y),
      is.null(z)
    ))
      return("Select")
    
    choice4 <- unique(df_prelevement[df_prelevement$prel_type == x & df_prelevement$prel_local == y & df_prelevement$prel_condi == z, "prel_solv"])
    selectInput("solsol", h4("Solvant"), choices = choice4, selectize =FALSE)
  })
  
  #####selection des valeurs par defaut pour un type de prelevement
  #####en fonction du type
  observeEvent(input$typetype, {
    if (input$typetype == "sang") {
      updateSelectInput(session,"localoca", selected="jugulaire")}
    if (input$typetype == "feces") {
      updateSelectInput(session,"localoca", selected="anus")}
    if (input$typetype == "poils") {
      updateSelectInput(session,"localoca", selected="coup")}
    if (input$typetype == "mucus") {
      updateSelectInput(session,"localoca", selected="conduit auditif"); updateSelectInput(session,"nbre_echant", selected= as.character(2))}
    if (input$typetype == "peau") {
      updateSelectInput(session,"localoca", selected="oreille")}
    if (input$typetype == "tiques") {
      updateSelectInput(session,"localoca", selected="corps")}
  })
  #####en fonction de la localisation
  observeEvent(input$localoca, {
    if (input$localoca == "anus") {
      updateSelectInput(session,"condi", selected="pilulier 25 ml")}
    if (input$localoca == "coup") {
      updateSelectInput(session,"condi", selected="papier aluminium")}
    })

  
  liste_prel_db = dbGetQuery(con,"select sav_intitule from listes.tr_samples_verification_sav")
  
  ##################           RUBRIQUE COLLIER                                     #################
  
  query <- eventReactive((input$nAnimal != "" || input$nAnimal2 != ""),{
    date_mod = input$date_caract
    date_mod = format(date_mod, "%d/%m/%Y")
    date_mod = as.character(date_mod)
    
    mois = strsplit(date_mod, "/")[[1]][2]
    annee = strsplit(date_mod, "/")[[1]][3]
    if (as.integer(mois)>=10) {annee_suivie <- as.integer(annee) + 1}
    if (as.integer(mois)<10) {annee_suivie <- annee}
    
    liste_collier <- dbGetQuery(con,paste0("select eqc_annee_suivi, teq_nom_court, eqc_remarque, eqt_id_usuel,eqc_drop_off,sen_association, eqc_couleur_collier, eqc_couleur_boitier,eqt_frequence, eqc_memoire FROM public.t_equipement_eqt, public.t_equipement_conf_eqc, public.tr_type_equipement_teq,listes.tr_sensors_sen  where eqc_eqt_id = eqt_id
                                           and teq_id = eqt_teq_id and eqc_sen_id=sen_id and eqc_annee_suivi = '",annee_suivie,"' and eqc_pose = FALSE order by teq_nom_court")) 
    
    return(liste_collier)})
  
  output$tablecollier = DT::renderDataTable(expr = query() , selection = 'single')
  
  affichage_choix_collier <- observeEvent(input$tablecollier_rows_selected, {
    if (!is.null(input$tablecollier_rows_selected)) {
      ligne_selection = input$tablecollier_rows_selected
      collier_tech = query()[ligne_selection,"teq_nom_court"]
      collier_col_c = query()[ligne_selection,"eqc_couleur_collier"]
      collier_col_b = query()[ligne_selection,"eqc_couleur_boitier"]
      cat_col = paste(toupper(collier_tech),": collier ", toupper(collier_col_c)," boitier ", toupper(collier_col_b) )
      output$collier_choisi = renderText(cat_col)
      
      
      find_eqt_id <<- dbGetQuery(con, paste0("select eqt_id from public.t_equipement_eqt where eqt_id_usuel = '",query()[ligne_selection,"eqt_id_usuel"],"'"))[1,1]
      find_pb_collier <<- dbGetQuery(con, paste0("select eqc_remarque from public.t_equipement_conf_eqc where eqc_eqt_id = '",find_eqt_id,"'"))[1,1]
      find_association <<- dbGetQuery(con, paste0("select sen_association from public.t_equipement_conf_eqc, listes.tr_sensors_sen where eqc_sen_id =sen_id and eqc_eqt_id = '",find_eqt_id,"'"))[1,1]
      if (length(grep("activite",find_association) != 0)) {act<- TRUE} else {act<- FALSE}
      if (length(grep("proximite",find_association) != 0)) {prox<- TRUE} else {prox<- FALSE}
      
      if (input$remarque_collier != ""){
        remarque_collier<- input$remarque_collier
        remarque_collier<-gsub("'","''",remarque_collier)
        Encoding(remarque_collier) <- "UTF-8"
        remarque_collier <- iconv(remarque_collier, "UTF-8", "UTF-8",sub=' ')
        if (is.na(find_pb_collier)){remarque_collier<- remarque_collier} else {remarque_collier<-paste0(find_pb_collier,"~", remarque_collier)}
      } else {if (is.na(find_pb_collier)) {remarque_collier<- NULL} else {remarque_collier<-find_pb_collier}}
      
      annee_suivie <-  query()[ligne_selection,"eqc_annee_suivi"]
      req<- paste0("INSERT INTO public.tj_equipement_animal_eqt_ani_eqa (eqa_ani_id, eqa_eqt_id, eqa_date_debut, eqa_date_fin_arrondi, eqa_probleme, eqa_activite, eqa_annee_suivi) VALUES
                 ('",find_ani_id,"', '",find_eqt_id,"', '",as.character(input$date_caract),"', FALSE ,'",remarque_collier,"','",act,"','",annee_suivie,"')
                 ON CONFLICT (eqa_ani_id, eqa_date_debut) DO UPDATE SET (eqa_eqt_id, eqa_activite, eqa_probleme) = (excluded.eqa_eqt_id, excluded.eqa_activite, excluded.eqa_probleme)")
      req<- gsub("'NA'","NULL", req)
      req<- gsub("''","NULL", req)
      dbSendQuery(con, req)
      
      req2<- paste0("UPDATE public.t_capture_cap set cap_proximite_contact = '",prox,"' where cap_id= ",cap_id," ")
      
      req2 = gsub("'NA'","NULL", req2)
      req2 = gsub("''","NULL", req2)
      
      dbSendQuery(con, req2)
      
    }
  })
  
  observeEvent(input$sup_col, {
    req<- paste0("DELETE FROM public.tj_equipement_animal_eqt_ani_eqa WHERE eqa_ani_id='",find_ani_id,"' and eqa_eqt_id='",find_eqt_id,"' and eqa_date_debut='",as.character(input$date_caract),"'")
    req<- gsub("'NA'","NULL", req)
    req<- gsub("''","NULL", req)
    dbSendQuery(con, req)
    proxy <- dataTableProxy("tablecollier",session, deferUntilFlush = FALSE)
    reloadData(proxy, resetPaging = TRUE, clearSelection = c("all"))
    ####remise a jour de la ligne texte de selection du collier
    output$collier_choisi = renderText("")
    input<-reactiveValues(tablecollier_rows_selected= NULL)
    input$tablecollier_rows_selected<-NULL
  })
  
  
  
  ##################           RUBRIQUE TABLE                                       #################
  
  updateSelectInput(session, "Notation_euro_table", choices = choix[["Notation_euro_table"]])
  
  observeEvent(input$to_current_time_table, {
    
    gettime_table_posix <<- Sys.time()
    gettime_table = as.character(Sys.time())
    gettime_table = strsplit(gettime_table, " ")[[1]]
    gettime_table = gettime_table[2]
    updateTextInput(session, "time_table", value = gettime_table)
  })
  
  observeEvent(input$criautre, {
    if (!is.null(input$criautre) && !is.null(input$cribague)) {
      if (((input$cribague == "NA" || input$cribague == "0")) && (input$criautre == "0")) {
        cri_synthese = FALSE }
      else {cri_synthese = TRUE }
    } })
  
  observeEvent(input$cribague, {
    if (!is.null(input$criautre) && !is.null(input$cribague)) {
      if (((input$cribague == "NA" || input$cribague == "0")) && (input$criautre == "0")) {
        cri_synthese = FALSE }
      else {cri_synthese = TRUE }
    } })
  
  ############ variables associées à une heure
  

  mes_heure = data.frame()
  row.names(mes_heure) = NULL
  
  output$table_mes_heure = DT::renderDT(expr = mes_heure,server = F)
  
  sup_Ligne_mes = observeEvent(input$sup_mes_heure, {
    if (!is.null(input$table_mes_heure_rows_selected)) {
      selected <-mes_heure[as.numeric(input$table_mes_heure_rows_selected),]
      mes_heure <<- mes_heure[-as.numeric(input$table_mes_heure_rows_selected),]
      output$table_mes_heure = DT::renderDT(mes_heure,server = F)
      var_id = dbGetQuery(con, paste0("SELECT var_id from tr_variable_mesuree_var where var_nom_court = '",selected[,"Mesure"],"'"))
      req = paste0("delete from tj_mesureenum_heure_capture_nhca where nhca_cap_id ='",cap_id,"' AND nhca_var_id ='",var_id,"'AND nhca_heure_locale_cest = '",selected[,"Heure_mesure"],"'")
      dbSendQuery(con, req)
    }
  })
  
  observeEvent(input$ajout_mes_heure, {
    wait (0.5)
    rem_mes_heure<- input$rem_mes_heure
    rem_mes_heure<-gsub("'","''",rem_mes_heure)
    Encoding(rem_mes_heure) <- "UTF-8"
    rem_mes_heure <- iconv(rem_mes_heure, "UTF-8", "UTF-8",sub=' ')
    heure_mesure = substring(Sys.time(),12,20)
    mes_heure <<- rbind(mes_heure, data.frame("Mesure" = c(input$mes), "Heure_mesure" = c(heure_mesure), "Valeur" = input$Val_mes,"Remarque" = c(rem_mes_heure)))
    output$table_mes_heure = DT::renderDT(mes_heure,server = F)
    updateSelectInput(session,"mes",selected= "temperature anale_ysi")
    updateNumericInput(session,"Val_mes",value = "")
    updateTextInput(session, "rem_mes_heure", value = "", placeholder = "Remarque")
    var_id <- dbGetQuery(con, paste0("SELECT var_id from tr_variable_mesuree_var where var_nom_court = '",input$mes,"'"))
    req <- paste0("INSERT INTO tj_mesureenum_heure_capture_nhca  (nhca_cap_id, nhca_var_id, nhca_heure_locale_cest, nhca_valeur, nhca_remarque) values (",cap_id,", '",var_id,"', '",heure_mesure ,"', '",input$Val_mes,"', '",rem_mes_heure,"')")
    dbSendQuery(con, req)
  })

  ############ variables booleenes
  
  
  bool = data.frame()
  row.names(bool) = NULL
  
  output$table_bool = DT::renderDT(expr = bool,server = F)
  
  sup_Ligne_mes = observeEvent(input$sup_bool, {
    if (!is.null(input$table_bool_rows_selected)) {
      selected <-bool[as.numeric(input$table_bool_rows_selected),]
      bool <<- bool[-as.numeric(input$table_bool_rows_selected),]
      output$table_bool = DT::renderDT(bool,server = F)
      var_id = dbGetQuery(con, paste0("SELECT var_id from tr_variable_mesuree_var where var_nom_court = '",selected[,"Mesure"],"'"))
      req = paste0("delete from public.tj_mesureebool_capture_bca where bca_cap_id =",cap_id," AND bca_var_id =",var_id," ")
      dbSendQuery(con, req)
    }
  })
  
  
  observeEvent(input$ajout_bool, {
    wait (0.5)
    rem_bool<- input$rem_bool
    rem_bool<-gsub("'","''",rem_bool)
    Encoding(rem_bool) <- "UTF-8"
    rem_bool <- iconv(rem_bool, "UTF-8", "UTF-8",sub=' ')
    bool <<- rbind(bool, data.frame("Mesure" = c(input$bool), "Valeur" = TRUE,"Remarque" = c(rem_bool)))
    #print(bool)
    output$table_bool = DT::renderDT(bool,server = F)
    updateSelectInput(session,"bool",selected= "myophos")
    #updateNumericInput(session,"Val_bool",value = "")
    updateTextInput(session, "rem_bool", value = "", placeholder = "Remarque")
    var_id <- dbGetQuery(con, paste0("SELECT var_id from tr_variable_mesuree_var where var_nom_court = '",input$bool,"'"))
    req <- paste0("INSERT INTO public.tj_mesureebool_capture_bca  (bca_cap_id, bca_var_id, bca_valeur, bca_remarque) values (",cap_id,", '",var_id,"', 'TRUE', '",rem_bool,"') ON CONFLICT (bca_cap_id, bca_var_id) DO UPDATE SET (bca_valeur, bca_remarque) = (excluded.bca_valeur, excluded.bca_remarque)")
    dbSendQuery(con, req)
  })
  
  ##################           RUBRIQUE HISTORIQUE                                  #################
  
  output$historique <- DT::renderDataTable({
    
    outp <- dbGetQuery(con,paste0("select * from historique.t_historique where ani = '",input$nAnimal2,"'"))
    ret <- DT::datatable(outp)
    return(ret)
  })

  ##################           RUBRIQUE CHECKLIST 1                                 #################
  ##################           Animal                                               #########
  
  checklist1 = data.frame()
  row.names(checklist1) = NULL
  output$tablechecklist1 = DT::renderDT(expr = checklist1,server = F)
  
  output$checklist_1 <- renderUI( { 
    
    checklist1 = data.frame()

    if ((input$numSabot)=="") {
      checklist1 = data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Numéro de Sabot"))}
    
    if ((input$nAnimal =="") & input$estNouvelAnimal == 'oui') {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Numéro de l'animal")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$nAnimal2=="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Numéro de l'animal")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'non')  & (input$nAnimal=="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Numéro de l'animal")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$idSite2 =="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nom du site")))}
    
    if ((input$estNouvelAnimal == 'non')  & (input$identifie == 'non') & (input$idSite =="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nom du site")))}
    
    if ((input$estNouvelAnimal == 'oui') & (input$idSite=="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nom du site")))}
    
    if (input$age =="") {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Age")))}

    if (is.na(input$pSabotPlein-input$pSabotVide)) {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Poids de l'animal")))}
    
    if (input$time_caract =="") {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Heure debut")))}

    if ((input$identifie == 'non' && input$idRFID=="") || (input$identifie == 'non' && is.null(input$idRFID))) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("RFID")))}

    if ((input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$idRFID2 == "" && input$newRFIDbox == F) | ( input$estNouvelAnimal == 'non' && input$identifie == 'oui' && is.null(input$idRFID2) && input$newRFIDbox == F)) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("RFID")))}
    
    if ((input$estNouvelAnimal == 'non') && (input$identifie == 'oui') && (input$newRFIDbox == T) && (input$idRFID_new == "")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nouveau RFID")))}
    
    if ((input$estNouvelAnimal == 'oui') & (input$idTagOrG)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Gauche")))}
    
    if ((input$estNouvelAnimal == 'oui') & (input$idTagOrD)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'non') & (input$idTagOrD)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'non') & (input$idTagOrG)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Gauche")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$newTagD == F) & (input$idTagOrD2)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$newTagG == F) & (input$idTagOrG2)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Gauche")))}
    
    if ((input$estNouvelAnimal == 'non') && (input$identifie == 'oui') && (input$newTagD == T) && (input$idTagOrD3=="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nouveau Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') && (input$identifie == 'oui') && (input$newTagG == T) && (input$idTagOrG3)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nouveau Tag Gauche")))}
    
    if (is.na(input$lPattArriere)) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Longueur patte")))}
    
    if (is.null(input$sexe)) {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Sexe")))}
    
    if (is.na(input$lBoisGauche) & !is.null(input$sexe)) {
      if (input$sexe=='M') {
        checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Longueur bois G")))}}
    
    if (is.na(input$lBoisDroit) & !is.null(input$sexe)) {
      if (input$sexe=='M') {
        checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Longueur bois D")))}}
    
    if (((input$etatBois)=="") & !is.null(input$sexe)){
      if (input$sexe=='M') {
        checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Etat bois")))}}
    
    # if (is.na(input$tglucose)) {
    #   checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Glucose")))}
    
    if (is.na(input$cirCou)) {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Cou")))}
    
    if (input$diarrhee =="") {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Diarrhee")))}
    
    if (input$tiques =="") {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nombre de tiques")))}
    
    if ((input$lactation=="") & !is.null(input$sexe)){
      if (input$sexe=='F') {
        checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Lactation")))}}
    
    if (nrow(checklist1)==0) {
      checklist1 =  rbind(checklist1,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
    
    output$tablechecklist1 = DT::renderDT(checklist1,server = F, options = list(pageLength = 15)) 
    
    
  #########          Table                                                          ########                
  
  checklist_table = data.frame()
  
  if (input$lutte == "NA" || input$lutte == "") {
    checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Lutte")))}
  
  if (input$halete == "NA" || input$halete == "") {
    checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Halete")))}
  
  if (input$cribague == "NA" || input$cribague == "") {
    checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Cri bague")))}
  
  if (input$criautre == "NA" || input$criautre == "") {
    checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Cri autre")))}
  
  if ((input$Notation_euro_table)=="") {
    checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Eurodeer")))}
  
  if (input$time_table =="") {
    checklist_table =  rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Heure fin")))}
  
  if (nrow(checklist_table)==0) {
    checklist_table =  rbind(checklist_table,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
  
  output$tablechecklist_table = DT::renderDT(checklist_table,server = F)

  
  #########          Prelevement                                                    ########
  
  checklist_prel = data.frame()
  
  if (input$tab == "Checklist 1"){
    if (nrow (prelevement) != 0)  {
      ref<- apply(liste_prel_db,2,as.character)
      prel<-prelevement[,c("Type","Localisation","Contenant","Solvant")]
      el<- NA
      for (i in 1:nrow(prel)){ 
        el<- append(el,paste0(apply(prel[i,],2,as.character),collapse = "_"))
      }
      el<-na.omit(el)
      prel<-na.omit(match(el,ref))
      if (!is.na(prel[1])) {checklist_prel <- data.frame("PRELEVEMENT_MANQUANT"= c(ref[-prel,]))} else {ref<- apply(liste_prel_db,2,as.character); checklist_prel <- data.frame("PRELEVEMENT_MANQUANT"= c(ref))}
      
      if (nrow(checklist_prel)==0) {
        checklist_prel <-  rbind(checklist_prel,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
      
    } else {ref<- apply(liste_prel_db,2,as.character); checklist_prel <- data.frame("PRELEVEMENT_MANQUANT"= c(ref))}
    
    
    output$tablechecklist_prel = DT::renderDT(checklist_prel)}
  
  #########          Collier                                                        ########
  
  checklist_collier = data.frame()
  
  if (is.null(input$tablecollier_rows_selected)) {
    checklist_collier = data.frame("COLLIER_MANQUANT"= c("ATTENTION PAS DE COLLIER CHOISI"))}
  
  if (!is.null(input$tablecollier_rows_selected)) {
    checklist_collier = data.frame("PARFAIT"= c("OK COLLIER BIEN SELECTIONNE"))}
  
  output$tablechecklist_collier = DT::renderDT(checklist_collier,server = F)
  

  #########          Bilan checklist 1                                              ########
  
  # print(input$valid_checklist1)
  # observeEvent(input$valid_checklist1, {
  # 
  #   if ( ((checklist_prel[1,1])!="PAS DE DONNEES MANQUANTES") || ((checklist_table[1,1])!="PAS DE DONNEES MANQUANTES") || ((checklist1[1,1])!="PAS DE DONNEES MANQUANTES") || ((checklist_collier[1,1])!="Collier bien sélectionné"))
  #   {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE,  callbackR = modalCallback_check1 )}
  #   else
  #   {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check1 )}
  # 
  # }, once = T) #, ignoreInit = T
  
  
  onclick("valid_checklist1", {
    
    if ( ((checklist_prel[1,1])!="PAS DE DONNEES MANQUANTES") || ((checklist_table[1,1])!="PAS DE DONNEES MANQUANTES") || ((checklist1[1,1])!="PAS DE DONNEES MANQUANTES") || ((checklist_collier[1,1])!="Collier bien sélectionné"))
    {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE,  callbackR = modalCallback_check1 )}
    else
    {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check1 )}
    
  })
  
  })
  ##################           RUBRIQUE LACHER                                      #################
  
  updateSelectizeInput(session, "habitat", choices = choix[["habitat"]])
  updateSelectizeInput(session, "habitat_perte", choices = choix[["habitat_perte"]])
  updateSelectizeInput(session, "Notation_euro", choices = choix[["Notation_euro"]])
  
  
  observeEvent(input$to_current_time, {
    gettime_time_posix <<- Sys.time()
    gettime_time <- as.character(Sys.time())
    gettime_time <- strsplit(gettime_time, " ")[[1]]
    gettime_time <- gettime_time[2]
    updateTextInput(session, "time", value = gettime_time)
  })
  
  
  observeEvent(input$to_current_time2, {
    gettime_time2_posix <<- Sys.time()
    gettime_time2= as.character(Sys.time())
    gettime_time2=strsplit(gettime_time2, " ")[[1]]
    gettime_time2<- gettime_time2[2]
    updateTextInput(session, "time2", value = gettime_time2)
  })
  
  observeEvent(input$save_checklist2, { 
    tmp_time= as.character(input$time)
    tmp_time=strsplit(tmp_time, " ")[[1]]
    tmp_time=tmp_time[2]
  }
  )
  
  ##################           RUBRIQUE CHECKLIST 2                                 #################
  checklist2 = data.frame()
  row.names(checklist2) = NULL
  output$tablechecklist2 = DT::renderDT(expr = checklist2,server = F)
  
  
  output$checklist_2 <- renderUI( {
    checklist2 = data.frame()
    
    if (input$vitesse == "NA"|| input$vitesse == "")  {
      checklist2 = data.frame("DONNNES_LACHER_MANQUANTES" = c("Vitesse"))}
    
    if (input$titube == "NA" || input$titube == "") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Titube")))}
    
    if (input$couche == "NA" || input$couche == "") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Couche")))}
    
    if (input$cabriole_saut == "NA" || input$cabriole_saut == "") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Cabriole")))}
    
    if (input$cri == "NA" || input$cri == "") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Cri")))}
    
    if (input$allure == "NA" || input$allure == "") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Allure")))}
    
    if (input$gratte_collier == "NA" || input$gratte_collier == "") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Gratte-collier")))}
    
    if (input$tombe == "NA" || input$tombe == "") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Tombe")))}
    
    if ((input$habitat)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Habitat")))}
    
    if ((input$Notation_euro)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Eurodeer")))}
    
    if ((input$habitat_perte)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Habitat perte")))}
    
    if (is.na(input$nbre_stops)) {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Nombre de stops")))}
    
    if ((input$visibilite)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Visibilité")))}
    
    if ((input$nbre_personnes)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Nombre de personnes")))}
    
    if ((input$time)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Heure de lacher")))}
    
    if (nrow(checklist2)==0) {
      checklist2 =  data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES"))}
    
  
    output$tablechecklist2 = DT::renderDT(checklist2,server = F) 
    
    ### Bilan
    
    # observeEvent(input$valid_checklist2, {
    #   if  ((checklist2[1,1])!="PAS DE DONNEES MANQUANTES")
    #   {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE, callbackR  = modalCallback_check2)}
    #   else      
    #   {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check2)}
    # },ignoreInit = T)
    
    onclick("valid_checklist2", {
      
      if (checklist2[1,1]!="PAS DE DONNEES MANQUANTES")
      {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE,  callbackR = modalCallback_check2 )}
      else
      {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check2 )}
      
    })
    
  })
  
  ##################           RUBRIQUE DONNEES                                     ########################
  
  setwd(tosd)

 observeEvent(input$to_current_time, {
   fichier_lu <- read.table(file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), sep = ";", na.strings = "", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character"))
  
  
   noms_colonnes<- c("N°Animal","ani_nom","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_pertinent","cap_lactation","RFID","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit","glucose","T°C_ext","TIQUES FIXES","autres parasites", "Peau","poils","sang","feces","tiques","vaginal","Nasal","remarque","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)","num_sabot","couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2","ventilation","remarque_table","localisation sonde temperature","eurodeer","titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque_generale","bague","autre","stop","habitat lacher","habitat perte vue","visibilite","nb_public","eurodeer_lacher","remise sabot","hre_lacher_2")
   colnames(fichier_lu)<- noms_colonnes
  
  output$csv <-DT::renderDataTable({
    c<-DT::datatable(as.data.frame(fichier_lu))
    return(c)})
    })
 
 observeEvent(input$shutdown, system("sudo shutdown -h now"))
 
  ##################           RUBRIQUE CAPTURE                                     #################
  
  ############faire apparaitre les rubriques cachées
  observe({
    toggle(condition = input$appear, selector = "#tab li a[data-value=Capture]")
    toggle(condition = input$appear, selector = "#tab li a[data-value=Sabot]")
    toggle(condition = input$appear, selector = "#tab li a[data-value=Checklist3]")
  }) 

  ####affichage des dates disponibles
 observe({
   if ((input$date_capture)=="") {
     setwd(tosd)
     fi<-grep(".csv",list.files(), value =TRUE)
     fi<-sub("captures_","",fi)
     fi<-c("choisir",sub(".csv","",fi))
     fi<<-gsub("_","-",fi)[-grep("choisir",gsub("_","-",fi))]
     #print(fi)
     updateSelectInput(session, "date_capture", choices = fi)
   }
 })
 
 ####affichage des données de l'individu
 observe({
   if (input$date_capture !="choisir" && input$date_capture!="") {
     fichier_lu <- read.table(file = paste0("captures_",gsub("-","_",input$date_capture), ".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character"))
      #noms_colonnes<- c("N°Animal","ani_nom","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_pertinent","cap_lactation","RFID","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit","glucose","T°C_ext","TIQUES FIXES","autres parasites", "Peau","poils","sang","feces","tiques","vaginal","Nasal","remarque","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)","num_sabot","couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2","remarque_table","localisation sonde temperature","eurodeer","titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque_generale","bague","autre","stop","habitat lacher","habitat perte vue","visibilite","nb_public","eurodeer_lacher","remise sabot","hre_lacher_2")
     noms_colonnes<- c("N°Animal","ani_nom","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_pertinent","cap_lactation","RFID","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit","glucose","T°C_ext","TIQUES FIXES","autres parasites", "Peau","poils","sang","feces","tiques","vaginal","Nasal","remarque","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)","num_sabot","couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2","ventilation","remarque_table","localisation sonde temperature","eurodeer","titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque_generale","bague","autre","stop","habitat lacher","habitat perte vue","visibilite","nb_public","eurodeer_lacher","remise sabot","hre_lacher_2")
     colnames(fichier_lu)<- noms_colonnes
     if (input$numSabot_capture == "") {
     updateSelectInput(session, "numSabot_capture", choices = c(choisir = "",unique(fichier_lu[,c("num_sabot")])))}
      print(input$numSabot_capture)
      if(length(input$numSabot_capture) != 0) {
           if (!is.null(input$numSabot_capture)){
                  if (input$numSabot_capture!="") {
        select_line <<- which(fichier_lu[,c("num_sabot")]==c(input$numSabot_capture),arr.ind=TRUE)[1]
        ani <- fichier_lu[select_line, c("N°Animal")]
        ani2<- ani
        sexe<- fichier_lu[select_line, c("Sexe")]
        
        sabot<- fichier_lu[select_line, c("num_sabot")]
        age<- fichier_lu[select_line, c("Age cahier")]
        tagd<- fichier_lu[select_line, c("cap_tag_droit")]
        tagg<- fichier_lu[select_line, c("cap_tag_gauche")]
        poids<-fichier_lu[select_line, c("Poids")]
        sante<-fichier_lu[select_line, c("etat_sante")]
        site<-fichier_lu[select_line, c("Site Capture")]
        anim<-dbGetQuery(con, "Select ani_etiq from t_animal_ani")
        if (length(grep(ani2,as.character(anim[,1]))) != 0){
          updateRadioButtons(session,"estNouvelAnimal", selected = "non")
          updateSelectizeInput(session, "idSite2",  selected = site)
          updateSelectizeInput(session, "nAnimal2",  selected = ani2)
        } else {
        #   updateRadioButtons(session,"estNouvelAnimal", selected = "oui")
        #   updateSelectizeInput(session, "idSite",  selected = site)
        #   updateTextInput(session, "nAnimal",  value = ani)}
        # #updateSelectizeInput(session, "numSabot_capture", choices = unique(fichier_lu[,c("num_sabot")]),  selected = sabot)
        # updateSelectizeInput(session, "age",  selected = age)
        # updateTextInput(session, "idTagOrG", value =  tagd)
        # updateTextInput(session, "idTagOrD", value =  tagg)
        # output$poids_ani = renderText({poids})
        # updateTextInput(session,"remarque_ani", value = sante)
        # updateSelectInput(session, "sexe", choices =choix[["sexe"]], selected = sexe)
        
        output$csv <-DT::renderDataTable({
          c<-DT::datatable(as.data.frame(fichier_lu))
          return(c)})
      }}}}}
  })
  

  ##################           RUBRIQUE SABOT                                       #################
  
  updateSelectizeInput(session, "cpt_dose_acepromazine", choices = choix[["cpt_dose_acepromazine"]])
  
  #####calcul de l'heure en sabot
  observe({
    if (!is.na(strptime(input$cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S"))) { if (!is.na(input$cpt_temps_filet)) {
      heure_sab<- strptime(input$cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S") + input$cpt_temps_filet*60
      updateTimeInput(session, "cpt_heure_mise_sabot", value = heure_sab)
    }}
  })
  
  ##################           RUBRIQUE CHECKLIST 3                                 #################
  
  checklist_capture = data.frame()
  row.names(checklist_capture) = NULL
  output$tablechecklist_capture = DT::renderDT(expr = checklist_capture,server = F)
  
  checklist_sabot = data.frame()
  row.names(checklist_sabot) = NULL
  output$tablechecklist_sabot = DT::renderDT(expr = checklist_sabot,server = F)
  
  output$checklist_capture <- renderUI( {
    
    if (length(unlist(strsplit(as.character(input$cpt_heure_debut_filet), " "))) == 2){
      gettime=as.character(input$cpt_heure_debut_filet)
      gettime=strsplit(gettime, " ")[[1]]
      gettime<<-sub(":00$","", gettime[2])} else {gettime <- "00:00"}
    if (length(strsplit(as.character(input$cpt_heure_mise_sabot), " ")[[1]]) == 2){
      gettime3=as.character(input$cpt_heure_mise_sabot)
      gettime3=strsplit(gettime3, " ")[[1]]
      gettime3<<-sub(":00$","", gettime3[2])}else{gettime3 <- "00:00"}
    if (length(strsplit(as.character(input$cpt_heure_fin_surv), " ")[[1]]) == 2){
      gettime4=as.character(input$cpt_heure_fin_surv)
      gettime4=strsplit(gettime4, " ")[[1]]
      gettime4<<-sub(":00$","", gettime4[2])} else {gettime4 <- "00:00"}
    
    checklist_capture = data.frame()
    
    if ((input$date_capture)=="")  {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Date")))}
    
    if ((gettime)== "00:00")  {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Heure début filet")))}
    
    if (is.na(input$cpt_temps_filet))  {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Temps passé au filet")))}
    
    if ((input$nom_capteur_txt)=="") {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Capteurs")))}
    
    if ((input$Nbre_pers_experimentes)=="") {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Nombre de personnes expérimentées")))}
    
    if (input$cpt_filet_vitesse == "NA" || input$cpt_filet_vitesse == "") {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Vitesse filet")))}
    
    if (input$cpt_filet_allure == "NA" || input$cpt_filet_allure == "") {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Allure filet")))}
    
    if (input$cpt_filet_lutte == "NA" || input$cpt_filet_lutte == "") {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Lutte filet")))}
    
    if (input$cpt_filet_halete == "NA" || input$cpt_filet_halete == "") {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Halete")))}
    
    if (input$cpt_filet_cri == "NA" || input$cpt_filet_cri == "") {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Cri")))}
    
    if (nrow(checklist_capture)==0) {
      checklist_capture =  rbind(checklist_capture,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
    
    output$tablechecklist_capture = DT::renderDT(checklist_capture,server = F)
    
    checklist_sabot = data.frame()
    
    if ((gettime3)=="00:00") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Heure mise en sabot")))}
    
    if ((gettime4)=="00:00") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Heure fin de surveillance")))}
    
    if ((input$cpt_dose_acepromazine)=="") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Dose azepromazine")))}
    
    if (input$cpt_sabot_retournement == "NA" || input$cpt_sabot_retournement == "") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Retournement ?")))}
    
    if (input$cpt_sabot_couche == "NA" || input$cpt_sabot_couche == "") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Couché ?")))}
    
    if (input$cpt_sabot_agitation == "NA" || input$cpt_sabot_agitation == "") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Agité ?")))}
    
    if ((input$Observateur)=="") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Observateur")))}
    
    if (nrow(checklist_sabot)==0) {
      checklist_sabot =  rbind(checklist_sabot,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
    
    output$tablechecklist_sabot = DT::renderDT(checklist_sabot,server = F)
    
    
    ### Bilan
    
    # observeEvent(input$valid_checklist3, {
    #   if  ((checklist_capture[1,1]!="PAS DE DONNEES MANQUANTES") || (checklist_sabot[1,1]!="PAS DE DONNEES MANQUANTES"))
    #   {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE, callbackR = modalCallback_check3)}
    #   else
    #   {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check3)}
    # 
    # }, ignoreInit = T)

 
  
  onclick("valid_checklist3", {
    
    if  ((checklist_capture[1,1]!="PAS DE DONNEES MANQUANTES") || (checklist_sabot[1,1]!="PAS DE DONNEES MANQUANTES"))
    {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE, callbackR = modalCallback_check3)}
    else
    {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check3)}
    
  })
  })
  ##################           CSV CHECKLIST 1                                      #####     
  
  modalCallback_check1 = function(value1) {
    if (value1 == TRUE) {
      
      test_checklist1_validated <<- value1
      
      save1 = data.frame()
      
      if (startsWith(input$nAnimal, "F")){
        faon1 =  "oui" }
      else {faon1="non"}
      
      if (startsWith(input$nAnimal2, "F")){
        faon2 =  "oui" }
      else {faon2="non"}
      
      date_mod = input$date_caract
      date_mod = format(date_mod, "%d/%m/%Y")
      date_mod = as.character(date_mod)
      
      jour = strsplit(date_mod, "/")[[1]][1]
      mois = strsplit(date_mod, "/")[[1]][2]
      annee = strsplit(date_mod, "/")[[1]][3]
      
      if (input$age == '0.5' || input$age == '<1' ) {
        cat_age_all = "jeune" 
        cat_age = "j"}
      else if (input$age=='1.5' || input$age=='1') {
        cat_age_all = "yearling"
        cat_age = "y" }
      else if (input$age=='2' || input$age=='2.5' || input$age=='3' || input$age=='3.5' || input$age=='4.5-5.5' || input$age=='4-5' || input$age=='>=6' || input$age=='>6.5') {cat_age_all="adulte"
      cat_age=""}
      else {cat_age_all="" 
      cat_age=""}
      
      if (!is.null(input$criautre) && !is.null(input$cribague)) {
        if (input$criautre!='0' || (input$cribague=='1-2' || input$cribague=='>2'))
        {cri_total = 1}
        else {cri_total = 0}
      }
      else {cri_total=""}
      
      ligne_selection <<- input$tablecollier_rows_selected
      collier_tech = query()[ligne_selection,"teq_nom_court"]
      collier_col_c = query()[ligne_selection,"eqc_couleur_collier"]
      collier_col_b = query()[ligne_selection,"eqc_couleur_boitier"]
      cat_col = paste(toupper(collier_tech),": collier ", toupper(collier_col_b)," boitier ", toupper(collier_col_c))
      
      if (input$nAnimal2 != "") {
        nbre_capt = dbGetQuery(con,paste0("SELECT count(cap_id) FROM public.t_capture_cap, public.t_animal_ani where ani_id = cap_ani_id and ani_etiq= '",input$nAnimal2,"' group by ani_etiq order by ani_etiq"))
        nbre_capt <- nbre_capt[1,1]
        
        cap_pertinent = dbGetQuery(con,paste0("select cap_annee_suivi from public.t_capture_cap, public.t_animal_ani where cap_ani_id=ani_id and ani_etiq = '",input$nAnimal2,"' order by cap_annee_suivi DESC"))
        cap_pertinent <- cap_pertinent[1,1]
        if (annee == cap_pertinent) {cap_pertinent = FALSE} else {cap_pertinent = TRUE} }
      

      #######traitement des prélèvements
      if (nrow(prelevement) == 0) {prel <- list(NA)} else {
        prelevement_liste <- list()
        for (i in 1:nrow(prelevement)){
          prelevement_liste [[i]] <-paste0(apply(prelevement[i,],2,as.character),collapse ="_")
        }
          if (nrow(prelevement) == 1) {type_prelevement <-unique(as.matrix(t(apply(prelevement,2,as.character)))[,"Type"])} else {type_prelevement <-unique(as.matrix(apply(prelevement,2,as.character))[,"Type"])}
        prel <- list()
        for (i in 1:length(type_prelevement)) {
          prel[[i]] <-grep(type_prelevement[i],prelevement_liste, value =TRUE)
          if (length(prel[[i]] > 1)) {prel[[i]] <- paste0(prel[[i]],collapse ="~")}
        }}####fin traitement des prelevements
      
      #######traitement des variables avec heure
      if (nrow(mes_heure) == 0) {mes <- list(NA)} else {
        mes_heure_liste <- list()
        mes <- list()
        for (i in 1:nrow(mes_heure)){
          mes_heure_liste [[i]] <-paste0(apply(mes_heure[i,],2,as.character),collapse ="-")
        }
        if (nrow(mes_heure) == 1) {type_mes <-unique(as.matrix(t(apply(mes_heure,2,as.character)))[,"Mesure"])} else {type_mes <-unique(as.matrix(apply(mes_heure,2,as.character))[,"Mesure"])}
        for (i in 1:length(type_mes)) {
          mes[[i]] <-grep(type_mes[i],mes_heure_liste, value =TRUE)
          if (length(mes[[i]] > 1)) {mes[[i]] <- paste0(mes[[i]],collapse ="~")}
        }}####fin traitement des mesures
      
      if (exists("collier_tech")) {
        if (!is.null(ligne_selection)) {
          collier_tech_test = collier_tech }
        else{collier_tech_test = ""}}
      else{collier_tech_test = ""}
      
      sen_association <-  dbGetQuery(con,"select sen_association from listes.tr_sensors_sen")
      sen_id_acc <- sen_association[grep("accelerometre",as.character(sen_association[,1])),1]
      sen_id_prox <- sen_association[grep("proximite",as.character(sen_association[,1])),1]
      sen_id_act <- sen_association[grep("activite",as.character(sen_association[,1])),1]
      
      if (!is.null(ligne_selection)) {
        collier_test = query()[ligne_selection,"sen_association"]
        collier_id_usuel <<- query()[ligne_selection,"eqt_id_usuel"] }
      else {collier_id_usuel <<- ""}
      
      if (!is.null(ligne_selection)) {
        if (collier_test != "rien") {
          if (collier_test %in% sen_id_acc) {
            collier_acc <- 1} else {collier_acc <- ""}} else {collier_acc <- ""}} else {collier_acc <- ""}
      
      if (!is.null(ligne_selection)) {
        if (collier_test != "rien") {
          if (collier_test %in% sen_id_prox) {
            collier_prox = 1 } else {collier_prox <- ""}} else {collier_prox <- ""}} else {collier_prox <- ""}
      
      if (!is.null(ligne_selection)) {
        if (collier_test != "rien") {                
          if (collier_test %in% sen_id_act) {
            collier_act = 1}else {collier_act <- ""}} else {collier_act <- ""}} else {collier_act <- ""}
      
      diarrhee = paste("diarrhee/",input$diarrhee, sep="")
      bledia = paste(input$liste_blessures, diarrhee)
      
      if (as.integer(mois)>=10) {
        annee_suivie <- as.integer(annee) + 1  }
      if (as.integer(mois)<10) {annee_suivie <- annee}
      
      #loc_sonde =  as.integer(dbGetQuery(con, "select tel_id from listes.tr_temperatures_localisation_tel where tel_localisation = 'anus'")[1,1])
      
      if (exists("gettime_table_posix")) {
        if (exists("gettime_caract_posix")) {
          duree_marquage <- times(as.numeric(difftime(gettime_table_posix, gettime_caract_posix)) / (24*3600))}
        else {duree_marquage=""}} else {duree_marquage=""}
      
      ######cas d''une capture         
      if(((input$estNouvelAnimal == 'oui') || (input$estNouvelAnimal == 'non' && input$identifie == 'non')) && input$nAnimal!="") {
        
        #####cap_date dans la bd
        if (input$nAnimal != "") {
          if (startsWith(input$nAnimal, "F")){
            faon <- TRUE }
          else {faon<- FALSE}
        
        if (faon == TRUE) {
          if   (input$idTagOrD !=""){
            cap_bague2 <<- paste0("F", input$idTagOrD, "_", str_sub(annee_suivie, -2)) } else{cap_bague2 <<- paste0("F", input$idTagOrG, "_", str_sub(annee_suivie, -2))}
        } else {
          if   (input$idTagOrD !=""){
            cap_bague2 <<- paste0(input$idTagOrD, "_", str_sub(annee_suivie, -2)) } else{cap_bague2 <<- paste0(input$idTagOrG, "_", str_sub(annee_suivie, -2))}
        }
        ani_id<- as.character(dbGetQuery(con, paste0("select ani_id from public.t_animal_ani where ani_etiq= '",input$nAnimal,"'"))[1,1])
        cap_id<- as.integer(as.character(dbGetQuery(con, paste0("SELECT cap_id FROM t_capture_cap WHERE cap_ani_id = ",ani_id," "))[1,1]))
        
        if (exists("cap_id")) {
          req<-paste0("UPDATE public.t_capture_cap SET (cap_bague) = ('",cap_bague2,"') where  cap_id= ",cap_id," ")
          
          req = gsub("'NA'","NULL", req)
          req = gsub("''","NULL", req)
          
          dbSendQuery(con, req)}}  
        
        save1 = data.frame("N°Animal" = c(input$nAnimal))
        save1 = cbind(save1,data.frame("ani_nom" = c("")))
        save1 = cbind(save1,data.frame("N°Animal telemetrie" = c(paste0(tolower(input$sexe),cat_age,"_",input$nAnimal))))
        save1 = cbind(save1,data.frame("N° bague annee capture" = c(cap_bague2)))
        save1 = cbind(save1,data.frame("Nombre capture" = c(1)))
        save1 = cbind(save1,data.frame("inconnue" = c("TRUE")))
        save1 = cbind(save1,data.frame("Site Capture" = c(input$idSite)))
        save1 = cbind(save1,data.frame("capture faon" = c(faon1)))
        save1 = cbind(save1,data.frame("Date" = c(input$date_caract)))
        save1 = cbind(save1,data.frame("jour" = c(jour)))
        save1 = cbind(save1,data.frame("mois" = c(mois)))
        save1 = cbind(save1,data.frame("annee" = c(annee)))
        save1 = cbind(save1,data.frame("annee  de suivi" = c(annee_suivie)))
        if (!is.null(input$sexe)) {save1 = cbind(save1,data.frame("Sexe" = as.character(c(input$sexe)))) }
        if (is.null(input$sexe)) {save1 = cbind(save1,data.frame("Sexe" = c(""))) }            
        save1 = cbind(save1,data.frame("Age cahier" = c(input$age)))
        save1 = cbind(save1,data.frame("Age corrige" = c(input$age)))
        save1 = cbind(save1,data.frame("categorie d'age" = c(cat_age_all)))
        save1 = cbind(save1,data.frame("etat_sante" = c(bledia)))
        save1 = cbind(save1,data.frame("cap_tag_droit" = c(input$idTagOrD)))
        save1 = cbind(save1,data.frame("cap_tag_gauche" = c(input$idTagOrG)))
        save1 = cbind(save1,data.frame("cap_tag_droit_metal" = c(input$metal_tag_d)))
        save1 = cbind(save1,data.frame("cap_tag_gauche_metal" = c(input$metal_tag_g)))
        save1 = cbind(save1,data.frame("cap_pertinent" = c(TRUE)))
        if (is.null(input$sexe)) {save1 = cbind(save1,data.frame("cap_lactation" = c("indeterminé"))) }
        if (input$sexe == 'M') {save1 = cbind(save1,data.frame("cap_lactation" = c("non")))}
        if (input$sexe == 'F') {save1 = cbind(save1,data.frame("cap_lactation" = c(input$lactation)))}
        #save1 = cbind(save1,data.frame("RFID" = c(input$idRFID)))
        # print(paste0("input$rfid:",input$idRFID,""))
        # print(paste0("input$rfid not null:",input$idRFID!="" || !is.null(input$idRFID),""))
        # print(paste0("input$rfid null:",length(input$idRFID) != 0 & (input$idRFID =="" || is.null(input$idRFID)),""))
        # if (length(input$idRFID) != 0) { if (input$idRFID!="" || !is.null(input$idRFID)) {save1 = cbind(save1,data.frame("RFID" = c(input$idRFID)))}}
        # if (length(input$idRFID) != 0 & (input$idRFID =="" || is.null(input$idRFID))) {save1 = cbind(save1,data.frame("RFID" = c("")))}
        # if (length(grep("RFID", names(save1))) == 0) {save1 = cbind(save1,data.frame("RFID" = c("")))}
        print(paste0("input$rfid:",input$idRFID,""))
	      if (!is.null(input$idRFID) && nchar(input$idRFID) == 10) {save1 = cbind(save1,data.frame("RFID" = c(input$idRFID)))} else {
        save1 = cbind(save1,data.frame("RFID" = c("")))}
        save1 = cbind(save1,data.frame("Poids" = c(input$pSabotPlein - input$pSabotVide)))
        save1 = cbind(save1,data.frame("Cir Cou" = c(input$cirCou)))
        save1 = cbind(save1,data.frame("Long patte Ar" = c(input$lPattArriere)))
        save1 = cbind(save1,data.frame("machoire" = c("")))
        save1 = cbind(save1,data.frame("long bois gauche" = c(input$lBoisGauche)))
        save1 = cbind(save1,data.frame("long bois droit" = c(input$lBoisDroit)))
        save1 = cbind(save1,data.frame("glucose" = c(input$tglucose)))
        save1 = cbind(save1,data.frame("T°C_ext" = c("")))
        save1 = cbind(save1,data.frame("TIQUES FIXES" = c(input$tiques)))
        save1 = cbind(save1,data.frame("autres parasites"= c(input$parasites)))
        if (length(grep("peau", prel)) != 0) {save1 = cbind(save1,data.frame("Peau" = c(prel[[grep("peau", prel)]])))} else {save1 = cbind(save1,data.frame("Peau" = c("")))}
        if (length(grep("poils", prel)) != 0) {save1 = cbind(save1,data.frame("poils" = c(prel[[grep("poils", prel)]])))} else {save1 = cbind(save1,data.frame("poils" = c("")))}
        if (length(grep("sang", prel)) != 0) {save1 = cbind(save1,data.frame("sang" = c(prel[[grep("sang", prel)]])))} else {save1 = cbind(save1,data.frame("sang" = c("")))}
        if (length(grep("feces", prel)) != 0) {save1 = cbind(save1,data.frame("feces" = c(prel[[grep("feces", prel)]])))} else {save1 = cbind(save1,data.frame("feces" = c("")))}
        if (length(grep("tiques", prel)) != 0) {save1 = cbind(save1,data.frame("tiques" = c(prel[[grep("tiques", prel)]])))} else {save1 = cbind(save1,data.frame("tiques" = c("")))}
        if (length(grep("vagin", prel)) != 0) {save1 = cbind(save1,data.frame("vaginal" = c(prel[[grep("vagin", prel)]])))} else {save1 = cbind(save1,data.frame("vaginal" = c("")))}
        if (length(grep("nez", prel)) != 0) {save1 = cbind(save1,data.frame("Nasal" = c(prel[[grep("nez", prel)]])))} else {save1 = cbind(save1,data.frame("Nasal" = c("")))}
        if (!is.null(input$remarques_ani)) {save1 = cbind(save1,data.frame("remarque" = c(input$remarques_ani)))} else {save1 = cbind(save1,data.frame("remarque" = c("")))}
        save1 = cbind(save1,data.frame("Collier" = c(collier_tech_test)))
        save1 = cbind(save1,data.frame("accelero" = c(collier_acc)))
        save1 = cbind(save1,data.frame("proximite" = c(collier_prox)))
        save1 = cbind(save1,data.frame("id_collier" = c(collier_id_usuel)))
        save1 = cbind(save1,data.frame("dat_deb" = c(input$date_caract)))
        save1 = cbind(save1,data.frame("date_fin" = c("")))
        save1 = cbind(save1,data.frame("date_fin arrondie" = c("")))
        save1 = cbind(save1,data.frame("date_fin_capteur" = c("")))
        save1 = cbind(save1,data.frame("suivi_GPS oui si>60jours" = c("")))
        save1 = cbind(save1,data.frame("jrs_suivi" = c("")))
        save1 = cbind(save1,data.frame("capteur Activite" = c(collier_act)))
        if (!is.null(input$remarque_collier)) {save1 = cbind(save1,data.frame("probleme collier" = c(input$remarque_collier)))} else {save1 = cbind(save1,data.frame("probleme collier" = c("")))}
        save1 = cbind(save1,data.frame("site vie" = c("")))
        save1 = cbind(save1,data.frame("secteur" = c("")))
        save1 = cbind(save1,data.frame("Mort" = c("")))
        save1 = cbind(save1,data.frame("Date mort" = c("")))
        save1 = cbind(save1,data.frame("Date mort arrondie" = c("")))
        save1 = cbind(save1,data.frame("Cause detaille" = c("")))
        save1 = cbind(save1,data.frame("cause categories" = c("")))
        save1 = cbind(save1,data.frame("Pds mort" = c("")))
        save1 = cbind(save1,data.frame("nom capteur" = c("")))
        save1 = cbind(save1,data.frame("nombre d'experimentes (n)" = c("")))
        save1 = cbind(save1,data.frame("arrivee filet course (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("arrivee filet panique (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("lutte" = (c(""))))
        save1 = cbind(save1,data.frame("haletement (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("cri (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("acepromazine (1=0,3cc)" = c("")))
        save1 = cbind(save1,data.frame("num_sabot" = c(input$numSabot)))
        save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("agitation (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("retournement (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("hre fin surv" = c("")))
        save1 = cbind(save1,data.frame("surveillance (mn)" = c("")))
        save1 = cbind(save1,data.frame("distance (KM)" = c("")))
        if (is.null(input$lutte)) { save1 = cbind(save1,data.frame("lutte (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("lutte (1/0)" = (c(input$lutte))))}
        if (is.null(input$halete)) { save1 = cbind(save1,data.frame("halete (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("halete (1/0)" = (c(input$halete))))}
        save1 = cbind(save1,data.frame("cri" = c(cri_total)))
        if( length(grep("temperature anale_ysi",mes)) != 0) {save1 = cbind(save1,data.frame("T°C 1" = gsub("temperature anale_","",grep("temperature anale_ysi",mes, value =TRUE))))} else{save1 = cbind(save1,data.frame("T°C 1" = ""))}
        if( length(grep("temperature ext_ysi",mes)) != 0) {save1 = cbind(save1,data.frame("T°C 2" = gsub("temperature ext_","",grep("temperature ext_ysi",mes, value =TRUE))))} else {save1 = cbind(save1,data.frame("T°C 2" = ""))}
        if( length(grep("rythme cardiaque",mes)) != 0) {save1 = cbind(save1,data.frame("Cœur 1" = gsub("rythme cardiaque-","",grep("rythme cardiaque",mes, value =TRUE))))} else{save1 = cbind(save1,data.frame("Cœur 1" = ""))}
        save1 = cbind(save1,data.frame("Cœur 2" = c("")))
        if( length(grep("ventilation",mes)) != 0) {save1 = cbind(save1,data.frame("Ventilation" = gsub("ventilation-","",grep("ventilation",mes, value =TRUE))))} else {save1 = cbind(save1,data.frame("Ventilation" = ""))}
        save1 = cbind(save1,data.frame("remarque_table" = c(input$remarques_table)))
        save1 = cbind(save1,data.frame("localisation sonde temperature" = c("")))
        save1 = cbind(save1,data.frame("eurodeer" = c(input$Notation_euro_table)))
        save1 = cbind(save1,data.frame("titube (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("course (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("tombe (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("gratte collier (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("cabriole (1/0)" = (c("")))) 
        save1 = cbind(save1,data.frame("bolide (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("aboiement/cri (1/0)" = (c("")))) 
        save1 = cbind(save1,data.frame("filet" = c("")))
        save1 = cbind(save1,data.frame("sabot sur place" = c("")))
        save1 = cbind(save1,data.frame("transport+attente" = c("")))
        save1 = cbind(save1,data.frame("marquage" = c(duree_marquage) ))
        save1 = cbind(save1,data.frame("total" = c("")))
        save1 = cbind(save1,data.frame("capture" = c("")))
        save1 = cbind(save1,data.frame("sabot" = c("")))
        save1 = cbind(save1,data.frame("acepro" = c("")))
        save1 = cbind(save1,data.frame("transport" = c("")))
        save1 = cbind(save1,data.frame("table" = c(input$time_caract)))
        save1 = cbind(save1,data.frame("lache" = c("")))
        save1 = cbind(save1,data.frame("remarque_generale" = c("")))
        if (is.null(input$cribague)) { save1 = cbind(save1,data.frame("bague" = (c(""))))} else {save1 = cbind(save1,data.frame("bague" = c(input$cribague)) )}
        if (is.null(input$criautre)) { save1 = cbind(save1,data.frame("autre" = (c(""))))} else {save1 = cbind(save1,data.frame("autre" = c(input$criautre)))}
        save1 = cbind(save1,data.frame("stop" = c("")))
        save1 = cbind(save1,data.frame("habitat lacher" = c("")))
        save1 = cbind(save1,data.frame("habite perte vue" = c("")))
        save1 = cbind(save1,data.frame("visibilite" = c("")))
        save1 = cbind(save1,data.frame("nb_public" = c("")))
        save1 = cbind(save1,data.frame("eurodeer_lacher" = c("")))
        save1 = cbind(save1,data.frame("remise sabot" = c("")))
        save1 = cbind(save1,data.frame("heure_lacher_2" = c("")))

        if (exists("cap_id")) {
          req<-paste0("UPDATE public.t_capture_cap SET cap_bague = '",cap_bague2,"' where  cap_id= ",cap_id," ")
          req = gsub("'NA'","NULL", req)
          req = gsub("''","NULL", req)
          
          dbSendQuery(con, req)  
        
        if (length(grep("temperature anale",mes)) != 0){
          dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_tble_temp_animal = '",gsub("temperature anale_","",grep("temperature anale",mes, value =TRUE)),"' where cpt_cap_id = ",cap_id," "))
        }
        if (length(grep("temperature ext",mes)) != 0){
          dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_tble_temp_exterieur = '",gsub("temperature ext_","",grep("temperature ext",mes, value =TRUE)),"' where cpt_cap_id = ",cap_id," "))
        }
        if (length(grep("rythme cardiaque",mes)) != 0){
          dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_tble_coeur = '",gsub("rythme cardiaque-","",grep("rythme cardiaque",mes, value =TRUE)),"' where cpt_cap_id = ",cap_id," "))
        }
        if (length(grep("ventilation",mes)) != 0){
          dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_tble_ventilation = '",gsub("ventilation-","",grep("ventilation",mes, value =TRUE)),"' where cpt_cap_id = ",cap_id," "))
        }}
       print(save1)
      }
      
      ######## cas d''une recapture 
      
      if(input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$nAnimal2!="") {
        
        save1 = data.frame("N°Animal" = c(input$nAnimal2))
        save1 = cbind(save1,data.frame("ani_nom" = c("")))
        save1 = cbind(save1,data.frame("N°Animal telemetrie" = c(paste0(tolower(input$sexe),cat_age,"_",input$nAnimal2))))
        save1 = cbind(save1,data.frame("N° bague annee capture" = c("")))
        save1 = cbind(save1,data.frame("Nombre capture" = c(nbre_capt)))
        save1 = cbind(save1,data.frame("inconnue" = c("FALSE")))
        save1 = cbind(save1,data.frame("Site Capture" = c(input$idSite2)))
        save1 = cbind(save1,data.frame("capture faon" = c(faon2)))
        save1 = cbind(save1,data.frame("Date" = c(input$date_caract)))
        save1 = cbind(save1,data.frame("jour" = c(jour)))
        save1 = cbind(save1,data.frame("mois" = c(mois)))
        save1 = cbind(save1,data.frame("annee" = c(annee)))
        save1 = cbind(save1,data.frame("annee  de suivi" = c(annee_suivie)))
        if (!is.null(input$sexe)) {save1 = cbind(save1,data.frame("Sexe" = c(input$sexe))) }
        if (is.null(input$sexe)) {save1 = cbind(save1,data.frame("Sexe" = c(""))) }
        save1 = cbind(save1,data.frame("Age cahier" = c(input$age)))
        save1 = cbind(save1,data.frame("Age corrige" = c(input$age)))
        save1 = cbind(save1,data.frame("categorie d'age" = c(cat_age_all)))
        save1 = cbind(save1,data.frame("etat_sante" = c(bledia)))
        if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagD == F) {save1 = cbind(save1,data.frame("cap_tag_droit" = c(input$idTagOrD2))) }
        if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagD == T) {save1 = cbind(save1,data.frame("cap_tag_droit" = c(input$idTagOrD3))) }
        if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagG == F) {save1 = cbind(save1,data.frame("cap_tag_gauche" = c(input$idTagOrG2))) }
        if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagG == T) {save1 = cbind(save1,data.frame("cap_tag_gauche" = c(input$idTagOrG3))) }
        if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagD == F) {save1 = cbind(save1,data.frame("cap_tag_droit_metal" = c(input$metal_tag_d2)))}
        if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagD == T) {save1 = cbind(save1,data.frame("cap_tag_droit_metal" = c(input$metal_tag_d3))) }
        if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagG == F) {save1 = cbind(save1,data.frame("cap_tag_gauche_metal" = c(input$metal_tag_g2))) }
        if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagG == T) {save1 = cbind(save1,data.frame("cap_tag_gauche_metal" = c(input$metal_tag_g3))) }
        save1 = cbind(save1,data.frame("cap_pertinent" = c(cap_pertinent)))
        if (is.null(input$sexe)) {save1 = cbind(save1,data.frame("cap_lactation" = c("indeterminé"))) }
        if (input$sexe == 'M') {save1 = cbind(save1,data.frame("cap_lactation" = c("non")))}
        if (input$sexe == 'F') {save1 = cbind(save1,data.frame("cap_lactation" = c(input$lactation)))}
        if (input$newRFIDbox == F && input$idRFID2!="") {save1 = cbind(save1,data.frame("RFID" = c(input$idRFID2)))}
        if (input$newRFIDbox == F && input$idRFID2=="") {save1 = cbind(save1,data.frame("RFID" = c("")))}
        if (input$newRFIDbox == T && input$idRFID_new!="" ){save1 = cbind(save1,data.frame("RFID" = c(input$idRFID_new)))}
        if (input$newRFIDbox == T && input$idRFID_new=="" ){ save1 = cbind(save1,data.frame("RFID" = c("")))}
        save1 = cbind(save1,data.frame("Poids" = c(input$pSabotPlein - input$pSabotVide)))
        save1 = cbind(save1,data.frame("Cir Cou" = c(input$cirCou)))
        save1 = cbind(save1,data.frame("Long patte Ar" = c(input$lPattArriere)))
        save1 = cbind(save1,data.frame("machoire" = c("")))
        save1 = cbind(save1,data.frame("long bois gauche" = c(input$lBoisGauche)))
        save1 = cbind(save1,data.frame("long bois droit" = c(input$lBoisDroit)))
        save1 = cbind(save1,data.frame("glucose" = c(input$tglucose)))
        save1 = cbind(save1,data.frame("T°C_ext" = c("")))
        save1 = cbind(save1,data.frame("TIQUES FIXES" = c(input$tiques)))
        save1 = cbind(save1,data.frame("autres parasites"= c(input$parasites)))
        if (length(grep("peau", prel)) != 0) {save1 = cbind(save1,data.frame("Peau" = c(prel[[grep("peau", prel)]])))} else {save1 = cbind(save1,data.frame("Peau" = c("")))}
        if (length(grep("poils", prel)) != 0) {save1 = cbind(save1,data.frame("poils" = c(prel[[grep("poils", prel)]])))} else {save1 = cbind(save1,data.frame("poils" = c("")))}
        if (length(grep("sang", prel)) != 0) {save1 = cbind(save1,data.frame("sang" = c(prel[[grep("sang", prel)]])))} else {save1 = cbind(save1,data.frame("sang" = c("")))}
        if (length(grep("feces", prel)) != 0) {save1 = cbind(save1,data.frame("feces" = c(prel[[grep("feces", prel)]])))} else {save1 = cbind(save1,data.frame("feces" = c("")))}
        if (length(grep("tiques", prel)) != 0) {save1 = cbind(save1,data.frame("tiques" = c(prel[[grep("tiques", prel)]])))} else {save1 = cbind(save1,data.frame("tiques" = c("")))}
        if (length(grep("vagin", prel)) != 0) {save1 = cbind(save1,data.frame("vaginal" = c(prel[[grep("vagin", prel)]])))} else {save1 = cbind(save1,data.frame("vaginal" = c("")))}
        if (length(grep("nez", prel)) != 0) {save1 = cbind(save1,data.frame("Nasal" = c(prel[[grep("nez", prel)]])))} else {save1 = cbind(save1,data.frame("Nasal" = c("")))}
        if (!is.null(input$remarques_ani)) {save1 = cbind(save1,data.frame("remarque" = c(input$remarques_ani)))} else {save1 = cbind(save1,data.frame("remarque" = c("")))}
        save1 = cbind(save1,data.frame("Collier" = c(collier_tech_test)))
        save1 = cbind(save1,data.frame("accelero" = c(collier_acc)))
        save1 = cbind(save1,data.frame("proximite" = c(collier_prox)))
        save1 = cbind(save1,data.frame("id_collier" = c(collier_id_usuel)))
        save1 = cbind(save1,data.frame("dat_deb" = c(input$date_caract)))
        save1 = cbind(save1,data.frame("date_fin" = c("")))
        save1 = cbind(save1,data.frame("date_fin arrondie" = c("")))
        save1 = cbind(save1,data.frame("date_fin_capteur" = c("")))
        save1 = cbind(save1,data.frame("suivi_GPS oui si>60jours" = c("")))
        save1 = cbind(save1,data.frame("jrs_suivi" = c("")))
        save1 = cbind(save1,data.frame("capteur Activite" = c(collier_act)))
        if (!is.null(input$remarque_collier)) {save1 = cbind(save1,data.frame("probleme collier" = c(input$remarque_collier)))} else {save1 = cbind(save1,data.frame("probleme collier" = c("")))}
        save1 = cbind(save1,data.frame("site vie" = c("")))
        save1 = cbind(save1,data.frame("secteur" = c("")))
        save1 = cbind(save1,data.frame("Mort" = c("")))
        save1 = cbind(save1,data.frame("Date mort" = c("")))
        save1 = cbind(save1,data.frame("Date mort arrondie" = c("")))
        save1 = cbind(save1,data.frame("Cause detaille" = c("")))
        save1 = cbind(save1,data.frame("cause categories" = c("")))
        save1 = cbind(save1,data.frame("Pds mort" = c("")))
        save1 = cbind(save1,data.frame("nom capteur" = c("")))
        save1 = cbind(save1,data.frame("nombre d'experimentes (n)" = c("")))
        save1 = cbind(save1,data.frame("arrivee filet course (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("arrivee filet panique (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("lutte" = (c(""))))
        save1 = cbind(save1,data.frame("haletement (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("cri (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("acepromazine (1=0,3cc)" = c("")))
        save1 = cbind(save1,data.frame("num_sabot" = c(input$numSabot)))
        save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("agitation (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("retournement (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("hre fin surv" = c("")))
        save1 = cbind(save1,data.frame("surveillance (mn)" = c("")))
        save1 = cbind(save1,data.frame("distance (KM)" = c("")))
        if (is.null(input$lutte)) { save1 = cbind(save1,data.frame("lutte (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("lutte (1/0)" = (c(input$lutte))))}
        if (is.null(input$halete)) { save1 = cbind(save1,data.frame("halete (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("halete (1/0)" = (c(input$halete))))}
        save1 = cbind(save1,data.frame("cri" = c(cri_total)))
        if( length(grep("temperature anale_ysi",mes)) != 0) {save1 = cbind(save1,data.frame("T°C 1" = gsub("temperature anale_","",grep("temperature anale_ysi",mes, value =TRUE))))} else{save1 = cbind(save1,data.frame("T°C 1" = ""))}
        if( length(grep("temperature ext_ysi",mes)) != 0) {save1 = cbind(save1,data.frame("T°C 2" = gsub("temperature ext_","",grep("temperature ext_ysi",mes, value =TRUE))))} else {save1 = cbind(save1,data.frame("T°C 2" = ""))}
        if( length(grep("rythme cardiaque",mes)) != 0) {save1 = cbind(save1,data.frame("Cœur 1" = gsub("rythme cardiaque-","",grep("rythme cardiaque",mes, value =TRUE))))} else{save1 = cbind(save1,data.frame("Cœur 1" = ""))}
        save1 = cbind(save1,data.frame("Cœur 2" = c("")))
        if( length(grep("ventilation",mes)) != 0) {save1 = cbind(save1,data.frame("Ventilation" = gsub("ventilation-","",grep("ventilation",mes, value =TRUE))))} else {save1 = cbind(save1,data.frame("Ventilation" = ""))}
        save1 = cbind(save1,data.frame("remarque_table" = c(input$remarques_table)))
        save1 = cbind(save1,data.frame("localisation sonde temperature" = c(""))) 
        save1 = cbind(save1,data.frame("eurodeer" = c(input$Notation_euro_table)))
        save1 = cbind(save1,data.frame("titube (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("course (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("tombe (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("gratte collier (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("cabriole (1/0)" = (c("")))) 
        save1 = cbind(save1,data.frame("bolide (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("aboiement/cri (1/0)" = (c(""))))
        save1 = cbind(save1,data.frame("filet" = c("")))
        save1 = cbind(save1,data.frame("sabot sur place" = c("")))
        save1 = cbind(save1,data.frame("transport+attente" = c("")))
        save1 = cbind(save1,data.frame("marquage" = c(duree_marquage)))
        save1 = cbind(save1,data.frame("total" = c("")))
        save1 = cbind(save1,data.frame("capture" = c("")))
        save1 = cbind(save1,data.frame("sabot" = c("")))
        save1 = cbind(save1,data.frame("acepro" = c("")))
        save1 = cbind(save1,data.frame("transport" = c("")))
        save1 = cbind(save1,data.frame("table" = c(input$time_caract)))
        save1 = cbind(save1,data.frame("lache" = c("")))
        save1 = cbind(save1,data.frame("remarque_generale" = c("")))
        if (is.null(input$cribague)) { save1 = cbind(save1,data.frame("bague" = (c(""))))} else {save1 = cbind(save1,data.frame("bague" = c(input$cribague)) )}
        if (is.null(input$criautre)) { save1 = cbind(save1,data.frame("autre" = (c(""))))} else {save1 = cbind(save1,data.frame("autre" = c(input$criautre)))}
        save1 = cbind(save1,data.frame("stop" = c("")))
        save1 = cbind(save1,data.frame("habitat lacher" = c("")))
        save1 = cbind(save1,data.frame("habite perte vue" = c("")))
        save1 = cbind(save1,data.frame("visibilite" = c("")))
        save1 = cbind(save1,data.frame("nb_public" = c("")))
        save1 = cbind(save1,data.frame("eurodeer_lacher" = c("")))
        save1 = cbind(save1,data.frame("remise sabot" = c("")))
        save1 = cbind(save1,data.frame("heure_lacher_2" = c("")))
        save1<<-save1
        ######mise a jour cap_bague bd
        if (input$nAnimal2 != "") {
          if (startsWith(input$nAnimal2, "F")){
            faon <-  TRUE }
          else {faon <- FALSE}
        
        
        if (faon == TRUE) {
          if   (save1[dim(save1)[1],"cap_tag_droit"] !=""){
            cap_bague2 <<- paste0("F", save1[dim(save1)[1],"cap_tag_droit"], "_", str_sub(annee_suivie, -2)) } else{cap_bague2 <<- paste0("F", save1[dim(save1)[1],"cap_tag_gauche"], "_", str_sub(annee_suivie, -2))}
        } else {
          if   (save1[dim(save1)[1],"cap_tag_droit"] !=""){
            cap_bague2 <<- paste0(save1[dim(save1)[1],"cap_tag_droit"], "_", str_sub(annee_suivie, -2)) } else{cap_bague2 <<- paste0(save1[dim(save1)[1],"cap_tag_gauche"], "_", str_sub(annee_suivie, -2))}
        }
        ani_id<- as.character(dbGetQuery(con, paste0("select ani_id from public.t_animal_ani where ani_etiq= '",input$nAnimal2,"'"))[1,1])
        cap_id<- as.integer(as.character(dbGetQuery(con, paste0("SELECT cap_id FROM t_capture_cap WHERE cap_ani_id = ",ani_id," order by cap_id DESC"))[1,1]))
        
        
        if (exists("cap_id")) {
          req<-paste0("UPDATE public.t_capture_cap SET cap_bague = '",cap_bague2,"' where  cap_id= ",cap_id," ")
          req = gsub("'NA'","NULL", req)
          req = gsub("''","NULL", req)
          
          dbSendQuery(con, req) 
        
        if (length(grep("temperature anale",mes)) != 0){
          dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_tble_temp_animal = '",gsub("temperature anale_","",grep("temperature anale",mes, value =TRUE)),"' where cpt_cap_id = ",cap_id," "))
        }
        if (length(grep("temperature ext",mes)) != 0){
          dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_tble_temp_exterieur = '",gsub("temperature ext_","",grep("temperature ext",mes, value =TRUE)),"' where cpt_cap_id = ",cap_id," "))
        }
        if (length(grep("rythme cardiaque",mes)) != 0){
          dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_tble_coeur = '",gsub("rythme cardiaque-","",grep("rythme cardiaque",mes, value =TRUE)),"' where cpt_cap_id = ",cap_id," "))
        }
        if (length(grep("ventilation",mes)) != 0){
          dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_tble_ventilation = '",gsub("ventilation-","",grep("ventilation",mes, value =TRUE)),"' where cpt_cap_id = ",cap_id," "))
        }}
        
      }
      
      save1<-as.matrix(save1)
      save1[dim(save1)[1],"N..bague.annee.capture"]<- c(cap_bague2)
      save1<-as.data.frame(save1)
      }
      
      setwd(tosd)
      write.table(save1 , file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), append = TRUE, col.names=!file.exists(paste0("captures_",gsub("-","_",Sys.Date()), ".csv")), na="", row.names = F, sep=";")
      
      setwd(tousb)
      write.table(save1 , file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), append = TRUE, col.names=!file.exists(paste0("captures_",gsub("-","_",Sys.Date()), ".csv")), na="", row.names = F, sep=";")
      setwd(tosd)
      if (exists("prelevement")) {rm(list = c("prelevement"), envir = .GlobalEnv)}
    }}
  
  ##################           CSV CHECKLIST 2                                      #####     
  
  modalCallback_check2 = function  (value2) {
    if (value2 == TRUE) {
      
      setwd(tosd)
      fichier_lu <- read.table(file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), sep = ";", na.strings = "", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character"))


      noms_colonnes<- c("N°Animal","ani_nom","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_pertinent","cap_lactation","RFID","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit","glucose","T°C_ext","TIQUES FIXES","autres parasites", "Peau","poils","sang","feces","tiques","vaginal","Nasal","remarque","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)","num_sabot","couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2","ventilation","remarque_table","localisation sonde temperature","eurodeer","titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque_generale","bague","autre","stop","habitat lacher","habitat perte vue","visibilite","nb_public","eurodeer_lacher","remise sabot","hre_lacher_2")
      colnames(fichier_lu)<- noms_colonnes


      remarque_tot = paste0(input$remarques_capt, input$remarque_collier, input$remarques_table, input$remarques_lacher, sep = "~")

      if ((input$time2)!=""){
        remise_sabot = 1}
      else {remise_sabot = ""}

      if (input$nAnimal != "") {
        select_line = which(fichier_lu[1]==(input$nAnimal),arr.ind=TRUE)[1] }
      if (input$nAnimal2 != "") {
        select_line = which(fichier_lu[1]==(input$nAnimal2),arr.ind=TRUE)[1] }

      ####column to fill
      col_concerned<- c("titube (1/0)","couche (1/0)", "course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","stop","habitat lacher","habitat perte vue","visibilite","nb_public","eurodeer_lacher")
      ####reactive values names
      reactive_values <- c("titube" ,"couche","vitesse","tombe","gratte_collier","cabriole_saut","allure","cri","nbre_stops","habitat","habitat_perte","visibilite","nbre_personnes","Notation_euro")
      ####loop to fill each column with corresponding reactive_value

      for (i in 1: length(col_concerned)){
        if (!is.null(input[[reactive_values[i]]])) {
          fichier_lu[select_line,col_concerned[i]]<- as.character(input[[reactive_values[i]]])
        } else {
          fichier_lu[select_line, col_concerned[i]]<- c("")}}

      #####fill the last columns automatically

      fichier_lu[select_line,"lache"]<- input$time
      fichier_lu[select_line,"hre_lacher_2"]<- input$time2
      fichier_lu[select_line,"remise sabot"]<- remise_sabot
      fichier_lu[select_line,"remarque_generale"]<- remarque_tot

      if (input$numSabot != "") {dbSendQuery(con, paste0("update listes.tr_sabots_sab set sab_disponible = FALSE where sab_valeur =",input$numSabot,""))}
      if (exists ("collier_id_usuel") && collier_id_usuel != "") {dbSendQuery(con,paste0("UPDATE public.t_equipement_conf_eqc SET eqc_pose = TRUE WHERE eqc_eqt_id in (select eqc_eqt_id  from t_equipement_conf_eqc, t_equipement_eqt where eqc_eqt_id = eqt_id and eqt_id_usuel = '",collier_id_usuel,"') ")); rm(list = c("collier_id_usuel"), envir = .GlobalEnv)}

      setwd(tosd)
      write.table(fichier_lu , file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), append = FALSE, col.names= file.exists(paste0("captures_",gsub("-","_",Sys.Date()), ".csv")), na="", row.names = F, sep=";")

      setwd(tousb)
      write.table(fichier_lu , file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), append = FALSE, col.names=!file.exists(paste0("captures_",gsub("-","_",Sys.Date()), ".csv")), na="", row.names = F, sep=";")
      setwd(tosd)
      
      ##################           CSV CHECKLIST 2 refresh                #####
      text_input<-c("time_caract", "time_table","time","time2","idTagOrD", "idTagOrG","lBoisDroit","lBoisGauche", "liste_blessures","nAnimal", "nom_capteur_txt", "Observateur","parasites", "remarque_ani","remarque_collier","Remarques","remarques_ble","remarques_capt","remarques_lacher","remarques_prel","remarques_table", "rfid_erase")
      numeric_input<-c("tglucose","cirCou","lPattArriere", "pSabotPlein","pSabotVide", "nbre_stops")
      #awe_radio_input<-c("sexe")
      radio_input<-c("lutte", "halete", "titube" ,"couche","tombe","gratte_collier","cabriole_saut","cri")
      check_input<-c("metal_tag_d","metal_tag_d2","metal_tag_g","metal_tag_g2","newRFIDbox","newTagD","newTagG", "suivi_temp", "create_ind", "create_cap")
      select_input<-c("visibilite", "habitat", "habitat_perte","tiques","diarrhee","age", "idTagOrD2","idTagOrG2","lactation","nAnimal2","Nbre_pers_experimentes","nbre_personnes","Notation_euro", "Notation_euro_table","numSabot_capture") #,"idRFID", "idRFID2"
      
      updateRadioButtons(session,"estNouvelAnimal", selected = "oui")
      updateRadioButtons(session,"identifie", selected = "non")
      updateSelectInput(session,"idSite2", selected = input$idSite2)
      updateSelectInput(session,"idSite", selected = input$idSite)
      updateRadioButtons(session,"cribague", choices = choix[["cribague"]], selected = "NA")
      updateRadioButtons(session,"criautre", choices = choix[["criautre"]], selected = "NA")
      updateRadioButtons(session,"vitesse", choiceNames = choix[["vitesse"]],choiceValues = choix[["values_vitesse"]], selected = "NA")
      updateRadioButtons(session,"allure", choiceNames = choix[["allure"]],choiceValues = choix[["values_allure"]], selected = "NA")
      updateSelectInput(session,"typetype", choices = unique(df_prelevement$prel_type),selected = "")
      updateSelectInput(session,"numSabot", choices = c(choisir = "", dbGetQuery(con,"select sab_valeur from listes.tr_sabots_sab where sab_disponible is not FALSE order by sab_id")[,1]), selected = NULL)
      updateSelectInput(session,"sexe",selected = "")
      updateSelectInput(session,"mes",selected= "temperature ext_ysi")
      updateNumericInput(session,"Val_mes",value = NA)
      updateTextInput(session, "rem_mes_heure", value = "", placeholder = "Remarque")
      
      updateSelectizeInput(session, "idRFID2", choices = c(choisir = "", dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id")[,1]), selected = "")
      updateSelectInput(session, "idRFID", choices = c(choisir = "", dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null")[,1]), selected = "")

      for (i in 1:length(text_input)){
        updateTextInput(session, text_input[i], value = NA)}
      for (i in 1:length(numeric_input)){
        updateNumericInput(session, numeric_input[i], value = NA)}
      for (i in 1:length(radio_input)){
        updateRadioButtons(session, radio_input[i], choiceNames = choix[["names_oui_non"]],choiceValues =choix[["values_oui_non"]], selected = "NA")}
      for (i in 1:length(check_input)){
        updatePrettyCheckbox (session, check_input[i], value = FALSE)}
      for (i in 1:length(select_input)){
        updateSelectInput(session, select_input[i], choices = choix[[select_input[i]]], selected = NULL)}
      
      ###effacement du tableau de prelevements
      prelevement <<- prelevement[-as.numeric(input$tableprelevement_rows_selected),]
      output$tableprelevement = DT::renderDT(prelevement,server = F)
      ###effacement des blessures
      blessure <<- blessure[-as.numeric(input$tableblessure_rows_selected),]
      #blessure <- data.frame()
      output$tableblessure = DT::renderDT(blessure,server = F)
      ####deselection du collier
      proxy <- dataTableProxy("tablecollier",session, deferUntilFlush = FALSE)
      reloadData(proxy, resetPaging = TRUE, clearSelection = c("all"))
      ###effacement des mesures avec heure
      mes_heure <<- mes_heure[-as.numeric(input$table_mes_heure_rows_selected),]
      output$table_mes_heure = DT::renderDT(mes_heure,server = F)
      ###effacement des booleens
      bool <<- bool[-as.numeric(input$table_bool_rows_selected),]
      output$table_bool = DT::renderDT(bool,server = F)
      
      ####remise a jour de la ligne texte de selection du collier
      output$collier_choisi = renderText("")
      if (exists ("cap_id")) {rm(list = c("cap_id"), envir = .GlobalEnv)}
      if (exists ("cap_bague2")) {rm(list = c("cap_bague2"), envir = .GlobalEnv)}
      if (exists ("find_ani_id")) {rm(list = c("find_ani_id"), envir = .GlobalEnv)}
      if (exists ("fi")) {rm(list = c("fi"), envir = .GlobalEnv)}
      if (exists ("test_checklist1_validated")) {rm(list = c("test_checklist1_validated"), envir = .GlobalEnv)}
      #if (exists ("liste_prelevement")) {rm(list = c("liste_prelevement"), envir = .GlobalEnv)}
    }}
  
  ##################           CSV CHECKLIST 3                                      #####     
  
  
  modalCallback_check3 = function(value) {
    if (value == TRUE) {
      fichier_lu2 <- read.table(file = paste0("captures_",gsub("-","_",input$date_capture), ".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character"))
       noms_colonnes<- c("N°Animal","ani_nom","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_pertinent","cap_lactation","RFID","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit","glucose","T°C_ext","TIQUES FIXES","autres parasites", "Peau","poils","sang","feces","tiques","vaginal","Nasal","remarque","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)","num_sabot","couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2","ventilation","remarque_table","localisation sonde temperature","eurodeer","titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque_generale","bague","autre","stop","habitat lacher","habitat perte vue","visibilite","nb_public","eurodeer_lacher","remise sabot","hre_lacher_2")
      colnames(fichier_lu2)<- noms_colonnes
      
      if (input$nAnimal != "") {
        select_line = which(fichier_lu2[1]==(input$nAnimal),arr.ind=TRUE)[1] }
      if (input$nAnimal2 != "") {
        select_line = which(fichier_lu2[1]==(input$nAnimal2),arr.ind=TRUE)[1] }
      

      if ((fichier_lu2[select_line, "hre_lacher_2"] != "")){
      cpt_heure_debut_filet<- paste0("",fi," ",substring(input$cpt_heure_debut_filet, 12,25)," CEST")
      heure_second_lacher<- paste0("",fi," ",fichier_lu2[select_line, "hre_lacher_2"]," CEST")
      duree_totale <-  hour_conv(as.numeric(difftime(strptime(heure_second_lacher, "%Y-%m-%d %H:%M:%S"), strptime(input$cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S"), units = "hours")))
      } else {
      cpt_heure_debut_filet<- paste0("",fi," ",substring(input$cpt_heure_debut_filet, 12,25)," CEST")  
      heure_lacher<- paste0("",fi," ",as.character(fichier_lu2[select_line, "lache"])," CEST");
      duree_totale <-  hour_conv(as.numeric(difftime(strptime(heure_lacher, "%Y-%m-%d %H:%M:%S"), strptime(cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S"), units = "hours")))}
      surveillance<-as.numeric(difftime(strptime(input$cpt_heure_fin_surv, "%Y-%m-%d %H:%M:%S"), strptime(input$cpt_heure_mise_sabot, "%Y-%m-%d %H:%M:%S"), units = "min"))

      ####column to fill
      col_concerned<- c("nom capteur", "nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)"
                        ,"lutte","haletement (1/0)","cri (1/0)","filet","acepromazine (1=0,3cc)"
                        ,"couche (1/0)","agitation (1/0)","retournement (1/0)")
      ####reactive values names
      reactive_values <- c("nom_capteur_txt","Nbre_pers_experimentes","cpt_filet_vitesse","cpt_filet_allure"
                           ,"cpt_filet_lutte","cpt_filet_halete","cpt_filet_cri","cpt_temps_filet","cpt_dose_acepromazine"
                           ,"cpt_sabot_couche","cpt_sabot_agitation","cpt_sabot_retournement")
      ####loop to fill each column with corresponding reactive_value
      
      for (i in 1: length(col_concerned)){
        if (length(as.character(input[[reactive_values[i]]])) != 0) {
          if (reactive_values[i] == "nom_capteur_txt") {fichier_lu2[select_line,col_concerned[i]]<- toupper(input[[reactive_values[i]]])} else {fichier_lu2[select_line,col_concerned[i]]<- input[[reactive_values[i]]]}
        } else {
          fichier_lu2[select_line, col_concerned[i]]<- c("")}}
      
      #####add fill the last column automatically 
      #print(duree_totale)
      fichier_lu2[select_line,"total"]<- duree_totale[1]
      fichier_lu2[select_line,"surveillance (mn)"]<- surveillance
      fichier_lu2[select_line,"hre fin surv"]<- substring(input$cpt_heure_fin_surv, 12,19)
      fichier_lu2[select_line,"capture"]<- substring(input$cpt_heure_debut_filet, 12,19)
      fichier_lu2[select_line,"sabot"]<- substring(input$cpt_heure_mise_sabot, 12,19)
      fichier_lu2[select_line,"acepro"]<- substring(input$cpt_heure_mise_sabot, 12,19)

      write.table(fichier_lu2 , file = paste0("captures_",gsub("-","_",input$date_capture), ".csv"), append = FALSE, col.names= TRUE, na="", row.names = F, sep=";")
      # setwd(tousb)
      # write.table(fichier_lu2 , file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), append = FALSE, col.names=file.exists(paste0("captures_",gsub("-","_",Sys.Date()), ".csv")), na="", row.names = F, sep=";")
      setwd(tosd)
      
      ##################           CSV CHECKLIST 3 refresh                #####
      text_input<-c("remarques_capt", "nom_capteur_txt","Remarques","Observateur","time_caract", "time_table","time","time2","idTagOrD", "idTagOrG","lBoisDroit","lBoisGauche", "liste_blessures","nAnimal", "nom_capteur_txt", "Observateur","parasites", "remarque_ani","remarque_collier","Remarques","remarques_ble","remarques_capt","remarques_lacher","remarques_prel","remarques_table", "rfid_erase")
      numeric_input<-c("cpt_temps_filet","tglucose","cirCou","lPattArriere", "pSabotPlein","pSabotVide", "nbre_stops")
      radio_input<-c("cpt_filet_cri", "cpt_filet_halete", "cpt_sabot_agitation","cpt_sabot_couche", "cpt_sabot_retournement", "lutte", "halete", "titube" ,"couche","tombe","gratte_collier","cabriole_saut","cri")
      check_input<-c("metal_tag_d","metal_tag_d2","metal_tag_g","metal_tag_g2","newRFIDbox","newTagD","newTagG", "suivi_temp")
      select_input<-c("Nbre_pers_experimentes", "cpt_dose_acepromazine","visibilite", "habitat", "habitat_perte","tiques","diarrhee","age","idRFID", "idRFID2", "idTagOrD2","idTagOrG2","lactation","nAnimal2","Nbre_pers_experimentes","nbre_personnes","Notation_euro", "Notation_euro_table","numSabot","numSabot_capture")
      
      updateRadioButtons(session,"estNouvelAnimal", selected = "oui")
      updateRadioButtons(session,"identifie", selected = "non")
      updateSelectInput(session,"idSite2", selected = input$idSite2)
      updateSelectInput(session,"idSite", selected = input$idSite)
      updateRadioButtons(session,"cribague", choices = choix[["cribague"]], selected = "NA")
      updateRadioButtons(session,"criautre", choices = choix[["criautre"]], selected = "NA")
      updateRadioButtons(session,"vitesse", choiceNames = choix[["vitesse"]],choiceValues = choix[["values_vitesse"]], selected = "NA")
      updateRadioButtons(session,"allure", choiceNames = choix[["allure"]],choiceValues = choix[["values_allure"]], selected = "NA")
      updateRadioButtons(session,"cpt_filet_vitesse", choiceNames = choix[["vitesse"]],choiceValues = choix[["values_vitesse"]], selected = "NA")
      updateRadioButtons(session,"cpt_filet_allure", choiceNames = choix[["allure"]],choiceValues = choix[["values_allure"]], selected = "NA")
      updateRadioButtons(session,"cpt_filet_lutte", choices =choix[["cpt_filet_lutte"]], selected = "")
      updateTimeInput(session, "cpt_heure_fin_surv", value = strptime("00:00",format = "%H:%M"))
      updateTimeInput(session, "cpt_heure_debut_filet", value = strptime("00:00",format = "%H:%M"))
      updateTimeInput(session, "cpt_heure_mise_sabot", value = strptime("00:00",format = "%H:%M"))
      updateSelectInput(session,"mes",selected= "")
      updateNumericInput(session,"Val_mes",value = "")
      updateTextInput(session, "rem_mes_heure", value = "", placeholder = "Remarque")
      updateSelectInput(session, "numSabot_capture", selected = "")
      
      for (i in 1:length(text_input)){
        updateTextInput(session, text_input[i], value = NA, placeholder = "Entrez un texte :")}
      for (i in 1:length(numeric_input)){
        updateNumericInput(session, numeric_input[i], value = NA)}
      for (i in 1:length(radio_input)){
        updateRadioButtons(session, radio_input[i], choiceNames = choix[["names_oui_non"]],choiceValues =choix[["values_oui_non"]], selected ="NA")}
      for (i in 1:length(check_input)){
        updateCheckboxInput(session, check_input[i], value = FALSE)}
      for (i in 1:length(select_input)){
        updateSelectInput(session, select_input[i], choices = choix[[select_input[i]]], selected = NULL)}
      
      ###effacement du tableau de prelevements
      prelevement <<- prelevement[-as.numeric(input$tableprelevement_rows_selected),]
      output$tableprelevement = DT::renderDT(prelevement,server = F)
      ###effacement des blessures
      blessure <<- blessure[-as.numeric(input$tableblessure_rows_selected),]
      output$tableblessure = DT::renderDT(blessure,server = F)
      ###effacement des mesures avec heure
      mes_heure <<- mes_heure[-as.numeric(input$table_mes_heure_rows_selected),]
      output$table_mes_heure = DT::renderDT(mes_heure,server = F) 
      ####deselection du collier
      proxy <- dataTableProxy("tablecollier",session, deferUntilFlush = FALSE)
      reloadData(proxy, resetPaging = TRUE, clearSelection = c("all"))
      ####remise a jour de la ligne texte de selection du collier
      output$collier_choisi = renderText("")
      ####remise à jour du poids 
      output$poids_ani<-renderText(NA)
      #rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
      #con<- dbConnect(PostgreSQL(), host="localhost", dbname="db_chevreuils", user="postgres", password="postgres")
      

    }}
  
  
  
  ##################           BASE DE DONNEES                                      ####
  observe({
    ########################### Nouvel animal                                       ####
    ######creation d''une entree dans t_animal_ani
    if (exists ("cap_id")) {rm(list = c("cap_id"), envir = .GlobalEnv)}
    if (exists ("cap_bague2")) {rm(list = c("cap_bague2"), envir = .GlobalEnv)}
    if (exists ("find_ani_id")) {rm(list = c("find_ani_id"), envir = .GlobalEnv)}
    
    date_mod <- as.character(input$date_caract)
    annee <- strsplit(date_mod, "-")[[1]][1]
    jour <- strsplit(date_mod, "-")[[1]][3]
    mois <- strsplit(date_mod, "-")[[1]][2]
    
    if (as.integer(mois)>=10) {
      annee_suivie <- as.integer(annee) + 1  }
    if (as.integer(mois)<10) {annee_suivie <- annee}
    
    cap_bague <- paste0("_", str_sub(annee_suivie, -2))
    
    ############## Capture d''un animal ou Recap d''un ind non identifié            #########

    
    if (input$estNouvelAnimal == 'oui' && input$date_capture =="choisir" || input$estNouvelAnimal == 'non' && input$identifie == 'non' &&  input$date_capture =="choisir") {
  
        onclick("cre_ind",
        if (input$nAnimal != "" && nchar(as.character(input$nAnimal)) >= 4 && nchar(as.character(input$nAnimal)) < 6 && input$sexe != ""){
          dbSendQuery(con,paste0("INSERT INTO public.t_animal_ani( ani_etiq, ani_sexe, ani_date_mort_arrondi, ani_poids_mort_na) values ('", input$nAnimal,"', '",input$sexe,"', FALSE, FALSE) ON CONFLICT (ani_etiq) DO UPDATE SET (ani_sexe) = (excluded.ani_sexe)"))
          updateCheckboxInput(session,"create_ind", value = TRUE)
          updateTextInput (session, "idTagOrD", value = input$nAnimal)
          updateTextInput (session, "idTagOrG", value = input$nAnimal)
                  })
      
        if (input$create_ind == TRUE){  
        if (input$estNouvelAnimal == 'non' && input$identifie == 'non') {
          dbSendQuery(con, paste0("INSERT into public.t_correspondance_animal_cor (cor_ancien, cor_valide, cor_updated) VALUES ('",input$nAnimal,"','",input$nAnimal,"',FALSE) ON CONFLICT (cor_ancien) DO UPDATE SET (cor_valide)=(excluded.cor_valide)"))}
        ######creation d''une entree dans t_capture_cap
        ## 

        if (input$nAnimal != "" && nchar(as.character(input$nAnimal)) >= 4 && nchar(as.character(input$nAnimal)) < 6 && input$idSite != "" && as.character(input$date_caract) != "" && !is.null(input$sexe)){
          
          find_ani_id <<- dbGetQuery(con, paste0("select ani_id from public.t_animal_ani where ani_etiq= '",input$nAnimal,"'"))[1,1]
          find_site_id <- dbGetQuery(con, paste0("select sit_id from public.tr_site_capture_sit where sit_nom_court= '",input$idSite,"'"))[1,1]
          
          faon<- FALSE
          
          
          if (!is.na(input$pSabotPlein - input$pSabotVide)){
            req<- paste0("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_date, cap_annee_suivi, cap_faon, cap_poids, cap_num_sabot, cap_bague, cap_tag_droit, cap_tag_droit_metal, cap_tag_gauche, cap_tag_gauche_metal) 
                         values (",as.integer(find_ani_id),", '",find_site_id,"', '",as.Date(input$date_caract),"', '",annee_suivie,"', '",faon,"', '",(input$pSabotPlein - input$pSabotVide),"','",input$numSabot,"', '",cap_bague,"', '",input$idTagOrD,"', '",input$metal_tag_d,"', '",input$idTagOrG,"', '",input$metal_tag_g,"')
                         ON CONFLICT (cap_ani_id, cap_date) DO UPDATE SET (cap_sit_id, cap_annee_suivi, cap_faon, cap_poids, cap_num_sabot, cap_bague, cap_tag_droit, cap_tag_droit_metal, cap_tag_gauche, cap_tag_gauche_metal) = (excluded.cap_sit_id, excluded.cap_annee_suivi, excluded.cap_faon, excluded.cap_poids, excluded.cap_num_sabot, excluded.cap_bague, excluded.cap_tag_droit, excluded.cap_tag_droit_metal, excluded.cap_tag_gauche, excluded.cap_tag_gauche_metal) RETURNING cap_id")
          } else {
            req<- paste0("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_date, cap_annee_suivi, cap_faon, cap_num_sabot, cap_bague,  cap_tag_droit, cap_tag_droit_metal, cap_tag_gauche, cap_tag_gauche_metal) 
                         values (",as.integer(find_ani_id),", '",find_site_id,"', '",as.Date(input$date_caract),"', '",annee_suivie,"', '",faon,"','",input$numSabot,"',  '",cap_bague,"', '",input$idTagOrD,"', '",input$metal_tag_d,"', '",input$idTagOrG,"', '",input$metal_tag_g,"')
                         ON CONFLICT (cap_ani_id, cap_date) DO UPDATE SET (cap_sit_id, cap_annee_suivi, cap_faon, cap_num_sabot, cap_bague, cap_tag_droit, cap_tag_droit_metal, cap_tag_gauche, cap_tag_gauche_metal) = (excluded.cap_sit_id, excluded.cap_annee_suivi, excluded.cap_faon, excluded.cap_num_sabot, excluded.cap_bague, excluded.cap_tag_droit, excluded.cap_tag_droit_metal, excluded.cap_tag_gauche, excluded.cap_tag_gauche_metal) RETURNING cap_id")
          }
          
          req = gsub("'NA'","NULL", req)
          req = gsub("''","NULL", req)
          
          cap_id<<-dbGetQuery(con,req)
          }}####fin creation entree dans t_capture_cap
      
        if (input$idTagOrD != "" && exists("cap_id") && !is.null(cap_id)) {
          req<-paste0("UPDATE public.t_capture_cap SET (cap_tag_droit, cap_tag_droit_metal) = ('",input$idTagOrD,"',",input$metal_tag_d,") where  cap_id= ",cap_id," ")
          
          req = gsub("'NA'","NULL", req)
          req = gsub("''","NULL", req)
          
          dbSendQuery(con, req)
          
          cap_bague <- paste0("",input$idTagOrD,"_", str_sub(annee_suivie, -2))
          dbSendQuery(con,paste0("UPDATE public.t_capture_cap set cap_bague = '",cap_bague,"' where cap_id = '",cap_id,"' "))
        
        }
        
        if (input$idTagOrG != "" && exists("cap_id") && !is.null(cap_id)) {
          req<-paste0("UPDATE public.t_capture_cap SET (cap_tag_gauche, cap_tag_gauche_metal) = ('",input$idTagOrG,"',",input$metal_tag_g,") where  cap_id= ",cap_id," ")
          
          req = gsub("'NA'","NULL", req)
          req = gsub("''","NULL", req)
          
          dbSendQuery(con, req)
          
          if (cap_bague == paste0("_", str_sub(annee_suivie, -2))){
            cap_bague <- paste0("",input$idTagOrG,"_", str_sub(annee_suivie, -2))
            dbSendQuery(con,paste0("UPDATE public.t_capture_cap set cap_bague = '",cap_bague,"' where cap_id = '",cap_id,"' "))
          }
        }

        if ((input$idRFID != "" && length(input$idRFID) != 0) && exists("cap_id")) {
          if (!is.null(cap_id)){
          req<-paste0("INSERT INTO public.t_rfid_rfi (rfi_cap_id, rfi_tag_code) VALUES (",as.numeric(cap_id),",'",input$idRFID,"') ON CONFLICT (rfi_cap_id, rfi_tag_code) DO NOTHING")
          #req<-paste0("UPDATE public.t_rfid_rfi SET rfi_cap_id =",as.numeric(cap_id)," where rfi_tag_code = '",input$idRFID,"' ")
          
          req = gsub("'NA'","NULL", req)
          req = gsub("''","NULL", req)
          
          dbSendQuery(con, req)
        }}
      
       if (!is.na(input$pSabotPlein - input$pSabotVide) && exists("cap_id") && !is.null(cap_id)) {
        req<-paste0("UPDATE t_capture_cap SET cap_poids = '",input$pSabotPlein - input$pSabotVide,"' where cap_id = ",as.numeric(cap_id)," ")
        
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        dbSendQuery(con, req)
       }
      
      if (input$numSabot != "" && input$numSabot != "" && exists("cap_id") && !is.null(cap_id)) {
        req<-paste0("UPDATE t_capture_cap SET cap_num_sabot = '",input$numSabot,"' where cap_id = ",as.numeric(cap_id)," ")
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        dbSendQuery(con, req)
      }
      
      
        
        } ####### fin capture d''un animal
    ############## Recapture d''un animal identifié                                 ####
    
    if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$date_capture =="choisir") {
      
      if (input$nAnimal2 != "" && input$idSite2 != "" && as.character(input$date_caract) != "" && !is.null(input$sexe) && input$sexe != ""){
        
        find_ani_id <<- dbGetQuery(con, paste0("select ani_id from public.t_animal_ani where ani_etiq= '",input$nAnimal2,"'"))[1,1]
        find_site_id <- dbGetQuery(con, paste0("select sit_id from public.tr_site_capture_sit where sit_nom_court= '",input$idSite2,"'"))[1,1]
        
        
        if (input$nAnimal2 != "") {
          if (startsWith(input$nAnimal2, "F")){
            faon =  TRUE }
          else {faon= FALSE} }
        
        cap_bague = paste0("_", str_sub(annee_suivie, -2))
        
        ####ici je teste si le poids est pas renseigné et si le cap_bague existe déjà car dans le cas d'une remise en sabot (pour le poids) et de la seconde validation csv2 (pour le cap_bague), il ne faut pas écraser le poids précédemment calculé et le cap_bague donné       
        if (!is.na(input$pSabotPlein - input$pSabotVide)){
          req<- paste0("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_date, cap_annee_suivi, cap_faon, cap_poids, cap_num_sabot, cap_bague, cap_tag_droit, cap_tag_droit_metal, cap_tag_gauche, cap_tag_gauche_metal) 
                       values (",as.integer(find_ani_id),", '",find_site_id,"', '",as.Date(input$date_caract),"', '",annee_suivie,"', '",faon,"', '",(input$pSabotPlein - input$pSabotVide),"','",input$numSabot,"', '",cap_bague,"', '",input$idTagOrD,"', '",input$metal_tag_d,"', '",input$idTagOrG,"', '",input$metal_tag_g,"')
                       ON CONFLICT (cap_ani_id, cap_date) DO UPDATE SET (cap_sit_id, cap_annee_suivi, cap_faon, cap_bague) = (excluded.cap_sit_id, excluded.cap_annee_suivi, excluded.cap_faon, excluded.cap_bague) RETURNING cap_id")
          #updateCheckboxInput(session,"create_cap", value = TRUE)
        } else {
          req<- paste0("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_date, cap_annee_suivi, cap_faon, cap_num_sabot, cap_bague,  cap_tag_droit, cap_tag_droit_metal, cap_tag_gauche, cap_tag_gauche_metal) 
                       values (",as.integer(find_ani_id),", '",find_site_id,"', '",as.Date(input$date_caract),"', '",annee_suivie,"', '",faon,"','",input$numSabot,"',  '",cap_bague,"', '",input$idTagOrD,"', '",input$metal_tag_d,"', '",input$idTagOrG,"', '",input$metal_tag_g,"')
                       ON CONFLICT (cap_ani_id, cap_date) DO UPDATE SET (cap_sit_id, cap_annee_suivi, cap_faon, cap_bague) = (excluded.cap_sit_id, excluded.cap_annee_suivi, excluded.cap_faon, excluded.cap_bague) RETURNING cap_id")
          #UPDATE SET (cap_sit_id, cap_annee_suivi, cap_faon, cap_num_sabot, cap_bague, cap_tag_droit, cap_tag_droit_metal, cap_tag_gauche, cap_tag_gauche_metal) = (excluded.cap_sit_id, excluded.cap_annee_suivi, excluded.cap_faon, excluded.cap_num_sabot, excluded.cap_bague, excluded.cap_tag_droit, excluded.cap_tag_droit_metal, excluded.cap_tag_gauche, excluded.cap_tag_gauche_metal) RETURNING cap_id")
          #updateCheckboxInput(session,"create_cap", value = TRUE)
        }
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        cap_id<<-dbGetQuery(con,req)
      
        } ###fin de la creation de l''entree dans t_capture_cap
      
      if (!is.na(input$pSabotPlein - input$pSabotVide) && exists("cap_id")){
        req<-paste0("UPDATE public.t_capture_cap SET cap_poids = (",(input$pSabotPlein - input$pSabotVide),") where  cap_id= ",cap_id," ")
        
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        dbSendQuery(con, req)        
      }
      
      if (input$idTagOrD2 != "" && input$newTagD == FALSE && exists("cap_id") && !is.null(cap_id)) {
        req<-paste0("UPDATE public.t_capture_cap SET (cap_tag_droit, cap_tag_droit_metal) = ('",input$idTagOrD2,"',",input$metal_tag_d,") where  cap_id= ",cap_id," ")
        
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        dbSendQuery(con, req)
        
        if (startsWith(input$nAnimal2, "F")){
        cap_bague <- paste0("F",input$idTagOrD2,"_", str_sub(annee_suivie, -2))} else {cap_bague <- paste0("",input$idTagOrD2,"_", str_sub(annee_suivie, -2))}
        dbSendQuery(con,paste0("UPDATE public.t_capture_cap set cap_bague = '",cap_bague,"' where cap_id = '",cap_id,"' "))
        
      }
      
      if (input$idTagOrG2!= "" && input$newTagG == FALSE && exists("cap_id") && !is.null(cap_id)){
        req<-paste0("UPDATE public.t_capture_cap SET (cap_tag_gauche, cap_tag_gauche_metal) = ('",input$idTagOrG2,"',",input$metal_tag_g2,") where  cap_id= '",cap_id,"' ")
        
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        dbSendQuery(con, req)
        
      }                
      
      if (input$newTagG == TRUE && !is.null(input$idTagOrG3) && exists("cap_id") && !is.null(cap_id)) {
        req<-paste0("UPDATE public.t_capture_cap SET (cap_tag_gauche, cap_tag_gauche_metal) = ('",input$idTagOrG3,"',",input$metal_tag_g3,") where  cap_id= ",cap_id," ")
        
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        dbSendQuery(con, req)
        
      } 
      
      if (input$newTagD == TRUE && !is.null(input$idTagOrD3) && exists("cap_id") && !is.null(cap_id)) {
        
        req<-paste0("UPDATE public.t_capture_cap SET (cap_tag_droit, cap_tag_droit_metal) = ('",input$idTagOrD3,"',",input$metal_tag_d3,") where  cap_id= ",cap_id," ")
        
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        dbSendQuery(con, req)
        
        if (startsWith(input$nAnimal2, "F")){
          cap_bague <- paste0("F",input$idTagOrD3,"_", str_sub(annee_suivie, -2))} else {cap_bague <- paste0("",input$idTagOrD3,"_", str_sub(annee_suivie, -2))}
        dbSendQuery(con,paste0("UPDATE public.t_capture_cap set cap_bague = '",cap_bague,"' where cap_id = '",cap_id,"' "))
        
      }
      
      if (input$nAnimal2!="" && exists("cap_id") && !is.null(cap_id) && exists("annee_suivie")) {
        cap_pert = dbGetQuery(con,paste0("select cap_annee_suivi, cap_date from public.t_capture_cap, public.t_animal_ani where cap_ani_id=ani_id and ani_etiq = '",input$nAnimal2,"' order by cap_annee_suivi DESC"))[1,]
        cap_annee <- as.character(cap_pert[1,1])
        cap_date <- as.Date(cap_pert[1,2])
        if (cap_date != as.Date(input$date_caract) && annee_suivie == cap_annee) {cap_pertinent = FALSE} else {cap_pertinent = TRUE} 
        req<-paste0("UPDATE public.t_capture_cap SET (cap_pertinent) = (",cap_pertinent,") where  cap_id= '",cap_id,"' ")
        
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        dbSendQuery(con, req)
      }
      
      if (input$idSite2 != "" && exists("cap_id") && !is.null(cap_id)) {
        
        find_site_id <- dbGetQuery(con, paste0("select sit_id from public.tr_site_capture_sit where sit_nom_court= '",input$idSite2,"'"))[1,1]
        
        req<-paste0("UPDATE public.t_capture_cap SET (cap_sit_id) = (",find_site_id,") where  cap_id = (",cap_id,") ")
        
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        dbSendQuery(con, req)
      }
      
      if (input$newRFIDbox == FALSE && (input$idRFID2 != "" && length(input$idRFID) != 0) && !is.null(input$idRFID2) && exists("cap_id") && !is.null(cap_id)) {
        req<-paste0("INSERT INTO public.t_rfid_rfi (rfi_cap_id, rfi_tag_code) VALUES (",as.numeric(cap_id),",'",input$idRFID2,"') ON CONFLICT DO NOTHING")
        
        req = gsub("'NA'","NULL", req)
        req = gsub("''","NULL", req)
        
        dbSendQuery(con, req)
      }
      
      if (input$newRFIDbox == TRUE && (!is.null(input$idRFID_new) && length(input$idRFID) != 0) && exists("cap_id") && !is.null(cap_id)) {
        if (input$idRFID_new != ""){
          dbSendQuery(con,paste0("DELETE FROM public.t_rfid_rfi WHERE rfi_cap_id = ",as.numeric(cap_id)," "))
          req<-paste0("INSERT INTO public.t_rfid_rfi (rfi_cap_id, rfi_tag_code) values (",as.numeric(cap_id),",'",input$idRFID_new,"')")
          
          req = gsub("'NA'","NULL", req)
          req = gsub("''","NULL", req)
          
          dbSendQuery(con, req)
        }}
      
      } ####### fin de la recapture d''un animal identifie
    
    ############## Donnees non specifique au type de capture                        ###############
    

    if (exists("cap_id") && !is.null (cap_id)) {
    
    
    if (input$idSite != "") {
      find_site_id <- dbGetQuery(con, paste0("select sit_id from public.tr_site_capture_sit where sit_nom_court= '",input$idSite,"'"))[1,1]
      
      req<-paste0("UPDATE  public.t_capture_cap SET  cap_sit_id = (",find_site_id,") where  cap_id= ",cap_id," ")
      
      req = gsub("'NA'","NULL", req)
      req = gsub("''","NULL", req)
      dbSendQuery(con, req)
    }
    
    if (input$numSabot != "" && input$numSabot != "") {
      
      req<-paste0("UPDATE  public.t_capture_cap SET  cap_num_sabot = ('",input$numSabot,"') where  cap_id= ",cap_id," ")
      req = gsub("'NA'","NULL", req)
      req = gsub("''","NULL", req)
      
      dbSendQuery(con, req)
    }
    
    if (input$age != "") {
      
      if (input$age == '<1' || input$age == '0.5' ) {
        cat_age_all = "jeune" } else if (input$age=='1.5' || input$age=='1') {
          cat_age_all = "yearling"} else if (input$age=='2' || input$age=='2.5' || input$age=='3' || input$age=='3.5' || input$age=='4.5-5.5' || input$age=='4-5' || input$age=='>=6' || input$age=='>6.5') {
            cat_age_all="adulte"} else {cat_age_all=" "}
      
      req<- paste0("UPDATE public.t_capture_cap set (cap_age, cap_age_corrige, cap_age_classe) = ('",input$age,"', '",input$age,"', '",cat_age_all,"') where cap_id= ",cap_id," ")
      
      req = gsub("'NA'","NULL", req)
      req = gsub("''","NULL", req)
      
      dbSendQuery(con, req)}
    
    
    if (!is.na(input$cirCou)) {
      
      req<- paste0("UPDATE public.t_capture_cap set cap_circou = '",input$cirCou,"' where cap_id= ",cap_id," ")
      
      req = gsub("'NA'","NULL", req)
      req = gsub("''","NULL", req)
      
      dbSendQuery(con, req)
      
    }
    
    if (!is.na(input$lPattArriere)) {
      
      req<- paste0("UPDATE public.t_capture_cap set cap_lpa = '",input$lPattArriere,"' where cap_id= ",cap_id," ")
      
      req = gsub("'NA'","NULL", req)
      req = gsub("''","NULL", req)
      
      dbSendQuery(con,req)
      
    }    
    if (input$diarrhee != "" || input$remarque_ani!= ""  || input$liste_blessures!= "" ) {
      
      if (input$diarrhee != "") {diarrhee = paste0("diarrhee/",input$diarrhee)} else {diarrhee = "diarrhee/"}
      
      bledia <- paste(input$remarque_ani, input$liste_blessures, diarrhee, sep = "~")
      bledia<-gsub("'","''",bledia)
      Encoding(bledia) <- "UTF-8"
      bledia <- iconv(bledia, "UTF-8", "UTF-8",sub=' ')
      
      req<- paste0("UPDATE public.t_capture_cap set cap_etat_sante = '",bledia,"' where cap_id= ",cap_id," ")
      
      req = gsub("'NA'","NULL", req)
      req = gsub("''","NULL", req)
      
      dbSendQuery(con, req)
    }
    
    
    if (input$time != "") {
      if (input$time2 != ""){
        heure_lacher<- input$time2} else {heure_lacher<-input$time} 
      
      req<- paste0("UPDATE public.t_capture_cap set cap_heure_lacher = '",heure_lacher,"' where cap_id= ",cap_id," ")
      
      req = gsub("'NA'","NULL", req)
      req = gsub("''","NULL", req)
      
      dbSendQuery(con, req)
    }  
    
    if (input$Notation_euro_table != "") {
      
      ect_id<- dbGetQuery(con, paste0("Select ect_id from listes.tr_eurodeer_comp_table_ect where ect_comportement = '",input$Notation_euro_table,"' "))[1,1]
      
      req<- paste0("UPDATE public.t_capture_cap set cap_ect_id = '",ect_id,"' where cap_id= ",cap_id," ")
      
      req = gsub("'NA'","NULL", req)
      req = gsub("''","NULL", req)
      
      dbSendQuery(con, req)
      
    }
    
    if (input$Notation_euro != "") {
      ecl_id<- dbGetQuery(con, paste0("Select ecl_id from listes.tr_eurodeer_comp_lache_ecl where ecl_comportement_lache = '",input$Notation_euro,"' "))[1,1]
      
      req<- paste0("UPDATE public.t_capture_cap set cap_ecl_id = '",ecl_id,"' where cap_id= ",cap_id," ")
      
      req = gsub("'NA'","NULL", req)
      req = gsub("''","NULL", req)
      
      dbSendQuery(con, req)
    }
    
    id_lgg <- dbGetQuery(con, "select var_id from public.tr_variable_mesuree_var where var_nom_court = 'longueur bois gauche' ")
    id_lgd <- dbGetQuery(con, "select var_id from public.tr_variable_mesuree_var where var_nom_court = 'longueur bois droit' ")
    id_etat_bois <- dbGetQuery(con, "select var_id from public.tr_variable_mesuree_var where var_nom_court = 'etat des bois' ")
    id_lactation <- dbGetQuery(con, "select var_id from public.tr_variable_mesuree_var where var_nom_court = 'lactation' ")
    
    if (!is.na(input$lBoisGauche) && input$sexe == 'M') {
      req <- paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_lgg,"', '",cap_id,"','",input$lBoisGauche,"')
                    ON CONFLICT (nca_var_id, nca_cap_id) DO UPDATE SET (nca_valeur) = (excluded.nca_valeur)")
      dbSendQuery(con, req)
    }
    
    if (!is.na(input$lBoisDroit) && input$sexe == 'M') {
      req <- paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_lgd,"', '",cap_id,"','",input$lBoisDroit,"')
                    ON CONFLICT (nca_var_id, nca_cap_id) DO UPDATE SET (nca_valeur) = (excluded.nca_valeur)")
      dbSendQuery(con, req)
    }
    
    if (input$etatBois != "" && !is.null(input$sexe) && input$sexe == 'M') {
      value_etatbois <- dbGetQuery(con, paste0("SELECT etb_id from listes.tr_etat_bois_etb where etb_description = '",input$etatBois,"' "))[1,1]
      req <- paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_etat_bois,"', '",cap_id,"','",value_etatbois,"')
                    ON CONFLICT (nca_var_id, nca_cap_id) DO UPDATE SET (nca_valeur) = (excluded.nca_valeur)")
      dbSendQuery(con, req)
    }
    
    if (input$lactation != ""  && !is.null(input$sexe) && input$sexe == 'F') {
      req <- paste0("INSERT INTO public.tj_mesureealpha_capture_aca (aca_var_id, aca_cap_id, aca_valeur) VALUES ('",id_lactation,"', '",cap_id,"','",input$lactation,"')
                    ON CONFLICT (aca_var_id, aca_cap_id) DO UPDATE SET (aca_valeur) = (excluded.aca_valeur)")
      dbSendQuery(con, req)
    }
    
    if (exists("cap_id") && !is.null(cap_id)){
      deb_zone_etude <- substring(input$zone_etude, 1, 1)
      var_id_glu <- dbGetQuery(con, paste0("SELECT var_id FROM para_phy.tr_variable_measured_var where var_name_long ='glucose_blood'"))[1,1]
      var_id_dia <- dbGetQuery(con, paste0("SELECT var_id FROM para_phy.tr_variable_measured_var where var_name_long ='diarrhée'"))[1,1]
      var_id_tique <- dbGetQuery(con, paste0("SELECT var_id FROM para_phy.tr_variable_measured_var where var_name_long ='ticks_count'"))[1,1]
      var_id_autres_parasites <-dbGetQuery(con, paste0("SELECT var_id FROM para_phy.tr_variable_measured_var where var_name_long ='divers_macro_parasites'"))[1,1]
      
      exp_id <- dbGetQuery(con, paste0("SELECT exp_id FROM para_phy.tr_experimenter_exp where exp_name ='Verheyden, Helene/Thomas Roedl'"))[1,1]
      cnt_id <- dbGetQuery(con, paste0("SELECT cnt_id FROM para_phy.tr_analysis_counting_cnt where cnt_analysis_type ='counted around the head and between the rear leggs'"))[1,1]
      pat_id <- dbGetQuery(con, paste0("SELECT pat_id FROM para_phy.tr_pathogen_pat where pat_name ='Ixodida'"))[1,1]
      pat_id_autres_parasites<-dbGetQuery(con, paste0("SELECT pat_id FROM para_phy.tr_pathogen_pat where pat_name ='Arthropoda'"))[1,1]
      
      if (input$estNouvelAnimal == 'oui' || (input$estNouvelAnimal == 'non' && input$identifie == 'non')) {ani<-input$nAnimal} else {ani<-input$nAnimal2}
      cat_labo = paste0(deb_zone_etude, "_", annee, mois, jour,"_", ani)
      cat_phyhuman_glu = paste0(ani, "_", cat_labo, "_", as.character(input$date_caract), "_", 'glucose_blood')
      cat_phyhuman_dia = paste0(ani, "_", cat_labo, "_", as.character(input$date_caract), "_", 'diarrhée')
      
      if (!is.na(input$tglucose)) {
        req<- paste0("INSERT INTO para_phy.t_physiology_phy (phy_human_id, phy_ani_id, phy_laboriginid, phy_daysampling, phy_var_id, phy_exp_id, phy_res) VALUES
                     ('",cat_phyhuman_glu,"', '",find_ani_id,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",var_id_glu,"', '",exp_id,"', '",input$tglucose,"')
                     ON CONFLICT (phy_ani_id, phy_daysampling, phy_var_id, phy_laboriginid) DO UPDATE SET (phy_res) = (excluded.phy_res)")
        req <-gsub("'NA'","NULL", req)
        req <-gsub("''","NULL", req)
        dbSendQuery(con, req) }
      
      if (input$diarrhee != "") {
        req <- paste0("INSERT INTO para_phy.t_physiology_phy (phy_human_id, phy_ani_id, phy_laboriginid, phy_daysampling, phy_var_id, phy_exp_id, phy_res) VALUES
                      ('",cat_phyhuman_dia,"', '",find_ani_id,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",var_id_dia,"', '",exp_id,"', '",input$diarrhee,"')
                      ON CONFLICT (phy_ani_id, phy_daysampling, phy_var_id, phy_laboriginid) DO UPDATE SET (phy_res) = (excluded.phy_res)")
        req <-gsub("'NA'","NULL", req)
        req <-gsub("''","NULL", req)
        dbSendQuery(con, req) }
      
      if (input$tiques != "") {
        req <-paste0("INSERT INTO para_phy.t_parasitology_para ( para_ani_id, para_laboriginid, para_daysampling, para_exp_id, para_var_id, para_pat_id, para_res, para_eli_id, para_pcr_id , para_cnt_id) VALUES
                     ('",find_ani_id,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",exp_id,"', '",var_id_tique,"', '",pat_id,"', '",input$tiques,"', 0, 0, '",cnt_id,"')
                     ON CONFLICT (para_ani_id, para_laboriginid, para_daysampling, para_pat_id, para_var_id, para_eli_id, para_pcr_id, para_cnt_id) DO UPDATE SET (para_res) = (excluded.para_res)")
        req <-gsub("'NA'","NULL", req)
        req <-gsub("''","NULL", req)
        dbSendQuery(con,req) }
      
      if (input$parasites != "") {
        req <-paste0("INSERT INTO para_phy.t_parasitology_para ( para_ani_id, para_laboriginid, para_daysampling, para_exp_id, para_var_id, para_pat_id, para_res, para_eli_id, para_pcr_id , para_cnt_id) VALUES
                     ('",find_ani_id,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",exp_id,"', '",var_id_autres_parasites,"', '",pat_id_autres_parasites,"', '",input$parasites,"', 0, 0, '",cnt_id,"')
                     ON CONFLICT (para_ani_id, para_laboriginid, para_daysampling, para_pat_id, para_var_id, para_eli_id, para_pcr_id, para_cnt_id) DO UPDATE SET (para_res) = (excluded.para_res)")
        req <-gsub("'NA'","NULL", req)
        req <-gsub("''","NULL", req)
        dbSendQuery(con,req) }
      

      # if (!is.null(input$tablecollier_rows_selected)){
      #   ligne_selection <<- input$tablecollier_rows_selected
      #   find_eqt_id <<- dbGetQuery(con, paste0("select eqt_id from public.t_equipement_eqt where eqt_id_usuel = '",query()[ligne_selection,"eqt_id_usuel"],"'"))[1,1]
      #   find_pb_collier <<- dbGetQuery(con, paste0("select eqc_remarque from public.t_equipement_conf_eqc where eqc_eqt_id = '",find_eqt_id,"'"))[1,1]
      #   find_association <<- dbGetQuery(con, paste0("select sen_association from public.t_equipement_conf_eqc, listes.tr_sensors_sen where eqc_sen_id =sen_id and eqc_eqt_id = '",find_eqt_id,"'"))[1,1]
      #   if (length(grep("activite",find_association) != 0)) {act<- TRUE} else {act<- FALSE}
      #   if (length(grep("proximite",find_association) != 0)) {prox<- TRUE} else {prox<- FALSE}
      #   
      #   if (input$remarque_collier != ""){
      #     remarque_collier<- input$remarque_collier
      #     remarque_collier<-gsub("'","''",remarque_collier)
      #     Encoding(remarque_collier) <- "UTF-8"
      #     remarque_collier <- iconv(remarque_collier, "UTF-8", "UTF-8",sub=' ')
      #     if (is.na(find_pb_collier)){remarque_collier<- remarque_collier} else {remarque_collier<-paste0(find_pb_collier,"~", remarque_collier)}
      #   } else {if (is.na(find_pb_collier)) {remarque_collier<- NULL} else {remarque_collier<-find_pb_collier}} 
      #   
      #   
      #   req<- paste0("INSERT INTO public.tj_equipement_animal_eqt_ani_eqa (eqa_ani_id, eqa_eqt_id, eqa_date_debut, eqa_date_fin_arrondi, eqa_probleme, eqa_activite, eqa_annee_suivi) VALUES
      #                ('",find_ani_id,"', '",find_eqt_id,"', '",as.character(input$date_caract),"', FALSE ,'",remarque_collier,"','",act,"','",annee_suivie,"')
      #                ON CONFLICT (eqa_ani_id, eqa_date_debut) DO UPDATE SET (eqa_eqt_id, eqa_activite, eqa_probleme) = (excluded.eqa_eqt_id, excluded.eqa_activite, excluded.eqa_probleme)") 
      #   req<- gsub("'NA'","NULL", req)
      #   req<- gsub("''","NULL", req)
      #   dbSendQuery(con, req)
      #   
      #   req2<- paste0("UPDATE public.t_capture_cap set cap_proximite_contact = '",prox,"' where cap_id= ",cap_id," ")
      #   
      #   req2 = gsub("'NA'","NULL", req2)
      #   req2 = gsub("''","NULL", req2)
      #   
      #   dbSendQuery(con, req2)
      #   
      # }
      # 
      # if (input$sup_col && exists("ligne_selection")){
      #   req<- paste0("DELETE FROM public.tj_equipement_animal_eqt_ani_eqa WHERE eqa_ani_id='",find_ani_id,"' and eqa_eqt_id='",find_eqt_id,"' and eqa_date_debut='",as.character(input$date_caract),"'")
      #   req<- gsub("'NA'","NULL", req)
      #   req<- gsub("''","NULL", req)
      #   dbSendQuery(con, req)            
      # }
      
      if (find_ani_id != "" && as.character(input$date_caract) != ""|| input$estNouvelAnimal == 'oui' && as.character(input$date_caract) != "" || input$estNouvelAnimal == 'non' && input$identifie == 'non' && as.character(input$date_caract) != ""){
        
        if (!is.na(input$criautre) && !is.na(input$cribague) && exists("cap_id") && !is.null(cap_id)) {
          if (input$criautre!='0' || (input$cribague=='1-2' || input$cribague=='>2'))
          {cri_total = TRUE}
          else {cri_total = FALSE}
        }else {cri_total=FALSE}
        
        remarque_tot <- paste0(input$remarques_capt, input$remarques_table, input$remarques_lacher, sep = "~")
        remarque_tot<-gsub("'","''",remarque_tot)
        Encoding(remarque_tot) <- "UTF-8"
        remarque_tot <- iconv(remarque_tot, "UTF-8", "UTF-8",sub=' ') 
        
        if (input$estNouvelAnimal == 'oui' && input$date_capture =="choisir" || input$estNouvelAnimal == 'non' && input$identifie == 'non' && input$date_capture =="choisir") {ani<-input$nAnimal} else {ani<-input$nAnimal2}
        #print(paste0("ani est :",ani))
        if (ani != ""){
          #if (is.na(input$lutte)) {input$lutte <- NULL}
          req<- paste0("INSERT INTO cmpt.t_capture_cpt (cpt_ani_etiq, cpt_date, cpt_annee_suivi, cpt_tble_lutte, cpt_tble_halete, cpt_tble_cri_synthese, cpt_tble_cri_bague, cpt_tble_cri_autre, cpt_table_eurodeer, cpt_lache_titube, cpt_lache_couche, cpt_lache_course, cpt_lache_tombe, cpt_lache_gratte_collier, cpt_lache_cabriole, cpt_lache_bolide, cpt_lache_aboiement_cri, cpt_lache_nbre_stop, cpt_lache_habitat_lache, cpt_lache_habitat_pertevue, cpt_lache_visibilite, cpt_lache_public, cpt_lache_eurodeer,cpt_heure_debut_table, cpt_heure_lache, cpt_heure_second_lache, cpt_remarque, cpt_cap_id)
                       values ('",ani,"', '",as.character(input$date_caract),"', '",annee_suivie,"', '",input$lutte,"', '",input$halete,"', '",cri_total,"', '",input$cribague,"', '",input$criautre,"', '",input$Notation_euro_table,"', '",input$titube,"', '",input$couche,"', '",input$vitesse,"',
                       '",input$tombe,"', '",input$gratte_collier,"', '",input$cabriole_saut,"', '",input$allure,"', '",input$cri,"', '",input$nbre_stops,"', '",input$habitat,"', '",input$habitat_perte,"', '",input$visibilite,"',
                       '",input$nbre_personnes,"', '",input$Notation_euro,"', '",input$time_caract,"', '",input$time,"', '",input$time2,"', '",remarque_tot,"', '",cap_id,"')
                       ON CONFLICT (cpt_ani_etiq, cpt_date) DO nothing")
          
          req<- gsub("'NA'","NULL", req)
          req<- gsub("''","NULL", req)
          
          dbSendQuery(con, req)
        }}
      }
      
      if (!is.na(input$lutte)){
        req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_tble_lutte = '",input$lutte,"' where cpt_cap_id = ",cap_id," ")
        req<- gsub("'NA'","NULL", req)
        req<- gsub("''","NULL", req)
        dbSendQuery(con,req)
      }
      if (!is.na(input$halete)){
        req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_tble_halete = '",input$halete,"' where cpt_cap_id = ",cap_id," ")
        req<- gsub("'NA'","NULL", req)
        req<- gsub("''","NULL", req)
        dbSendQuery(con,req)
      }
      if (!is.na(input$criautre)){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set cpt_tble_cri_autre = '",input$criautre,"' where cpt_cap_id = ",cap_id," "))
        if (!is.null(input$criautre) && !is.null(input$cribague) && exists("cap_id") && !is.null(cap_id)) {
          if (input$criautre!='0' || (input$cribague=='1-2' || input$cribague=='>2'))
          {cri_total = TRUE}
          else {cri_total = FALSE}
        }else {cri_total= FALSE}
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set cpt_tble_cri_synthese = ",cri_total," where cpt_cap_id = ",cap_id," "))
      }
      if (!is.na(input$cribague)){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set cpt_tble_cri_bague = '",input$cribague,"' where cpt_cap_id = ",cap_id," "))
        if (!is.null(input$criautre) && !is.null(input$cribague) && exists("cap_id") && !is.null(cap_id)) {
          if (input$criautre!='0' || (input$cribague=='1-2' || input$cribague=='>2'))
          {cri_total = TRUE}
          else {cri_total = FALSE}
        }else {cri_total= FALSE}
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set cpt_tble_cri_synthese = ",cri_total," where cpt_cap_id = ",cap_id," "))
      }
      if (input$Notation_euro_table != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set cpt_table_eurodeer = '",input$Notation_euro_table,"' where cpt_cap_id = ",cap_id," "))
      }
      if (!is.na(input$titube)){
        req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_titube = '",input$titube,"' where cpt_cap_id = ",cap_id," ")
        req<- gsub("'NA'","NULL", req)
        req<- gsub("''","NULL", req)
        dbSendQuery(con,req)
      }
      if (!is.na(input$couche)){
        req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_couche = '",input$couche,"' where cpt_cap_id = ",cap_id," ")
        req<- gsub("'NA'","NULL", req)
        req<- gsub("''","NULL", req)
        dbSendQuery(con,req)
      }
      if (!is.na(input$vitesse)){
        req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_course = '",input$vitesse,"' where cpt_cap_id = ",cap_id," ")
        req<- gsub("'NA'","NULL", req)
        req<- gsub("''","NULL", req)
        dbSendQuery(con,req)
        }
      if (!is.na(input$tombe)){
        req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_tombe = '",input$tombe,"' where cpt_cap_id = ",cap_id," ")
        req<- gsub("'NA'","NULL", req)
        req<- gsub("''","NULL", req)
        dbSendQuery(con,req)
        }
      if (!is.na(input$gratte_collier)){
        req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_gratte_collier = '",input$gratte_collier,"' where cpt_cap_id = ",cap_id," ")
        req<- gsub("'NA'","NULL", req)
        req<- gsub("''","NULL", req)
        dbSendQuery(con,req)
        }
      if (!is.na(input$cabriole_saut)){
        req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_cabriole = '",input$cabriole_saut,"' where cpt_cap_id = ",cap_id," ")
        req<- gsub("'NA'","NULL", req)
        req<- gsub("''","NULL", req)
        dbSendQuery(con,req)
        }
      if (!is.na(input$allure)){
        req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_bolide = '",input$allure,"' where cpt_cap_id = ",cap_id," ")
        req<- gsub("'NA'","NULL", req)
        req<- gsub("''","NULL", req)
        dbSendQuery(con,req)
        }
    if (!is.na(input$cri)){
      req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_aboiement_cri = '",input$cri,"' where cpt_cap_id = ",cap_id," ")
      req<- gsub("'NA'","NULL", req)
      req<- gsub("''","NULL", req)
      dbSendQuery(con,req)
      }
      if (!is.null(input$nbre_stops)){
        req<- paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_nbre_stop = '",input$nbre_stops,"' where cpt_cap_id = ",cap_id," ")
        req<- gsub("'NA'","NULL", req)
        req<- gsub("''","NULL", req)
        dbSendQuery(con,req)
        }
      if (input$habitat != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_habitat_lache = '",input$habitat,"' where cpt_cap_id = ",cap_id," "))
      }
      if (input$habitat_perte != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_habitat_pertevue = '",input$habitat_perte,"' where cpt_cap_id = ",cap_id," "))
      }
      if (input$visibilite != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set cpt_lache_visibilite = '",input$visibilite,"' where cpt_cap_id = ",cap_id," "))
      }
      if (input$nbre_personnes != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_lache_public = '",input$nbre_personnes,"' where cpt_cap_id = ",cap_id," "))
      }
      if (input$Notation_euro != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_lache_eurodeer = '",input$Notation_euro,"' where cpt_cap_id = ",cap_id," "))
      }
      if (input$time_caract != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_heure_debut_table = '",input$time_caract,"' where cpt_cap_id = ",cap_id," "))
      }
      if (input$time != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_heure_lache = '",input$time,"' where cpt_cap_id = ",cap_id," "))
      }
      if (input$time2 != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_heure_second_lache = '",input$time2,"' where cpt_cap_id = ",cap_id," "))
      }
      if (input$remarques_capt != "" |input$remarques_table != ""|input$remarques_lacher != ""){
        remarque_tot <- paste0(input$remarques_capt, input$remarques_table, input$remarques_lacher, sep = "~")
        remarque_tot<-gsub("'","''",remarque_tot)
        Encoding(remarque_tot) <- "UTF-8"
        remarque_tot <- iconv(remarque_tot, "UTF-8", "UTF-8",sub=' ')
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_remarque = '",remarque_tot,"' where cpt_cap_id = ",cap_id," "))
      }

      if (nchar(as.character(input$time_caract)) == 8){
         if (nchar(as.character(input$time_table)) == 8){
                time_caract<-paste0("",as.character(input$date_caract)," ",input$time_caract," CEST")
                time_table<-paste0("",as.character(input$date_caract)," ",input$time_table," CEST") 
      y<-hour_conv(as.numeric(difftime(strptime(time_table, "%Y-%m-%d %H:%M:%S"), strptime(time_caract, "%Y-%m-%d %H:%M:%S"), units = "hours")))
      dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_temps_marquage = '",y,"' where cpt_cap_id = ",cap_id," "))
                                                       }
      }
    
    
    
    } ###if exists ("cap_id")

##### PARTIE CAPTURE ET SABOT
    
      if ((input$date_capture)!="") {
              if(!is.null(input$numSabot_capture)) {
                if (input$numSabot_capture!=""){
        cap_id<- dbGetQuery(con,paste0("select cap_id from t_capture_cap where cap_num_sabot = ",input$numSabot_capture," AND cap_date = '",input$date_capture,"' "))[1]
      
        if (input$nom_capteur_txt != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_nom_capteur = '",toupper(input$nom_capteur_txt),"' where cpt_cap_id = ",cap_id," "))
        }
        if (input$Nbre_pers_experimentes != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_nbre_pers_experimentes = '",input$Nbre_pers_experimentes,"' where cpt_cap_id = ",cap_id," "))
        }
        if (!is.null(input$cpt_filet_vitesse)){
          req<- paste0("UPDATE cmpt.t_capture_cpt set  cpt_arrivee_filet_course = '",input$cpt_filet_vitesse,"' where cpt_cap_id = ",cap_id," ")
          req<- gsub("'NA'","NULL", req)
          req<- gsub("''","NULL", req)
          dbSendQuery(con,req)
          }
        if (!is.null(input$cpt_filet_allure)){
          req<- paste0("UPDATE cmpt.t_capture_cpt set  cpt_arrivee_filet_panique = '",input$cpt_filet_allure,"' where cpt_cap_id = ",cap_id," ")
          req<- gsub("'NA'","NULL", req)
          req<- gsub("''","NULL", req)
          dbSendQuery(con,req)
          }
        if (!is.null(input$cpt_filet_lutte)){
          req<- paste0("UPDATE cmpt.t_capture_cpt set  cpt_filet_lutte = '",input$cpt_filet_lutte,"' where cpt_cap_id = ",cap_id," ")
          req<- gsub("'NA'","NULL", req)
          req<- gsub("''","NULL", req)
          dbSendQuery(con,req)
          }
        if (!is.null(input$cpt_filet_halete)){
          req<- paste0("UPDATE cmpt.t_capture_cpt set  cpt_filet_haletement = '",input$cpt_filet_halete,"' where cpt_cap_id = ",cap_id," ")
          req<- gsub("'NA'","NULL", req)
          req<- gsub("''","NULL", req)
          dbSendQuery(con,req)
          }
        if (!is.null(input$cpt_filet_cri)){
          req<- paste0("UPDATE cmpt.t_capture_cpt set  cpt_filet_cri = '",input$cpt_filet_cri,"' where cpt_cap_id = ",cap_id," ")
          req<- gsub("'NA'","NULL", req)
          req<- gsub("''","NULL", req)
          dbSendQuery(con,req)
          }
        if (!is.null(input$cpt_sabot_couche)){
          req<- paste0("UPDATE cmpt.t_capture_cpt set  cpt_sabot_couche = '",input$cpt_sabot_couche,"' where cpt_cap_id = ",cap_id," ")
          req<- gsub("'NA'","NULL", req)
          req<- gsub("''","NULL", req)
          dbSendQuery(con,req)
          }
        if (!is.null(input$cpt_sabot_agitation)){
          req<- paste0("UPDATE cmpt.t_capture_cpt set  cpt_sabot_agitation = '",input$cpt_sabot_agitation,"' where cpt_cap_id = ",cap_id," ")
          req<- gsub("'NA'","NULL", req)
          req<- gsub("''","NULL", req)
          dbSendQuery(con,req)
          }
        if (!is.null(input$cpt_sabot_retournement)){
          req<- paste0("UPDATE cmpt.t_capture_cpt set  cpt_sabot_retournement = '",input$cpt_sabot_retournement,"' where cpt_cap_id = ",cap_id," ")
          req<- gsub("'NA'","NULL", req)
          req<- gsub("''","NULL", req)
          dbSendQuery(con,req)
          }
      
        if (nchar(as.character(input$cpt_heure_fin_surv)) == 19){
        cpt_heure_fin_surv<- format(strptime(input$cpt_heure_fin_surv, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_hre_fin_surv = '",cpt_heure_fin_surv,"' where cpt_cap_id = ",cap_id," "))
        }
        if (nchar(as.character(input$cpt_heure_fin_surv)) == 19 && nchar(as.character(input$cpt_heure_debut_filet)) == 19 ){
        y<-as.numeric(difftime(strptime(input$cpt_heure_fin_surv, "%Y-%m-%d %H:%M:%S"), strptime(input$cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S"), units = "min"))
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_temps_surveillance_mn = ",y," where cpt_cap_id = ",cap_id," "))
        }
        if (input$time2 != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_distance_km = NULL where cpt_cap_id = ",cap_id," "))
        }
        if (nchar(as.character(input$cpt_heure_mise_sabot)) == 19 && nchar(as.character(input$cpt_heure_debut_filet)) == 19 ){
        y<-as.numeric(difftime(strptime(input$cpt_heure_mise_sabot, "%Y-%m-%d %H:%M:%S"), strptime(input$cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S"), units = "hours"))
        y<-hour_conv(y)
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_temps_filet = '",y,"' where cpt_cap_id = ",cap_id," "))
        }

        if (nchar(as.character(input$cpt_heure_debut_filet)) == 19){
        cpt_heure_debut_filet<- paste0("",fi," ",substring(input$cpt_heure_debut_filet, 12,25),"")
        cpt_heure_second_lacher<- dbGetQuery(con,paste0("select cpt_heure_second_lache from cmpt.t_capture_cpt where cpt_cap_id = ",cap_id," "))[1,1]
        cpt_heure_lacher<- dbGetQuery(con,paste0("select cpt_heure_lache from cmpt.t_capture_cpt where cpt_cap_id = ",cap_id," "))[1,1]
        if (!is.na(cpt_heure_second_lacher)) {heure_lacher<- cpt_heure_second_lacher} else {heure_lacher<-cpt_heure_lacher}
        heure_lacher<-paste0("",fi," ",heure_lacher," CEST")
        y<-as.numeric(difftime(strptime(heure_lacher, "%Y-%m-%d %H:%M:%S"), strptime(cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S"), units = "hours"))
        y<-hour_conv(y)
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_temps_total = '",y,"' where cpt_cap_id = ",cap_id," "))
        }
          
        if (nchar(as.character(input$cpt_heure_debut_filet)) == 19){
        cpt_heure_debut_filet<- format(strptime(input$cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_heure_debut_filet = '",cpt_heure_debut_filet,"' where cpt_cap_id = ",cap_id," "))
        }
        if (nchar(as.character(input$cpt_heure_mise_sabot)) ==19){
        cpt_heure_mise_sabot<- format(strptime(input$cpt_heure_mise_sabot, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_heure_mise_sabot = '",cpt_heure_mise_sabot,"' where cpt_cap_id = ",cap_id," "))
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_heure_acepro = '",cpt_heure_mise_sabot,"' where cpt_cap_id = ",cap_id," "))
        }
        if (input$cpt_dose_acepromazine != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_dose_acepromazine = '",input$cpt_dose_acepromazine,"' where cpt_cap_id = ",cap_id," "))
        }
        if (input$time2 != ""){
        dbSendQuery(con,paste0("UPDATE cmpt.t_capture_cpt set  cpt_heure_debut_transport = NULL where cpt_cap_id = ",cap_id," "))
        }
      }}}####fin capture_sabot
  })####fin observe pour db
}###fonction serveur 
  