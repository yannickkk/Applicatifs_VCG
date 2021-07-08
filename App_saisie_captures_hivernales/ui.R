##################              USER INTERFACE                 ################# 

#       Objet: Saisie des données de capture pour les chevreuils de Fabas et Gardouch
#       Réalisation: projet tuteuré Master 1 Bioinformatique Université Paul Sabatier
#       Réalisé par : Tristan BERLIN, Marie JEREMIE, Edi TIHIC encadrés par Yannick CHAVAL
#       Date de réalisation : fev/juil 2018
#       Tablette => résolution : 1280*800 (Mozilla : CTRL+SHIFT+M  et  CHROME : F12)


# Création de la mise en page des formulaires

##################           Rubrique Animal               ############################

contentcaractanimal = fluidPage(
  useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  #jsResetCode <- extendShinyjs(text = "shinyjs.reset = function() {history.go(0)}"), 
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tags$head(
    tags$style(HTML("#idTagOrG2 + div> div>.item {
                    color: blue !important;}
                    #idTagOrD2 + div> div>.item {
                    color: blue !important;}
                    #idRFID2 + div> div>.item {
                    color: blue !important;}
                    #idSite2 + div> div>.item {
                    color: blue !important;}
                    #nAnimal2 + div> div>.item {
                    color: blue !important;}
                    #rfid_erase + div> div>.item {
                    color: blue !important;}      "))
    ),

  fluidRow(
    uiOutput("sabotExiste"),
    uiOutput("out_sabot_plein"),
    uiOutput("out_sabot_vide"),
    uiOutput("animalExiste"),
    uiOutput("alert_poids"),
    uiOutput("tagDroitExiste"),
    uiOutput("tagGaucheExiste"),
    uiOutput("tagDroitExiste2"),
    uiOutput("tagGaucheExiste2"),
    uiOutput("animal_inexistant"),
    uiOutput("sexe_inexistant"),
    uiOutput("mauvais_identifiant"),
    uiOutput("perte_poids"),
    
    #column(2, selectizeInput("numSabot", h4("N° Sabot"), choices ="", options=list(placeholder='Choisir une valeur :',create= TRUE, onInitialize = I('function() { this.setValue(""); }'), onFocus=I('function() {blur()}');",onChange="blur();",onBlur="blur();"), selected = NULL)),
    column(2, selectInput("numSabot", h4("N° Sabot"), choices ="",selectize =FALSE, selected = NULL)),
    column(2, numericInput(inputId = "pSabotPlein", value = "",label = h4("P.Sabot Plein"),min=0,max=65)),
    column(2, numericInput(inputId = "pSabotVide", value = "",label = h4("P.Sabot Vide"),min=0,max=50)),
    column(2, h4("Poids Animal"), textOutput("poids_ani")),
    column(2, checkboxInput("appear", "faire apparaitre Capture-sabot", FALSE)),
    column(1, textInput("rfid_erase", h4("GR250"), value = "")),
    column(1, actionButton("rfid_clear", "clear")),
    #column(2, awesomeRadio("open_key",p("Saisie"),  choices = c("Oui","Non"),inline = TRUE,  selected = "Non")), 
    
    column(12),
    
    column(2, textInput("time_caract", h4("Heure Début:"), value = NULL), 
    actionButton("to_current_time_caract", "Heure de début")), 

    column(3, dateInput('date_caract',label=h4("Date"),value = Sys.Date())), 
    column(2, awesomeRadio(inputId = "estNouvelAnimal", choices = c("oui", "non"),selected = "oui",label = h4("1ere Capture"))),

    column(1, conditionalPanel(condition = "input.estNouvelAnimal == 'non'", awesomeRadio(inputId = "identifie", choices = c("oui","non"), selected = "non",label = h4("Identifé"))), conditionalPanel(condition = "input.estNouvelAnimal == 'oui'", awesomeRadio(inputId = "identifie", choices = c("non"), selected = "non",label = h4("Identifé")))),
    column(2, selectInput("sexe",h4("Sexe"),  choices = choix[["sexe"]], selected = "",selectize =FALSE)),
    column(1, actionButton("rfid_read" ,h4("Lire RFID"))),
    #column(1, awesomeRadio("sexe",h4("Sexe"),  choices = c("M","F"), selected = "NA")),
    
    column(12,hr()),
    
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifie == 'non')", textInput(inputId = "nAnimal", value = "",label = h4("N° Animal")))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifie == 'non')", textInput("idTagOrG", h4("Tag Or.Gauche"),value=""))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifie == 'non')", textInput("idTagOrD", h4("Tag Or.Droite"),value=""))),
    # column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || input.estNouvelAnimal == 'non' && input.identifie == 'non'", selectizeInput("idSite", h4("Site"),choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'), create = T), selected = NULL))),
    # column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifie == 'non')", selectizeInput("idRFID", h4("RFID"),                                                                                                                                                            choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || input.estNouvelAnimal == 'non' && input.identifie == 'non'", selectInput("idSite", h4("Site"),choices = "",selectize =FALSE, selected = NULL))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifie == 'non')", selectInput("idRFID", h4("RFID"),choices = "",selectize =FALSE, selected = NULL))),
    
    column(12),
    
    column(1, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifie == 'non')", actionButton("cre_ind" ,"Créer ?"))),
    column(1, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifie == 'non')", prettyCheckbox("create_ind", "ind créé", FALSE, status =  'success' , fill = TRUE ))), 
    #column(1, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifie == 'non')", prettyCheckboxGroup("create_ind","",  choiceNames = c("",""), choiceValues =  c(FALSE,FALSE),inline = TRUE,  status =  'success' , fill = TRUE ))),
    column(1, offset = 0),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifie == 'non')", checkboxInput("metal_tag_g", "Tag G. métal", value = FALSE ))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifie == 'non')", checkboxInput("metal_tag_d", "Tag D. métal", value = FALSE ))),

    column(12),
    
    #column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'",selectInput("nAnimal2",h4("N° Animal"), choices = "",selectize =FALSE, selected = "choisir"))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'",selectizeInput("nAnimal2",h4("N° Animal"), choices = "", options=list( onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'",uiOutput("conditionalInput1"))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'",uiOutput("conditionalInput2"))),

    #column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'", selectizeInput("idSite2", h4("Site"), choices = "", options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'), create=T), selected = NULL))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'", selectInput("idSite2", h4("Site"), choices = "", selectize =FALSE, selected = NULL))),
    #span(h6("* ATTENTION: au second lâcher repeser l'animal"),  style="color:red"))
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'",uiOutput("conditionalInput3"))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'", h4("Poids antérieur"), textOutput("poids_ani_anc"))),
    column(12),
    
    column(2,offset = 2, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'",uiOutput("conditionalInput4"))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'", uiOutput("conditionalInput5"))),
    column(1, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'", checkboxInput("newTagG", "N.T.G", value = FALSE ))),
    column(1, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'", checkboxInput("newTagD", "N.T.D", value = FALSE ))),
    column(1, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifie == 'oui'", checkboxInput("newRFIDbox", "N.RFID", value = FALSE ))),
    column(12)
    
     ),
  
  column(12,hr()),
  
  fluidRow(
    column(2, numericInput("cirCou", value='', h4("Cir. cou"),min=0, max=1)),
    uiOutput("out_cirCou"),
    column(2, numericInput("lPattArriere", value='', h4("Long pat ar"),min=0, max=1)),
    uiOutput("out_lPattArriere"),
    column(2, numericInput("tglucose", value="", h4("T.Glucose"), min=0)),
    #column(2, selectizeInput("age", h4("Age"), choices =choix[["age"]], options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    column(2, selectInput("age", h4("Age"),choices = choix[["age"]], selectize =FALSE, selected = NULL)),
    
    #column(2, conditionalPanel(condition = "input.sexe == 'F'", selectizeInput("lactation", h4("Lactation"), choices = choix[["lactation"]],  options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(2, conditionalPanel(condition = "input.sexe == 'F'", selectInput("lactation", h4("Lactation"), choices = choix[["lactation"]],selectize =FALSE, selected = NULL))),
    
    column(12),
    # column(2, selectizeInput("diarrhee", h4("Diarrhee ?"),choices = choix[["diarrhee"]], options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    # column(2, selectizeInput("tiques", h4("Nombre Tiques"), choices = choix[["tiques"]], options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    column(2, selectInput("diarrhee", h4("Diarrhee ?"),choices = choix[["diarrhee"]],selectize =FALSE, selected = NULL)), 
    column(2, selectInput("tiques", h4("Nombre Tiques"), choices = choix[["tiques"]],selectize =FALSE, selected = NULL)), 
    column(2, textInput("parasites", h4("A.parasites"), value = "")),
    # column(2, selectizeInput("zone_etude", h4("Zone d'etude"), choices = c("Aurignac", "Gardouch"), selected = "Aurignac" )),
    column(2, selectInput("zone_etude", h4("Zone d'etude"), choices = c("Aurignac", "Gardouch"), selected = "Aurignac" )),
    column(2, textInput("remarque_ani", h4("Remarques"), value = ""))
    
      ),
  
  conditionalPanel(
    condition = "input.sexe == 'M'",
    
    fluidRow(
      column(2, numericInput("lBoisGauche", value='', h4("Long b.gauche"),min=0, max=1)),
      column(2, numericInput("lBoisDroit", value='', h4("Long b.droit"),min=0, max=1)),
      #uiOutput("out_lBoisGauche"), 
      #uiOutput("out_lBoisDroit"),
      #column(2, selectizeInput("etatBois", h4("etat bois"), choices = "" , options = list(create = TRUE)))
      column(2, selectInput("etatBois", h4("etat bois"), choices = "",selectize =FALSE, selected = NULL)) 
          )
  )
  
  
    )

##################           Rubrique Blessures            #################

contentblessures = fluidPage( 
  fluidRow(
    
    column(2,uiOutput("casc_ble1")),
    column(2,uiOutput("casc_ble2")),
    #column(3,selectizeInput("traitement", h4("Traitement"), choices = "", multiple=TRUE, options=list(create=TRUE))),
    #column(3,selectizeInput("traitement", h4("Traitement"), choices = "", multiple = T)),
    column(3,selectInput("traitement", h4("Traitement"), choices = "",selected = "rien",selectize =FALSE, multiple = T)),
    column(3,textInput("remarques_ble",h4("Remarques"),value = "")),
    column(12,hr()),
    column(2,offset = 1, actionButton("ajout_Bles","Ajouter blessure")),
    column(2,actionButton("sup_Bles", "Supprimer blessure")),
    column(12,hr()),
    dataTableOutput("tableblessure"),
    column(12,textInput("liste_blessures",h4("Liste des blessures"),value = "", width = "100%"))
  )
)

##################           Rubrique Prélèvements         #################

contentprelevement = fluidPage(
  
  fluidRow(
    column(2,uiOutput("control1")),
    column(2,uiOutput("control2")),
    column(2,uiOutput("control3")),
    column(2,uiOutput("control4")),
    #column(2, selectizeInput("nbre_echant", h4("Nombre"), choices = choix[["nbre_echant"]],options=list(create=T), selected = NULL)),
    column(2, selectInput("nbre_echant", h4("Nombre"), choices = choix[["nbre_echant"]],selectize =FALSE, selected = NULL)),
    column(2, textInput("remarques_prel",h4("Remarques"), value="")),
    column(12,hr()),
    column(2,offset = 3, actionButton("ajout_prelev",("Ajouter prelevement"))),
    column(2,actionButton("sup_prelev", "Supprimer prelevement")),
    column(12,hr()),
    dataTableOutput("tableprelevement")
  )
)

##################           Rubrique Collier              #################

contentcollier = fluidPage(
  fluidRow(
    #column(2, radioButtons(inputId = "new_collier", choices = c("oui","non"), selected = "non",label = h4("Nouveau collier"),inline = TRUE)),
    #column(12,hr()),
    column(7, DT::dataTableOutput("tablecollier")),
    column(12,hr()),
    column(5, h4("Le collier choisi est (tech, collier, boitier): "), verbatimTextOutput("collier_choisi") ,offset = 1),
    column(3, textInput("remarque_collier", label="Remarques") , offset = 1),
    column(2, actionButton("sup_col", "Désélectionner") , offset = 2),
    useShinyalert()
  ))

##################           Rubrique Table                #################

contenttable = fluidPage(

  fluidRow(
    #column(2,uiOutput("sonde_loc")),
    # column(2,selectizeInput("Notation_euro_table", h4("Notation Eurodeer"),
    #                         choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL),
    column(2,selectInput("Notation_euro_table", h4("Eurodeer"), choices = "",selectize =FALSE, selected = NULL),
    textInput("remarques_table", h4("Remarques"), value="" )),
    column(2,textInput("time_table", h4("Heure de fin:"),value=""),
    actionButton("to_current_time_table", "Afficher l'heure")),
    column(1,radioButtons("lutte",h4("Lutte"), choiceNames = choix[["names_oui_non"]], choiceValues = choix[["values_oui_non"]], selected = "NA")),
    column(1,radioButtons("halete",h4("Halete"),choiceNames = choix[["names_oui_non"]],choiceValues = choix[["values_oui_non"]], selected = "NA")),
    column(1,radioButtons("cribague",h4("Cri Bague"), choiceNames = c("NA","0","1-2",">2"), choiceValues = choix[["cribague"]],  selected = "NA")),
    column(1,radioButtons("criautre", h4("Cri Autre"), choiceNames = c("NA","0","1-2",">2"), choiceValues = choix[["criautre"]], selected = "NA")),
    
    column(12,hr())

    #dataTableOutput("tabletemperature")
  ),
  
  fluidRow(
    column(3, selectInput("bool", h4("Prophylaxie"), choices = choix[["booleens"]],selectize =FALSE, selected =  "myophos")),
    #column(2, checkboxInput("val_bool", h4("OK"), value = FALSE, width = NULL)),
    column(2, textInput("rem_bool",h4("Remarque"), value="")),
    #column(12,hr()),
    column(2, actionButton("ajout_bool","Ajouter prophylaxie")),
    column(2, actionButton("sup_bool", "Supprimer prophylaxie")),
    column(12,hr())
    
  ),
  
  fluidRow(
    column(3, selectInput("mes", h4("Mesure"), choices = choix[["mes_heure"]],selectize =FALSE, selected =  "temperature ext_ysi")),
    column(2, numericInput("Val_mes", value="", h4("Valeur"), min=0)),
    column(2, textInput("rem_mes_heure",h4("Remarque"), value="")),
    #column(12,hr()),
    column(2, actionButton("ajout_mes_heure","Ajouter mesure")),
    column(2, actionButton("sup_mes_heure", "Supprimer mesure")),
    column(12,hr()),
    dataTableOutput("table_mes_heure"),
    dataTableOutput("table_bool")
  )
  
  # ),
  # 
  # 
  # splitLayout(cellWidths = c( "25%", "65%"),
  #             list(checkboxInput("suivi_temp", h4("Suivi des températures"), value = F),
  #             selectInput( "sonde_temp1", h4("Sonde 1"), choices = list("rouge","blanche"),selectize =FALSE, selected = 'rouge'),
  #             selectInput( "position_temp1", h4("Positionnement 1"), choices = "",selectize =FALSE),
  #             selectInput( "sonde_temp2", h4("Sonde 2"), choices = list("rouge","blanche"),selectize =FALSE, selected = 'blanche'),
  #             selectInput( "position_temp2", h4("Positionnement 2"), choices = "",selectize =FALSE)),
  # 
  #             conditionalPanel(condition = "input.suivi_temp ==  TRUE" ,  plotOutput("plot"))
  # 
  # )

)

##################           Rubrique Historique           #################

contenthistorique <- fluidPage(
  #fluidRow(
  
  tabPanel("Historique de capture",
           DT::dataTableOutput("historique")
           #         conditionalPanel(
           #    condition = "input.estnouvelanimal == 0",h4("Historique de capture"),DT::dataTableOutput("historique")
  )
  # )
)

##################           Rubrique Checklist 1          #################

contentcheck1 =  fluidPage(fluidRow(
  uiOutput("checklist_1"),
  column(4, h3("Checklist - Animal"), offset=1),
  column(4, h3("Checklist - Table"),offset = 1),
  column(4,tabPanel("Cheklist 1", DT::dataTableOutput("tablechecklist1")),offset=1), 
  column(4,tabPanel("Checklist Table",DT::dataTableOutput("tablechecklist_table")), offset=1),
  column(12,hr()),
  column(4, h3("Checklist - Prelevement"),offset = 1),
  column(4, h3("Checklist - Collier"),offset = 1),
  column(4,tabPanel("Checklist Prelevement",DT::dataTableOutput("tablechecklist_prel")), offset=1),
  column(4,tabPanel("Checklist Collier",DT::dataTableOutput("tablechecklist_collier")), offset=1),
  column(12,hr()),
  column(12, useShinyalert(),actionButton("valid_checklist1", "ENREGISTRER LES DONNEES", width='100%')),
  column(12,hr())
))

##################           Rubrique Lâcher               #################


###submitButton(format(Sys.time(), "%X"))
#timeInput("time2", "Heure lâcher:", value = Sys.time(),seconds = FALSE))

contentlacher = fluidPage(
  
  uiOutput("checklist1_invalid"),
  # titlePanel("Comportement au lâcher"),
  fluidRow(
    column(2,textInput("time", h4("lâcher"), value = ""),
           actionButton("to_current_time", "Afficher l'heure")),
    
    column(2, textInput("time2", h4("2nd lâcher:"), value = ""),
           actionButton("to_current_time2", "Afficher l'heure")),
    
    column(2,numericInput("nbre_stops",value="", h4("Nbre stops"),min=0)),
    #span(h6("* ATTENTION: au second lâcher repeser l'animal"),  style="color:red")),
    # column(2,selectizeInput("nbre_personnes", h4("Nbre de personnes"), choices = choix[["nbre_personnes"]], options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    column(2,selectInput("nbre_personnes", h4("Nbre pers"), choices = choix[["nbre_personnes"]], selectize =FALSE, selected = NULL)),
    
    column(12,hr()),
    
    column(1,radioButtons("vitesse",h4("Vitesse"),choiceNames = choix[["vitesse"]],choiceValues = choix[["values_vitesse"]], selected = "NA")),
    column(1,radioButtons("allure",h4("Allure"),choiceNames = choix[["allure"]],choiceValues = choix[["values_allure"]], selected = "NA")),
    column(1,radioButtons("cabriole_saut",h4("Cabriole"), choiceNames = choix[["names_oui_non"]], choiceValues = choix[["values_oui_non"]], selected = "NA")),
    column(1,radioButtons("gratte_collier", h4("Gratte col"), choiceNames = choix[["names_oui_non"]], choiceValues = choix[["values_oui_non"]], selected = "NA")),
    column(1,radioButtons("tombe", h4("Tombe"), choiceNames = choix[["names_oui_non"]], choiceValues = choix[["values_oui_non"]], selected = "NA")),
    column(1,radioButtons("cri",h4("Cri"),choiceNames = choix[["names_oui_non"]],choiceValues = choix[["values_oui_non"]], selected = "NA")),
    column(1,radioButtons("titube",h4("Titube"),choiceNames = choix[["names_oui_non"]],choiceValues = choix[["values_oui_non"]], selected = "NA")),
    column(1,radioButtons("couche",h4("Couche"), choiceNames = choix[["names_oui_non"]], choiceValues = choix[["values_oui_non"]], selected = "NA")),
    
    column(12,hr()),
    
    # column(2,selectizeInput("visibilite", h4("Visibilite fuite"), 
    #                         choices = choix[["visibilite"]], options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    # 
    # column(2,selectizeInput("habitat", h4("Habitat lâcher"), 
    #                         choices = "", options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'),create = TRUE), selected = NULL)),
    # 
    # column(2, selectizeInput("habitat_perte", h4("Habitat perte de vue"), 
    #                          choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'),create = TRUE), selected = NULL)),
    # 
    # column(2,selectizeInput("Notation_euro", h4("Notation Eurodeer"), 
    #                         choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),

    column(2,selectInput("visibilite", h4("Visibilite fuite"), 
                            choices = choix[["visibilite"]],selectize =FALSE, selected = NULL)),
    
    column(2,selectInput("habitat", h4("Habitat lâcher"), 
                            choices = "",selectize =FALSE, selected = NULL)),
    
    column(2, selectInput("habitat_perte", h4("Hab perte vue"), 
                             choices = "",selectize =FALSE, selected = NULL)),
    
    column(2,selectInput("Notation_euro", h4("Eurodeer"), 
                            choices = "",selectize =FALSE, selected = NULL)),
    column(2, textInput("remarques_lacher", h4("Remarques"), value="")),
    column(2,useShinyalert())
    
  ))



##################           Rubrique Checklist 2          #################

contentcheck2 = fluidPage(fluidRow(
  uiOutput("checklist_2"),
  column(4, h3("Checklist - Lacher"),offset = 3),
  column(4,tabPanel("Checklist 2", DT::dataTableOutput("tablechecklist2")), offset=3), 
  column(12,hr()),
  column(12, useShinyalert(), actionButton("valid_checklist2","ENREGISTRER LES DONNEES", width='50%'), offset = 3),
  column(12,hr())
))

##################           Rubrique Données          #################

contentcsv = fluidPage(
              tabPanel("affichage des données :", DT::dataTableOutput("csv")
              ),
              
              column(12,hr()),
              
              column(2, actionButton("shutdown", "éteindre le Raspberry"))
)

##################           Rubrique Capture              #################

contentcapture = fluidPage(

  #titlePanel("Comportement Capture"),
  fluidRow(

    column(2,selectInput("date_capture",label=h4("Date"),choices = "",selectize =FALSE)),#,options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'))
    column(3,selectInput("numSabot_capture",label = h4("N° Sabot"), choices = "",selectize =FALSE)),#,options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'))
    #column(2,selectInput("date_capture",label=h4("Date"),choices = "",selectize =FALSE, selected = NULL)),
    #column(3,selectInput("numSabot_capture",label = h4("N° Sabot"), choices = "",selectize =FALSE, selected = NULL)),
    column(2,timeInput("cpt_heure_debut_filet",h4("Heure arrivee filet"), seconds = F, value = strptime("00:00",format = "%H:%M"))),
           #actionButton("time_debut_filet", "Afficher l'heure")),
    column(2,numericInput("cpt_temps_filet", h4("Temps passe filet"), value = NA, min = 0, max = 60, step = 1)),
           #actionButton("time_filet", "Afficher l'heure")),
    column(12,hr()),

    column(2,textInput("nom_capteur_txt",label=h4("Nom des capteurs",""))),
    #column(3,selectizeInput("Nbre_pers_experimentes",h4("Nombre de capteurs experimentes"),choices = choix[["Nbre_pers_experimentes"]],options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    column(3,selectInput("Nbre_pers_experimentes",h4("Nombre de capteurs experimentes"),choices = choix[["Nbre_pers_experimentes"]],selectize =FALSE, selected = NULL)),
    column(2,textInput("remarques_capt",label=h4("Remarques",""))),
    column(12,hr()),

    column(1,radioButtons("cpt_filet_vitesse",h4("Vitesse"),choiceNames = choix[["vitesse"]],choiceValues = choix[["values_vitesse"]], selected =  "NA")),
    column(1,radioButtons("cpt_filet_allure",h4("Allure"),choiceNames = choix[["allure"]],choiceValues = choix[["values_allure"]],selected =  "NA")),
    column(1,radioButtons("cpt_filet_lutte", h4("Lutte"), choiceNames = c("NA",0,1,2),choiceValues = choix[["cpt_filet_lutte"]], selected =  "NA")),
    column(1,radioButtons("cpt_filet_halete",h4("Halete"), choiceNames = choix[["names_oui_non"]], choiceValues = choix[["values_oui_non"]], selected =  "NA")),
    column(1,radioButtons("cpt_filet_cri",h4("Cri"),choiceNames = choix[["names_oui_non"]],choiceValues = choix[["values_oui_non"]], selected =  "NA"))
    # column(12,hr()),

    #column(1),
    # column(4,useShinyalert(),
    #        actionButton("checklist_capture", "Checklist",icon('eye')))


  ))


##################           Rubrique Sabot                #################


contentsabot = fluidPage(
  # titlePanel("Comportement sabot"),
  fluidRow(

    #Heure de mise en sabot
    column(3, timeInput("cpt_heure_mise_sabot", h4("Heure de mise en sabot:"), seconds = F, value = strptime("00:00",format = "%H:%M"))),
           #actionButton("time_sabot", "Afficher l'heure")),

    #Fin de surveillance
    column(3,timeInput("cpt_heure_fin_surv", h4("Fin de surveillance"), seconds = F, value = strptime("00:00",format = "%H:%M"))),
           #actionButton("time_fin", "Afficher l'heure")),

    column(12,hr()),

    #Acepromazine
    #column(2,selectizeInput("cpt_dose_acepromazine",h4("Acepromazine"), choices = "",options = (list(create = TRUE,placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'))), selected = NULL)),
    column(2,selectInput("cpt_dose_acepromazine",h4("Acepromazine"), choices = "",selectize =FALSE, selected = NULL)),

    #Sur le dos
    column(1,radioButtons("cpt_sabot_retournement",h4("Sur le dos"),choiceNames = choix[["names_oui_non"]],choiceValues = choix[["values_oui_non"]], selected = "NA")),

    #Couche
    column(1, radioButtons("cpt_sabot_couche",h4("Couche"),choiceNames = choix[["names_oui_non"]],choiceValues = choix[["values_oui_non"]], selected =  "NA")),

    #Agite
    column(1, radioButtons("cpt_sabot_agitation",h4("Agite"),choiceNames = choix[["names_oui_non"]],choiceValues = choix[["values_oui_non"]], selected = "NA")),

    column(12,hr()),

    #Observateur
    column(3,textInput("Observateur",label=h4("Observateurs",""))),

    #Remarque
    column(3,textInput("Remarques",label=h4("Remarque","")))

    # column(12,hr()),

    # column(4,useShinyalert(),
    #        actionButton("checklist_sabot", "Checklist",icon('eye')))
    #
  )
)


##################           Rubrique Checklist 3          #################

contentcheck3 = fluidPage(fluidRow(
  uiOutput("checklist_capture"),
  column(4, h3("Checklist - Capture"), offset = 1),
  column(4, h3("Checklist - Sabot"), offset = 1),
  column(4,tabPanel("Checklist Capture", DT::dataTableOutput("tablechecklist_capture")), offset=1),
  column(4,tabPanel("Checklist Sabot", DT::dataTableOutput("tablechecklist_sabot")), offset=1),
  column(12,hr()),
  column(12,useShinyalert(), actionButton("valid_checklist3","ENREGISTRER LES DONNEES", width='50%'), offset = 1),
  column(12,hr())
))

##################        ORGANISATION DES RUBRIQUES       #################


caractanimal = tabPanel("Caract. de l'animal",contentcaractanimal)
blessures = tabPanel("Blessures",contentblessures)
prelevement= tabPanel("Prelevements",contentprelevement)
caractcollier = tabPanel("Caract. du collier",contentcollier)
comportable = tabPanel("Comportement table",contenttable)
historique = tabPanel("Historique captures",contenthistorique)
checklist1 = tabPanel("checklist 1",contentcheck1)
comporlacher = tabPanel("Comportement lâcher",contentlacher)
checklist2 = tabPanel("checklist 2",contentcheck2)
csv = tabPanel("csv",contentcsv)
comporcapture = tabPanel("Comportement capture",contentcapture)
comporsabot = tabPanel("Comportement sabot",contentsabot)
checklist3 = tabPanel("Checklist3", contentcheck3)


##################                    UI                   #################

##Lumen or cerulean or sandstone

ui <- shinyUI(
  #theme=shinytheme("sandstone"),
  # Application title
    tabsetPanel(id = "tab",
    tabPanel  ("Animal", caractanimal),
    tabPanel  ("Blessures", blessures),
    tabPanel  ("Prelevement", prelevement),
    tabPanel  ("Collier",caractcollier),
    tabPanel  ("Table",comportable),
    tabPanel  ("Historique",historique),
    tabPanel  ("Checklist 1",checklist1),
    tabPanel  ("Lâcher",comporlacher),
    tabPanel  ("Checklist 2",checklist2),
    tabPanel  ("Données",csv),
    tabPanel  ("Capture",comporcapture),
    tabPanel  ("Sabot",comporsabot),
    tabPanel  ("Checklist3",checklist3)
    )
)

