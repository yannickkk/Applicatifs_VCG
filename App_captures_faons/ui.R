shinyUI(bootstrapPage(
  mainPanel(
  useShinyalert(),
  fluidRow(tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 12px;} .selectize-dropdown { font-size: 12px; line-height: 12px; }")),
  column(2,textInput("login","Utilisateur",value = "")),
  column(2,textInput("password","Mot de passe",value = "")),
  column(2,actionButton("soumettre", "Soumettre"),style = "margin-top: 25px;"),
  column(12),
  column(4,selectInput("configuration","configuration",choices = c(aucune="aucune",capture_faon="capture_faon", animal="animal",capture="capture",comportement_adulte="comportement_adulte", comportement_faon="comportement_faon"),selected= "aucune", multiple = FALSE)),
  column(4,selectInput("champs","champs à ajouter",choices = c("tous", names(input_data)),selected= "ani_etiq", multiple = TRUE)),
  #column(12),
  column(1,actionButton("add_btn", "Ajouter un individu"),style = "margin-top: 25px;"),
  column(1,actionButton("val_btn", "Valider la saisie"),style = "margin-top: 25px;margin-left: 125px;"),
  column(12),
  column(12),
  column(2,p(class = 'text-center', downloadButton('csv', 'Télécharger csv2'))),
  #DT::DTOutput("mod_table")),
  verbatimTextOutput('sel'),
  DT::dataTableOutput('mod_table')),
  column(12),
  column(2,actionButton("reset", "désélectionner"))
, theme = shinytheme("flatly")))

#theme darkly flatly journal lumen paper readable sandstone simplex slate spacelab superhero united yeti