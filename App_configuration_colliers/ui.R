
# App to define new GPS device and annual configuration
shinyUI(
  navbarPage("Rubriques: ",
             tabPanel("Caractéristiques du nouveau collier",  
                      fluidRow(
                        shinyjs::useShinyjs(),
                        shinyjs::inlineCSS(appCSS),
                        shinyalert::useShinyalert(),
                        column(width= 1,textInput(inputId = "eqt_id_usuel", label = labelMandatory("N°collier"))),
                        column(width= 2,textInput(inputId = "eqt_frequence", label = "Fréquence de l'émetteur VHF", value = "")),
                        column(width= 2,uiOutput("control1")),
                        column(width= 3,uiOutput("control2")),
                        column(width= 3,uiOutput("control3")),
                        br(),
                        column(width= 1,actionButton("subcol", "Submit", class = "btn-primary")),
                        br(), br(),br(),
                        DT::dataTableOutput("responses", width = 300), tags$hr()
                        
                                              )),
             tabPanel("Configuration du collier",  
                      fluidRow(
                        #column(width= 2,selectInput(inputId = "eqt_id_usuel_user", label = "N° du collier", choices = sort(as.numeric(dbGetQuery(con, "select distinct(eqt_id_usuel) from public.t_equipement_eqt")[,1])))),
                        column(width= 2,uiOutput("colliers_existant")), ####here we test if you the list integrate the last collar entered before
                        column(width= 1,numericInput(inputId = "eqc_annee_suivi", label = labelMandatory("Année de suivi"), value = 2021, min = year(Sys.time()), max = 2025, step = 1)),
                        column(width= 1,textInput(inputId = "eqc_drop_off", label = "N° de drop off")),
                        column(width= 1,selectInput(inputId = "eqc_couleur_collier", label = "Couleur du collier",choices = append("",na.omit(dbGetQuery(con, "select distinct(eqc_couleur_collier) col_col from public.t_equipement_conf_eqc order by col_col")[,1])), selected = "", selectize = FALSE)),
                        column(width= 1,selectInput(inputId = "eqc_couleur_boitier", label = "Couleur du boitier",choices = append("",na.omit(dbGetQuery(con, "select distinct(eqc_couleur_boitier) col_boi from public.t_equipement_conf_eqc order by col_boi")[,1])), selected = "", selectize = FALSE)), 
                        column(width= 1,numericInput(inputId = "eqc_memoire", label = labelMandatory("Mémoire récepteur"), value = NULL, min = 1, max = 257)),
                        column(width= 2,selectInput(inputId = "sen_association", label = labelMandatory("Capteurs associés"),choices = dbGetQuery(con, "select sen_association from listes.tr_sensors_sen order by sen_association DESC"))),
                        column(width= 2,textInput(inputId = "eqc_remarque", label = "Remarque")),
                        column(width= 12),
                        column(width= 2,selectInput(inputId = "eqc_freq", label = "Recherche freq VHF",choices = append("",na.omit(dbGetQuery(con, "select distinct eqt_frequence freq from public.t_equipement_eqt order by eqt_frequence")[,1])), selected = "")),
                        column(width= 2,actionButton("subcolconf", "Submit", class = "btn-primary"),style = "margin-top:25px;"),
                        br(),
                        br(), br(),br(),
                        DT::dataTableOutput("responsesconf", width = 300), tags$hr()
                      ))
 
             ))

dbDisconnect(con)