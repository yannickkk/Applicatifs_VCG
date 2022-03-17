#con<-local_gardouch
host<<-"pggeodb.nancy.inra.fr"
con<<- dbConnect(PostgreSQL(), host= host, dbname="db_chevreuils", user="ychaval",password="bvta;814")

shinyUI(
  navbarPage(windowTitle = "IE Gardouch: ", title=div(tags$a(href="https://github.com/yannickkk/Applicatifs_IE_Gardouch/blob/main/App_registre/readme.md","Documentation", target ="_blank")),
             tabPanel("Enristrer un crane en collection",  
                      fluidRow(
                        shinyjs::useShinyjs(),
                        shinyjs::inlineCSS(appCSS),
                        shinyalert::useShinyalert(),
                        column(width= 1,selectizeInput(inputId = "ani_etiq", label = labelMandatory("Animal"),choices =append("inconnu",enc2utf8(dbGetQuery(con,"select distinct(ani_etiq), ani_id FROM public.v_individus_total order by ani_id;")[,1])),selected ="inconnu"),style = "margin-top:25px;"),
                        bsTooltip("ani_etiq", "Choisir un animal","top", options = list(container = "body")),
                        column(width= 2,textOutput("ani_crane"),style = "margin-top:55px;"),
                        bsTooltip("ani_crane", "Ce code est à repporter au marqueur indélébile sur le crane et les machoires","top", options = list(container = "body")),
                        column(width= 1,uiOutput("container_parent_identifier"),style = "margin-top:10px;"),
                        column(width= 1,uiOutput("sample_line"),style = "margin-top:10px;"),
                        column(width= 1,uiOutput("sample_column"),style = "margin-top:10px;"),
                        column(width= 1,uiOutput("sampling_date"),style = "margin-top:30px;"),
                        column(width= 5,uiOutput("md_remark"),style = "margin-top:30px;"),
                        column(12),
                        column(width= 2,actionButton("save", "Enregistrer", class = "btn-primary"),style = "margin-top:50px;"),
                        bsTooltip("subcol", "les champs avec une * rouge doivent être remplis pour que le bouton soit actif","bottom", options = list(container = "body")),
                        column(width= 2,actionButton("new", "Effacer", class = "btn-primary"),style = "margin-top:50px;"),
                        bsTooltip("new", "Désélectionner l\\'animal en cours","bottom", options = list(container = "body")),
                        column(width= 2,p(class = 'text-center', downloadButton('downloadcsv', 'Télécharger csv')),style = "margin-top:50px;"),
                        bsTooltip("downloadcsv", "fichier csv en UTF-8 pour l'import dans Collec Science (import de masse)","bottom", options = list(container = "body")),
                        column(12, tags$br()),
                        column(12, tags$br()),
                        DT::dataTableOutput("registre", width = 300)#, add_busy_bar(color = "red", height = "8px")
                      ))
  ))

dbDisconnect(con)