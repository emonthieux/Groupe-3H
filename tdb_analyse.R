## app.R ##

#chargement des packages necessaires pour l'applicatif

#Interface du tableau de bord
library(shinydashboard)
#connection a la base de donnee
library(RPostgreSQL)
#Interface avec la base de donnee
library(DBI)
#graphiques
library(highcharter)

#connection a la base de donnees
drv<-dbDriver("PostgreSQL")

#parametres de la connection : db name est le nom de la base de donnees
                              #host est l IP de connection
                              #port est le port de connection de la base de donnees
                              #user est l identifiant de connection a la base de donnee
                              #password est le mot de passe de l identifiant de connection a la base de donnee
con <- dbConnect(drv, dbname = "mytestdb", host = "localhost", port = 5432,  user = "postgres", password = "stidniort")

#chargement de la liste des etablissements (etb) et des differents services (serv)

etb <- dbGetQuery(con,'Select distinct("LIB_ETB") as "ETABLISEMENT" from table_analyse order by "LIB_ETB"')
serv <- dbGetQuery(con,'Select distinct("LAFFECTATION") as "AFFECTATION" from table_analyse order by "LAFFECTATION"')

#chargement de la masse salariale par etablissement

total <- dbGetQuery(con,'Select "LIB_ETB" as "Etablissement", SUM("MONTANT") as "Masse salariale" from table_analyse  Group By "LIB_ETB" order by "LIB_ETB"')

#chargement de la masse salariale par CDI, CDD et autres types de contrats

CDI <- dbGetQuery(con,'Select "LIB_ETB" as "Etablissement", SUM("MONTANT") as "Masse salariale" from table_analyse  where "CNATURECONTRAT" =  \'CDI\' Group By "LIB_ETB" order by "LIB_ETB"')
CDD <- dbGetQuery(con,'Select "LIB_ETB" as "Etablissement", SUM("MONTANT") as "Masse salariale" from table_analyse  where "CNATURECONTRAT" =  \'CDD\' Group By "LIB_ETB" order by "LIB_ETB"')
reste <- dbGetQuery(con,'Select "LIB_ETB" as "Etablissement", SUM("MONTANT") as "Masse salariale" from table_analyse  where "CNATURECONTRAT" !=  \'CDI\' and "CNATURECONTRAT" !=  \'CDD\' Group By "LIB_ETB" order by "LIB_ETB"')

#creation de l interface utilisateur.
#On cree une page

ui <- dashboardPage(
  #L en tete de la page
  dashboardHeader(title = "Tableau de bord"),
  #La barre de selection de la page
  dashboardSidebar(
    sidebarMenu(
      menuItem("MASSE SALARIALE", tabName = "total", icon = icon("th")),
      menuItem("TYPES DE CONTRATS", tabName = "contrats", icon = icon("th")),
      menuItem("SERVICES", tabName = "services", icon = icon("th")),
      menuItem("QUALIFICATIONS DES METIERS", tabName = "qualifications", icon = icon("th")),
      menuItem("EMPLOYES", tabName = "employes", icon = icon("th"))
    )
  ),
  #Le corp de la page
  dashboardBody(
    #Les onglets de la page
    tabItems(
      
      #Contenu du premier onglet
      tabItem(tabName = "total",
              #On cree une ligne
              fluidRow(
                #on rajoute une boite dans laquelle on place un titre et une autre boite contenant un tableau
                box(h4("Masse salariale totale par etablissement"),tableOutput("total")
                )
                ,
                #Toujours dans la ligne on cree un graphique dans une boite
                box(highchartOutput("totalplot", height = 300)
                )  
              )
              
      ),
      #Contenu du deuxieme onglet
      tabItem(tabName = "contrats",
              #Ici on cree une ligne contenant deux box avec un tableau et un titre
              fluidRow(
                box(h4("Masse salariale des CDI par etablissement"),(tableOutput("CDI"))
                )
                ,
                box(h4("Masse salariale des CDD par etablissement"),(tableOutput("CDD"))
                )
              )
              ,
              #Puis une autre ligne avec un tableau avec un titre
              fluidRow(
                box(h4("Masse salariale des autres types de contrats par etablissement"),tableOutput("reste")
                )
              )
              ,
              #On fini la page avec une nouvelle ligne contenant deux box avec un graphique
              fluidRow(
                box(highchartOutput("CDIplot", height = 300)),
                box(highchartOutput("CDDplot", height = 300)
                    
                )
              )
      )
      ,
      #Contenu du troisieme onglet
      tabItem(tabName = "services",
              #Une ligne avec un tableau, un selecteur en liste deroulante et un graphique
              fluidRow(
                #Le tableau
                box(h4("Masse salariale par service"),tableOutput("services")
                ),
                #Le selecteur
                box(selectizeInput("services", "Etablissement", choices = etb)
                ),
                #Le graphique
                box(highchartOutput("servicesplot", height = 500)
                )  
              )
      ),
      #Contenu du quatrieme onglet
      tabItem(tabName = "qualifications",
              #Meme chose que pour le troisieme onglet
              fluidRow(
                box(h4("Masse salariale par qualification"),tableOutput("qualifications")
                ),
                box(selectizeInput("qualifications", "Etablissement", choices = etb)
                ),
                box(highchartOutput("qualificationsplot", height = 500)
                )  
              )
      ),
      #Contenu du cinquieme onglet
      tabItem(tabName = "employes",
              #On cree une ligne
              fluidRow(
                #Une box avec titre avec un tableau
                box(h4("Masse salariale par employe"), tableOutput("employes")
                ),
                #Une box contenant 4 box avec un selecteur
                box(box(title = "Etablissements",
                    #Un premier selecteur en liste deroulante
                    selectizeInput("employes01",
                                label = "SELECTIONNEZ UN ETABLISSEMENT",
                                choices = (etb)
                    )
                    ),
                  box(title = "Services",
                      #Un deuxieme selecteur en liste deroulante
                      selectizeInput("employes02",
                                  label = "SELECTIONNEZ UN SERVICE",
                                  choices = (serv)
                                  
                      )
                      
                  ),
                  box(title = "Nom",
                      #Un troisieme selecteur avec un champ de texte
                      textInput("employes03",label = "ENTREZ UN NOM")
                  ),
                  box(title = "Prenom",
                      #Un quatrieme selecteur avec un champ de texte
                      textInput("employes04",label = "ENTREZ UN PRENOM")
                  )
                )
              )
      )
      
    )
  )
)

#On cree la partie serveur de l applicatif
#Les variables input et outpout permettent le dialogue entre le serveur et l interface utilisateur

server <- function(input, output) {
  
  #On transforme le tableau de donnees contenant la masse salariale par etablissement en table
  
  output$total <- renderTable(total)
  
  #On transforme le tableau de donnees contenant la masse salariale par etablissement en graphique circulaire
  
  output$totalplot <- renderHighchart({
    highchart() %>%
      hc_add_series_labels_values(type = "pie",labels = paste(total$Etablissement,': ',total$'Masse salariale',' EUROS <br/>',round(total$`Masse salariale`/sum(total$`Masse salariale`)*100),'%'), values = total$'Masse salariale') %>%
      hc_title(text = "Masse salariale par etablissement en euros")
  })
  
  #On transforme les tableaux de donnees contenant la masse salariale par type de contrats et par etablissement en tables
  
  output$CDI <- renderTable(CDI)
  output$CDD <- renderTable(CDD)
  output$reste <- renderTable(reste)
   
  #On transforme les tableaux de donnees contenant la masse salariale pour par etablissement pour les CDI et les CDD en graphiques circulaires
  
  output$CDIplot <- renderHighchart({
    highchart() %>%
      hc_add_series_labels_values(type = "pie",labels = paste(CDI$Etablissement,': ',CDI$'Masse salariale',' EUROS <br/>',round(CDI$`Masse salariale`/sum(CDI$`Masse salariale`)*100),'%'), values = CDI$'Masse salariale') %>%
      hc_title(text = "Masse salariale des CDI par etablissement en euros")
  })
  
  output$CDDplot <- renderHighchart({
    highchart() %>%
      
      #type correspond au type de graphique
      #labels correspond a ce que l on souhaite afficher comme descriptif
      #values correspond aux donnees numeriques
      #Ici, labels est long car on cherche a afficher le nom de l etablissement avec sa masse salariale et la proportion que cela represente
      #L operation effectuee pour y arriver est de souder avec paste() le nom du service avec sa masse salariale et a la ligne le pourcentage que cela represente
      #La balise HTML <br/> est comprise par le package shiny et permet de revenir a la ligne
      #la fonction round() permet d arrondir un nombre
      
      hc_add_series_labels_values(type = "pie",labels = paste(CDD$Etablissement,': ',CDD$'Masse salariale',' EUROS <br/>',round(CDD$`Masse salariale`/sum(CDD$`Masse salariale`)*100),'%'), values = CDD$'Masse salariale') %>%
      
      #On donne un titre au graphique
      
      hc_title(text = "Masse salariale des CDD par etablissement en euros")
  })
  
  #On transforme le tableau de donnees contenant la masse salariale par service et selon un etablissement en table
  #La variable input$services correcpond a l etablissement choisi par le selecteur se nommant services
  
  output$services <- renderTable(dbGetQuery(con,paste('Select "LAFFECTATION" as "Service",sum("MONTANT") as "Masse salariale" from table_analyse where "LIB_ETB" = ',input$qualifications,' group by "LAFFECTATION" order by "LAFFECTATION"',sep ="\'", collapse = NULL)))
  
  #On transforme le tableau de donnees contenant la masse salariale par service et selon un etablissement en fonction
  #Cela est necessaire pour avoir des graphiques interactifs mais ne fonctionne pas avec les tables comme output$services
  
  servicesR <- reactive(dbGetQuery(con,paste('Select "LAFFECTATION" as "Service",sum("MONTANT") as "Masse salariale" from table_analyse where "LIB_ETB" = ',input$qualifications,' group by "LAFFECTATION" order by "LAFFECTATION"',sep ="\'", collapse = NULL)))
  
  #On transforme la fonction contenant la masse salariale selon un etablissement en graphique a barres verticales
  
  output$servicesplot <- renderHighchart({
    highchart() %>%
      
      #type correspond au type de graphique
      #name correspond a ce que l on souhaite afficher comme nom de serie, ici le nom du service
      #data correspond aux donnees numeriques
      
      hc_add_series(type = "column",name = input$services, data = servicesR()$'Masse salariale') %>%
      
      #categories correspond a ce que l on souhaite afficher comme descriptif
      #labels = list(enabled = FALSE)  permet de desactiver le nom des colonnes a la base du graphique car cela deviendrait illisible
      #Ici, categories est long car on cherche a afficher le nom de l etablissement avec sa masse salariale et la proportion que cela represente
      #L operation effectuee pour y arriver est de souder avec paste() le nom du service avec sa masse salariale et a la ligne le pourcentage que cela represente
      #La balise HTML <br/> est comprise par le package shiny et permet de revenir a la ligne
      #la fonction round() permet d arrondir un nombre
      
      hc_xAxis(categories = paste(servicesR()$Service,': ',servicesR()$'Masse salariale',' EUROS <br/>',round(servicesR()$`Masse salariale`/sum(servicesR()$`Masse salariale`)*100),'%'), labels = list(enabled = FALSE)) %>%
      
      #On donne un titre au graphique
      
      hc_title(text = "Repartition de la masse salariale par service en euros")
  })
  
  
  #On transforme le tableau de donnees contenant la masse salariale par qualification et selon un etablissement en fonction
  
  output$qualifications <- renderTable(dbGetQuery(con,paste('Select "LQUALIFICATION" as "Qualifiquation",sum("MONTANT") as "Masse salariale" from table_analyse where "LIB_ETB" = ',input$qualifications,' group by "LQUALIFICATION" order by "LQUALIFICATION"',sep ="\'", collapse = NULL)))
  
  #On transforme la fonction contenant la masse salariale par qualification et selon un etablissement en graphique a barres verticales
  
  qualificationsR <- reactive(dbGetQuery(con,paste('Select "LQUALIFICATION" as "Qualification",sum("MONTANT") as "Masse salariale" from table_analyse where "LIB_ETB" = ',input$qualifications,' group by "LQUALIFICATION" order by "LQUALIFICATION"',sep ="\'", collapse = NULL)))
  
  #On transforme la fonction contenant la masse salariale par qualification et selon un etablissement en graphique a barres verticales
  
  output$qualificationsplot <- renderHighchart({
    highchart() %>%
      
      #type correspond au type de graphique
      #name correspond a ce que l on souhaite afficher comme nom de serie, ici le nom du service
      #data correspond aux donnees numeriques
      
      hc_add_series(type = "column",name = input$qualifications, data = qualificationsR()$'Masse salariale') %>%
      
      #categories correspond a ce que l on souhaite afficher comme descriptif
      #labels = list(enabled = FALSE)  permet de desactiver le nom des colonnes a la base du graphique car cela deviendrait illisible
      #Ici, categories est long car on cherche a afficher le nom de l etablissement avec sa masse salariale et la proportion que cela represente
      #L operation effectuee pour y arriver est de souder avec paste() le nom du service avec sa masse salariale et a la ligne le pourcentage que cela represente
      #La balise HTML <br/> est comprise par le package shiny et permet de revenir a la ligne
      #la fonction round() permet d arrondir un nombre
      
      hc_xAxis(categories = paste(qualificationsR()$Qualification,': ',qualificationsR()$'Masse salariale',' EUROS <br/>',round(qualificationsR()$`Masse salariale`/sum(qualificationsR()$`Masse salariale`)*100),'%'), labels = list(enabled = FALSE)) %>%
      
      #On donne un titre au graphique
      
      hc_title(text = "Repartition de la masse salariale par qualification en euros")
  })
  
    #On transforme le tableau de donnees contenant la masse salariale par service et par etablissement en tableau de donnees  exploitable par le package shiny
    #on place des conditions if pour que la requette se fasse si aucun nom ou prenom n est entre par l utilisateur
    #la fonction toupper() passe du texe en majuscule ce qui permet a la saisie de ne pas etre sensible a la casse
    
    output$employes <- renderTable(dbGetQuery(con,paste('Select "MATRICULE", "NOM", "PRENOM", sum("MONTANT") as "Masse salariale" from table_analyse where "LIB_ETB" = ',input$employes01,' and "LAFFECTATION" = ',input$employes02,if (!is.null(input$employes03)){paste(' and "NOM" like ',paste(toupper(input$employes03),'%',sep = "",collapse = NULL),sep = "\'",collapse = NULL)},if (!is.null(input$employes04)){paste(' and "PRENOM" like ',paste(toupper(input$employes04),'%',sep = "",collapse = NULL),sep = "\'",collapse = NULL)},' group by "MATRICULE", "NOM", "PRENOM" order by "MATRICULE", "NOM", "PRENOM"',sep ="\'", collapse = NULL)))
    
}

#On execute l applicatif

shinyApp(ui, server)