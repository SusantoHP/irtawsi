library(shiny)
library(bs4Dash)
library(shinycssloaders)
library(mirt)
library(readxl)
library(gt)
library(rmarkdown)
library(diagram)
library(WrightMap)
library(mirtCAT)
library(writexl)


ui<-dashboardPage(
  dashboardHeader(h3("|"),
                  shinyWidgets::actionBttn(inputId = "inputdataok",label = "INPUT YOUR DATA",style = "fill",color = "primary",size = "sm",block = TRUE,no_outline = FALSE),h3("|"),
                  shinyWidgets::actionBttn(inputId = "fitingmodel",label = "FINDING FIT MODEL",style = "fill",color = "primary",size = "sm",block = TRUE,no_outline = FALSE),h3("|"),
                  shinyWidgets::actionBttn(inputId = "assumptionprove",label = "IRT ASSUMPTION ",style = "fill",color = "primary",size = "sm",block = TRUE,no_outline = FALSE),h3("|"),
                  shinyWidgets::actionBttn(inputId = "parameterokk",label = "ITEM PARAMETER ",style = "fill",color = "primary",size = "sm",block = TRUE,no_outline = FALSE),h3("|"),
                  shinyWidgets::actionBttn(inputId = "ploticciic",label = "ICC,IIC & TIC",style = "fill",color = "primary",size = "sm",block = TRUE,no_outline = FALSE),h3("|"),
                  shinyWidgets::actionBttn(inputId = "penskoran",label = "THETA SCORING",style = "fill",color = "primary",size = "sm",block = TRUE,no_outline = FALSE),h3("|")
  ),
  dashboardSidebar( disable =F, collapsed = F,width = 0,sidebarMenu(id="inTabset",menuItem("MANUAL BOOK",tabName = "h1"),br(),menuItem("INPUT YOUR DATA",tabName = "h2"),br(),menuItem("FINDING FIT MODEL)",tabName = "h3"),br(),menuItem("IRT ASSUMPTION ",tabName = "h4"),br(),menuItem("ITEM & PERSON PARAMETER",tabName = "h5"),br(),menuItem("ICC, IIC & TIC",tabName = "h6"),br(),menuItem("THETA SCORING",tabName = "h7"))),
  dashboardBody(tabItems(


    tabItem(tabName = "h1",
            column(
              h4("IRT Analysis With Steps and Interpretation (irtawsi)", style="  font-family: 'cursive';color: blue;   text-align:center "),hr(),width = 12,
              fluidRow(column("",width = 4),column(width = 4,align="center",
                                                   shinyWidgets::radioGroupButtons(checkIcon = list( yes = icon("square-check"),no = icon("square")),inputId = "boso",label = h4("CHOOSE  LANGUAGE", style="  font-family: 'cursive';color: red; align:center "),choices = c("in_English","in_indonesia"),status = "primary",justified = T,direction = "horizontal",individual = T,size = "lg")),column("",width = 4)),hr(),
              fluidRow(box(title = h5(textOutput("labelboxtentang"),style="text-align:center"),closable = FALSE,enable_label = TRUE,label_status = "danger",background = "navy",status = "primary",solidHeader = TRUE,collapsible = F,h5("PACKAGE NAME : irtawsi"),hr(),h5("DESCRIPTION  :"),h5(textOutput("ketdeskripsi")),hr(),h5("AUTHORS  :"),h5("1.Hari Purnomo Susanto, 2.Heri Retnawati,3.Agus Maman Abadi, 4.Haryanto, 5. Raden Muhammad ALI, 6. Hasan Djidu"),h5("AVAILABLE: English and Indonesia Language Interpretation,"),h5("IMPORTS:"),h5("shiny,bs4Dash,shinycssloaders, mirt, readxl, gt, DT,psyc, rmarkdown, diagram, mirtCAT, writexl, and WrightMap."),h5("LICENCE  : GPL (>= 3)"),h5("ENCODING : UTF-8"),h5("LAZYDATA : true"),h5("NEEDCOMPILATOIN: No"),width = 6),
                       box(title = h5(textOutput("labelboxstepstep"),style="text-align:center"),closable = FALSE,enable_label = TRUE,label_status = "danger",background = "navy",status = "primary",solidHeader = TRUE,collapsible = F,
                           plotOutput("flowchart",height = "600px"),width = 6)))),
    tabItem(tabName = "h2",
            column(width = 12,fluidRow(
              box(width = 3,
                  title =textOutput("labelboxinput"),
                  closable = FALSE,
                  enable_label = TRUE,
                  label_status = "danger",
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = FALSE,

                  fileInput("ambildata", textOutput("labelinput"),  accept = c( "text/csv", "text/comma-separated-values,text/plain","csv","xlsx","xls","txt")),
                  shinyWidgets::radioGroupButtons(checkIcon = list( yes = icon("square-check"),no = icon("square")),
                                                  inputId = "ekstensi",
                                                  label = textOutput("labelekstensi"),
                                                  choices = c("xlsx", "csv", "txt"),
                                                  status = "primary",
                                                  justified = T,
                                                  direction = "horizontal",
                                                  individual = T,
                                                  size = "sm"),
                  conditionalPanel(condition = "input.ekstensi=='csv' || input.ekstensi=='txt'",
                                   radioButtons("pemisahvariabel", "Separator",choices = c(Comma = ",",Semicolon = ";",  Tab = "\t"),selected = ",", inline = TRUE)),hr(),
                  shinyWidgets::radioGroupButtons(checkIcon = list( yes = icon("square-check"),no = icon("square")),
                                                  inputId = "pilihmodel",
                                                  label = textOutput("labelcls"),
                                                  choices = c("POLYTOMOUS", "DHICOTOMOUS"),
                                                  status = "primary",
                                                  justified = T,
                                                  direction = "horizontal",
                                                  individual = T,
                                                  size = "sm"),hr(),
                  actionButton("analisis","ANALYSIS",
                               status = "success",
                               size = "lg"),hr(),
                  h4(textOutput("deteksiawal"),style="  font-family: 'cursive';color: red;   text-align:center "),hr(),
                  conditionalPanel(condition = "input.pilihmodel=='DHICOTOMOUS'",
                                   fluidRow(bs4InfoBoxOutput("ibox",width = 6),
                                            bs4InfoBoxOutput("ibox2",width = 6),
                                            bs4InfoBoxOutput("ibox3",width = 6),
                                            bs4InfoBoxOutput("ibox4",width = 6))),
                  conditionalPanel(condition = "input.pilihmodel=='POLYTOMOUS'",
                                   fluidRow(bs4InfoBoxOutput("ibox5",width = 6),
                                            bs4InfoBoxOutput("ibox6",width = 6),
                                            bs4InfoBoxOutput("ibox7",width = 6))),hr(),
                  uiOutput("selesksiawal"),
                  footer = actionButton("buttoninfor1","Step 1",icon =  icon("walking"))),
              box(title = textOutput("labelboxdata"),
                  closable = FALSE,
                  enable_label = TRUE,
                  label_status = "danger",
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  DT::DTOutput("tampildata"),hr(),hr(),
                  uiOutput("tampildataok1"),width = 9 )))),

    tabItem(tabName = "h3",
            column(width = 12,fluidRow(
              box(width = 2,
                  title = textOutput("labelboxmetodefit"),
                  closable = FALSE,
                  enable_label = TRUE,
                  status = "danger",
                  solidHeader = TRUE,
                  shinyWidgets::radioGroupButtons(checkIcon = list( yes = icon("square-check"),no = icon("square")),
                                                  inputId = "metodefit",

                                                  choices = c( "Likelihood","M2*","C2"),
                                                  status = "success",
                                                  justified = T,
                                                  direction = "vertical",
                                                  individual = T,
                                                  size = "lg"),
                  footer = actionButton("buttoninfor1a","Infomation1",icon =  icon("info-circle"))),
              box(width = 6,
                  title = textOutput("labelboxcompar1"),
                  closable = FALSE,
                  enable_label = TRUE,
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  conditionalPanel(condition = "input.metodefit=='M2*' || input.metodefit=='C2'",
                                   tableOutput("perbandinganmodelfit")),
                  conditionalPanel(condition = "input.metodefit=='Likelihood'",
                                   tableOutput("perbandinganmodel")),
                  footer =fluidRow(conditionalPanel(condition = "input.metodefit=='M2*' || input.metodefit=='C2'",actionButton("buttoninfor2","Information_3",icon =  icon("info-circle"))),conditionalPanel(condition = "input.metodefit=='Likelihood'",actionButton("buttoninfor3","Information_2",icon =  icon("info-circle"))))),

              box(width =3 ,
                  title = textOutput("labelboxrecom"),
                  height = "260px",
                  closable = FALSE,
                  enable_label = TRUE,
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  conditionalPanel(
                    condition = "input.pilihmodel=='DHICOTOMOUS'",fluidRow(bs4InfoBoxOutput("ibox1a",width = 6),bs4InfoBoxOutput("ibox2a",width = 6),bs4InfoBoxOutput("ibox3a",width = 6),bs4InfoBoxOutput("ibox4a",width = 6))),conditionalPanel(condition = "input.pilihmodel=='POLYTOMOUS'",fluidRow(bs4InfoBoxOutput("ibox5a",width = 6),bs4InfoBoxOutput("ibox6a",width = 6),bs4InfoBoxOutput("ibox7a",width = 6)))),br(),
              box(width =1,
                  title = textOutput("labelboxpilihtbk"),
                  closable = FALSE,
                  enable_label = TRUE,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  uiOutput("modelpenentu")),
              box(title = textOutput("labelboxsumarymodel"),
                  closable = FALSE,
                  enable_label = TRUE,
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = F,
                  width =12,
                  DT::DTOutput("perbandinganitemfit"),
                  footer = actionButton("buttoninfors2","Step 2",icon =  icon("walking")))))),

    tabItem(tabName = "h4",
            tabBox(title = "",id = "tabset1",
                   tabPanel(textOutput("labeltabsyaunidim"),
                            fluidRow(
                              box(title=textOutput("labelboxmsa"),
                                  closable = FALSE,
                                  enable_label = TRUE,
                                  status = "danger",
                                  width = 12,
                                  solidHeader = TRUE,
                                  collapsible = FALSE,
                                  htmlOutput("kmo"),
                                  tags$head(tags$style("#kmo{color: blue;font-size: 20px;font-style: italic;alignment:left-right;}")),hr(),hr(),
                                  htmlOutput("kesimunidimensi"),
                                  tags$head(tags$style("#kesimunidimensi{color: red;font-size: 20px;font-style: italic;alignment:justify;}"))),
                              box(title=textOutput("labelboxunidimvisual"),
                                  closable = FALSE,
                                  enable_label = TRUE,
                                  status = "danger",
                                  width = 12,
                                  solidHeader = TRUE,
                                  collapsible = FALSE,
                                  plotOutput("plotunidimensi"),
                                  footer = actionButton("buttoninfor4","Step 3a",
                                                        icon =  icon("walking"))))),
                   tabPanel(textOutput("labeltabsyali"),
                            fluidRow(
                              column(width = 12,
                                     conditionalPanel(condition = "input.metodeld=='Q3'",h4("Q3 MATRIX ", style="  font-family: 'cursive';color: blue; text-align:center ")),
                                     conditionalPanel(condition = "input.metodeld=='LD'",h4("LD MATRIX ", style="  font-family: 'cursive';color: blue; text-align:center "))),
                              column(width = 12,title = "Matrix LD or Q3",
                                     DT::DTOutput("lokalindependen"),hr()),
                              column(width = 12,fluidRow(
                                box(width = 2,
                                    title = textOutput("labelmetodeld"),
                                    closable = FALSE,
                                    enable_label = TRUE,
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = FALSE,
                                    shinyWidgets::radioGroupButtons(checkIcon = list( yes = icon("square-check"),no = icon("square")),
                                                                    inputId = "metodeld",
                                                                    choices = c( "LD","Q3"),
                                                                    status = "success",
                                                                    justified = T,
                                                                    direction = "vertical",
                                                                    individual = T,size = "lg"),
                                    footer = actionButton("buttoninfor5","Step 3b",icon =  icon("walking"))),
                                box(width = 4,
                                    title = "Item Cause LD",
                                    closable = FALSE,
                                    enable_label = TRUE,
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = FALSE,
                                    tableOutput("itempairs")),
                                box(width = 6,
                                    title =  textOutput("labelldinterpret"),
                                    closable = FALSE,
                                    enable_label = TRUE,
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = FALSE,
                                    htmlOutput("ldinterpret")))))),
                   tabPanel(textOutput("labeltabsyainv"),
                            fluidRow(plotOutput("invariansiplot")),hr(),
                            fluidRow(
                              box(width = 2,
                                  title = "INTERPRETATION",
                                  closable = FALSE,
                                  enable_label = TRUE,
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = FALSE,
                                  selectInput(inputId = 'abilityok',width = "100%",
                                              label = textOutput("labelabilityok"),
                                              choices = c( "EAP","EAPSUM","MAP","ML"),
                                              selected = "EAP"),hr(),
                                  conditionalPanel(condition = "input.pilihmodel=='POLYTOMOUS'",
                                                   selectInput(inputId = 'difficultypoly',width = "100%",
                                                               label = "GenDifficulty",
                                                               choices = c( "IRF","MEAN","MEDIAN","TRIMMED"),
                                                               selected = "IRF")),
                                  footer = actionButton("buttoninfor6","Step 3c",icon =  icon("walking"))),

                              box(width = 6,
                                  title = textOutput("labelinvariankeputusan"),
                                  closable = FALSE,
                                  enable_label = TRUE,
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = FALSE,
                                  tableOutput("invariankeputusan")),
                              box(width = 4,
                                  title =textOutput("labelinvariankesimpulan"),
                                  closable = FALSE,
                                  enable_label = TRUE,
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = FALSE,
                                  htmlOutput("invariankesimpulan")))),
                   tabPanel(textOutput("labeltabsyainvmore"),
                            fluidRow(plotOutput("invarianproblem"),hr(),hr(),
                                     column(10, offset= 4,
                                            conditionalPanel(condition = "input.invparproblem=='ITEMPARAMETER'",
                                                             actionButton("left2","PREVIOUS",width=250,icon=icon("arrow-left"),class = "btn-success"),
                                                             actionButton("right2","NEXT",width=250,icon=icon("arrow-right"),class = "btn-success"))),
                                     column(width = 12,
                                            shinyWidgets::radioGroupButtons(checkIcon = list( yes = icon("square-check"),no = icon("square") ),
                                                                            inputId = "invparproblem",
                                                                            choices = c( "ITEMPARAMETER","ABILITYPARAMETER"),
                                                                            status = "primary",
                                                                            justified = T,
                                                                            direction = "horizontal",
                                                                            individual = T,size = "sm")),
                                     actionButton("buttoninfor7","Step 3d",icon =  icon("walking")))),width = 12)),
    tabItem(tabName = "h5",
            # tabBox(title = "",id = "tabset1",
            # tabPanel(textOutput("labeltabitempar"),
            fluidRow(
              box(width = 3,
                  title = textOutput("labelboxcatego"),
                  closable = FALSE,
                  enable_label = TRUE,
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  conditionalPanel(condition = "input.penentu=='3PL' ||  input.penentu=='4PL' || input.penentu=='2PL'  || input.penentu=='GRM'  || input.penentu=='GPCM' || input.penentu=='PCM' ",
                                   h5("DISCRIMINANT"),
                                   fluidRow(
                                     numericInput(inputId="lowflagdis",
                                                  label="LoDiscr",
                                                  value=NULL,
                                                  width = "50%",
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.1),
                                     numericInput(inputId="highflagdis",
                                                  label="UpDiscr",
                                                  value=NULL,
                                                  width = "50%",
                                                  min = 1,max = 2,
                                                  step = 0.1))),hr(),
                  h5("DIFFICULTY"),
                  fluidRow(
                    numericInput(inputId="easyflagdif",
                                 label="LoDifficulty",
                                 value=NULL,
                                 width = "50%",
                                 min = -4,
                                 max = 0,
                                 step = 0.1),
                    numericInput(inputId="hardflagdif",
                                 label="UpDifficulty",
                                 value=NULL,
                                 width = "50%",
                                 min = 0,
                                 max = 4,step = 0.1)),hr(),
                  conditionalPanel(condition = "input.penentu=='3PL' || input.penentu=='4PL'",
                                   h5("PSEUDO GUESSING"),
                                   fluidRow(
                                     numericInput(inputId="lowflaggues",
                                                  label="LoGuessing",
                                                  value=NULL,
                                                  width = "50%",
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.1),
                                     numericInput(inputId="highflaggues",
                                                  label="UpGuessing",
                                                  value=NULL,
                                                  width = "50%",
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.1))),
                  footer = actionButton("buttoninfor8","Step 4",icon =  icon("walking"))),
              box(
                title =textOutput("labelboxbutir"),
                closable = FALSE,
                enable_label = TRUE,
                status = "danger",
                solidHeader = TRUE,
                collapsible = FALSE,
                DT::DTOutput("modelparam"),
                uiOutput("fititemok") ,width=9))
            # ),
            # tabPanel(textOutput("labeltabsyaabipar"),
            #          fluidRow(
            #            box(width = 3,title = textOutput("labelcategoability"),closable = FALSE,enable_label = TRUE,status = "danger",solidHeader = TRUE,collapsible = FALSE,fluidRow(numericInput(inputId="lowflagabil",label="LoAbility",value=NULL,width = "180px",min = -4,max = 0,step = 0.1),numericInput(inputId="highflagabil",label="UpAbility",value=NULL,width = "180px",min = 0,max = 4,step = 0.1))),
            #            box(title = textOutput("labelmodelabliti"),closable = FALSE,enable_label = TRUE,status = "danger",solidHeader = TRUE,collapsible = FALSE,DT::DTOutput("modelabliti"),width = 9,footer = actionButton("buttoninfor9","Information",icon =  icon("info-circle"))))),width = 12)
    ),

    tabItem(tabName = "h6",
            tabBox(title = "",id = "tabset1",
                   tabPanel("Item  Characteristic Curve (ICC)",
                            fluidRow(column(width = 6,plotOutput("iccplot",height = "600px")),column(width = 6,plotOutput("iccplotempiric",height = "600px"))),
                            column(5, offset=4,actionButton("left","PREVIOUS",width=300,icon=icon("arrow-left"),class = "btn-success"),actionButton("right","NEXT",width=300,icon=icon("arrow-right"),class = "btn-success")),actionButton("buttoninfor10","Step 5a",icon =  icon("walking"))),
                   tabPanel("Item Information Curve (IIC)",
                            fluidRow(column(width = 6,plotOutput("iicitemtrace",height = "600px")),column(width = 6,plotOutput("iicitem",height = "600px"))),
                            column(5, offset=4,actionButton("left1","PREVIOUS",width=300,icon=icon("arrow-left"),class = "btn-success"),actionButton("right1","NEXT",width=300,icon=icon("arrow-right"),class = "btn-success")),actionButton("buttoninfor11","Step 5b",icon =  icon("walking"))),
                   tabPanel("Test Information Curve (TIC)",
                            fluidRow(column(width = 5,br(),br(),tableOutput("interpretifseok"),fluidRow(column(shinyWidgets::radioGroupButtons(checkIcon = list( yes = icon("square-check"),no = icon("square")),label = "Item Information",inputId = "iictestplh",choices = c( "All_ITEM","Fit_ITEM"),status = "primary",justified =T,direction = "horizontal",individual = T,size = "sm"),width = 12)),hr(),br(),br(),br(),br(),br(),br(),br(),br(),fluidRow(actionButton("buttoninfor12","Step 5c",icon =  icon("walking")),column(width = 1,""),shinyWidgets::downloadBttn('downloadReport',style = "simple",color = "primary",size = "sm"))),
                                     column(width = 7,plotOutput("iictest",height = "600px")))),width = 12)),

    tabItem(tabName = "h7",
            tabBox(title = "",id="tabset1",width = 12,
                   tabPanel("About Setting Data",icon = icon("info-circle"),
                            shinyWidgets::radioGroupButtons(checkIcon = list( yes = icon("square-check"),no = icon("square")),
                                                            inputId = "contohdata",
                                                            label = "",
                                                            choices = c("Dichotomous Parameter", "Polytomous parameter", "Scored Data"),
                                                            status = "primary",
                                                            justified = TRUE,
                                                            direction = "horizontal",
                                                            individual = TRUE,
                                                            size = "normal"),
                            conditionalPanel(condition = "input.contohdata=='Dichotomous Parameter' ",
                                             column(width = 12,
                                                    fluidRow(
                                                      h5("Instruction:",style="color:red"),
                                                      h5("if your instrument consists of 10 items, then you can write the item parameters in an excel file as in the example below. ",style="color:blue"),br(),
                                                    ),
                                                    fluidRow(
                                                      column(width = 3,align="center",
                                                             wellPanel(
                                                               h5("for 1PL or Rasch Model"),
                                                               tableOutput("onepl"))),
                                                      column(width = 3,align="center",
                                                             wellPanel(
                                                               h5("for 2PL Model"),
                                                               tableOutput("twopl"))),

                                                      column(width = 3,align="center",
                                                             wellPanel(
                                                               h5("for 3PL Model"),
                                                               tableOutput("threepl"))),
                                                      column(width = 3,align="center",
                                                             wellPanel(
                                                               h5("for 4PL Model"),
                                                               tableOutput("fourpl")))
                                                    )
                                             )
                            ),
                            conditionalPanel(condition = "input.contohdata=='Polytomous parameter' ",
                                             h5("Instruction:",style="color:red"),
                                             h5("suppose your instrument consists of 10 items and each item has 4 responses (strongly disagree, disagree, agree, strongly agree), then you can write the item parameters in the *.xlsx  file as in the example below. Given b1, b2, and b3 are the threshold, it must be written from largest to smallest for each item. ",style="color:blue"),br(),
                                             fluidRow(
                                               column(width = 3,align="center",
                                                      h5("for GRM and GPCM"),
                                                      tableOutput("poli")))
                            ),
                            conditionalPanel(condition = "input.contohdata=='Scored Data' ",
                                             h5("Instruction:",style="color:red"),
                                             h5("suppose your instruments consist of 10 items. The number of students (respondents) who will be scored is 15. then you can write the response in your *.xlsx file as in the example below. The row shows the number of students (respondents) and the column shows the number of items responded to by the respondent.",style="color:blue"),
                                             fluidRow(
                                               column(width = 6,align="center",
                                                      wellPanel(
                                                        h5("Dichotomous data"),
                                                        tableOutput("dikodata"))),
                                               column(width = 6,align="center",
                                                      wellPanel(
                                                        h5("Polytomous data"),
                                                        tableOutput("polidata"))))
                            )
                   ),



                   tabPanel("Input Data and Setting",icon = icon("upload"),

                            fluidRow(
                              box(
                                width = 3,
                                solidHeader = TRUE,
                                title = h6("Data Setting",style="align:center;font-weight:bold"),
                                headerBorder = TRUE,
                                boxToolSize = "sm",
                                collapsible = FALSE,
                                fluidRow(
                                  column(width = 3,
                                         numericInput(inputId='batastetaskor' ,
                                                      label="",
                                                      value=3,
                                                      min = 0,
                                                      max = 10,
                                                      step = 1,
                                                      width = "100%")),
                                  column(width = 9,br(),
                                         fluidRow(
                                           h6("The interval of theta score: "),
                                           textOutput("bataskorteta")
                                         )
                                  )
                                ),

                                fluidRow(
                                  column(width = 6,
                                         selectInput(inputId = 'pilihkelasskor',width = "100%",
                                                     label = "Data Type",
                                                     choices = c("Dichotomous", "Polytomous"),
                                                     selected = "Dichotomous")),
                                  column(width = 6,
                                         selectInput(inputId = 'pilihestimatelskor',width = "100%",
                                                     label = "Estimation method",
                                                     choices = c("EAP","MAP","ML","WLE","EAPsum","plausible","classify"),
                                                     selected = "EAP"))
                                ),

                                fluidRow(
                                  column(width=12,
                                         conditionalPanel(condition = "input.pilihkelasskor=='Polytomous' ",
                                                          shinyWidgets::radioGroupButtons(checkIcon = list( yes = icon("square-check"),no = icon("square") ),
                                                                                          inputId = "pilihmodelskor",
                                                                                          label = "IRT Model",
                                                                                          choices = c("GRM","GPCM"),
                                                                                          status = "primary",
                                                                                          justified = TRUE,
                                                                                          direction = "horizontal",
                                                                                          individual = TRUE,
                                                                                          size = "normal")
                                         ))
                                ),
                                column(width=12,
                                       # style="color:blue",
                                       fileInput("ambildatabaru", "Scored Data",  accept = c( "csv","xlsx","xls"),width = "100%")
                                ),
                                column(width=12,
                                       # style="color:blue",
                                       fileInput("ambildatapari", "Items Parameter",  accept = c( "csv","xlsx","xls"),width = "100%")
                                )
                              ),
                              box(
                                width = 9,
                                solidHeader = TRUE,
                                title = h6("Data View",style="align:center;font-weight:bold"),
                                headerBorder = TRUE,
                                boxToolSize = "sm",
                                collapsible = FALSE,

                                column(width = 12,
                                       wellPanel(
                                         h5("Scored Data"),
                                         DT::DTOutput("tampildataskor"))),
                                column(width = 12,
                                       wellPanel(
                                         h5("Items Parameter"),
                                         DT::DTOutput("tampildataparskor")))

                              ))
                   ),
                   tabPanel("Scoring Result",icon = icon("eye"),align="center",

                            fluidRow(
                              box(
                                width = 5,
                                solidHeader = TRUE,
                                title = h6("IRT  Score",style="align:center;font-weight:bold"),
                                headerBorder = TRUE,
                                boxToolSize = "sm",
                                collapsible = FALSE,
                                DT::DTOutput("tampilskor")
                              ),
                              box(
                                width = 2,
                                solidHeader = TRUE,
                                title = h6("Convert ",style="align:center;font-weight:bold"),
                                headerBorder = TRUE,
                                boxToolSize = "sm",
                                collapsible = FALSE,
                                fluidRow(
                                  column(width = 6,
                                         numericInput("batasbwh"," Lower ",min = -10,max = 10,step = 1,value=-3)),
                                  column(width = 6,
                                         numericInput("batasats", "upper ",min = -10,max = 10,step = 1,value = 3))),
                                shinyWidgets::actionBttn("analisis1","Convert",size = "lg",color = "success",style = "material-flat"),hr(),
                                conditionalPanel(condition = "input.analisis1!=0",
                                                 downloadButton("skordownload","download"))

                              ),
                              box(
                                width = 5,
                                solidHeader = TRUE,
                                title = h6("0-100 Score",style="align:center;font-weight:bold"),
                                headerBorder = TRUE,
                                boxToolSize = "sm",
                                collapsible = FALSE,
                                DT::DTOutput("tampilskor100")
                              )
                            ),
                            conditionalPanel(condition = "input.analisis1!=0",
                                             fluidRow(
                                               column(width=6,
                                                      h6("Histrogram of IRT Score",style="align:center;font-weight:bold"),
                                                      plotOutput("histogramirt")
                                               ),
                                               column(width=6,
                                                      h6("Histogram of 0-100 Score",style="align:center;font-weight:bold"),
                                                      plotOutput("histogramideal")
                                               )
                                             )
                            )
                   ),
                   tabPanel("Individual Analysis",

                            fluidRow(
                              column(width = 4,
                                     column(width = 12,
                                            infoBox(
                                              tabName = "cardsAPI0",
                                              title = "",
                                              value = textOutput("nama"),
                                              color = "primary",
                                              icon = icon("user"),
                                              width = 12
                                            )),
                                     column(width = 12,
                                            infoBox(
                                              tabName = "cardsAPI1",
                                              title = "",
                                              value = textOutput("skorirtsiswa"),
                                              color = "danger",
                                              icon = icon("laptop-code"),
                                              width = 12
                                            )),
                                     column(width = 12,
                                            infoBox(
                                              tabName = "cardsAPI2",
                                              title = "",
                                              value = textOutput("skoridealsiswa"),
                                              color = "success",
                                              icon = icon("book"),
                                              width = 12
                                            ))),
                              column(width = 8,
                                     box(
                                       width = 12,
                                       solidHeader = TRUE,
                                       title = h6("0-100 Score",style="align:center;font-weight:bold"),
                                       headerBorder = TRUE,
                                       boxToolSize = "sm",
                                       collapsible = FALSE,
                                       plotOutput("mapskor"),

                                       fluidRow(
                                         column(width = 6,
                                                actionButton("kembali","Previous",width="75%",style = "background-color:green;margin:5px")),
                                         column(width = 6,
                                                actionButton("lanjut","Next",width="75%",style = "background-color:green;margin:5px")))
                                     )
                              )

                            ))



            )
    )

  ))
)


