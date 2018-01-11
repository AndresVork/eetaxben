#Andres Võrk
#Viimati uuendatud 10.01. 2018
#Kasutab võimalusel EUROMODi Eesti mudeli tähiseid

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

###########################################################
#Parameetrid
###########################################################

#Töötuskindlustusmakse	1,60%
SICEE_RateUI_2017 <- 0.016
SICEE_RateUI_2018 <- 0.016
#Kogumispensionimakse	2%
SIC_RatePension2_2017 <- 0.02
SIC_RatePension2_2018 <- 0.02
#Maksuvabatulu kuus	500
IT_BasicAlw_2017 <- 2160
IT_BasicAlw_2018 <- 6000
#Pensionide maksuvabatulu aastas (kuus 236 eurot)
IT_PensionAlw_2017 <- 2832
IT_PensionAlw_2018 <- 0
#Tulumaksumäär	20%
IT_StdRate_2017 <- 0.2
IT_StdRate_2018 <- 0.2
#Alumine piir	1200
IT_Thresh1_2018 <- 14400
#Ülemine piir	2100
IT_Thresh2_2018 <- 25200
#arvutuslik tulumaksuvabamiinimum aastas
tinta_s_2018 <- function(x) {
  IT_BasicAlw <- IT_BasicAlw_2018
  IT_Thresh1 <- IT_Thresh1_2018
  IT_Thresh2 <- IT_Thresh2_2018
  
  tinta_s = ifelse(x<=IT_BasicAlw, x, 
                   ifelse(x<=IT_Thresh1, IT_BasicAlw, 
                          ifelse(x<=IT_Thresh2, IT_BasicAlw - IT_BasicAlw/(IT_Thresh2-IT_Thresh1)*(x-IT_Thresh1), 0))) 
  return(tinta_s)
}

tinta_s_2017 <- function(x) {
  IT_BasicAlw <- IT_BasicAlw_2017
  tinta_s = ifelse(x<=IT_BasicAlw, x, IT_BasicAlw) 
  return(tinta_s)
}
#töötuskindlustusmakse
tsceeui_s_2017 <- function(x){
  SICEE_RateUI <- SICEE_RateUI_2017
  tsceeui_s <- x*SICEE_RateUI
  return(tsceeui_s)
}
tsceeui_s_2018 <- function(x){
  SICEE_RateUI <- SICEE_RateUI_2018
  tsceeui_s <- x*SICEE_RateUI
  return(tsceeui_s)
}

#pensionikindlustusmakse
tpceepi_s_2017 <- function(x, lpm=1){
  SIC_RatePension2 <- SIC_RatePension2_2017*lpm
  tpceepi_s <- x*SIC_RatePension2
  return(tpceepi_s)
}
tpceepi_s_2018 <- function(x, lpm=1){
  SIC_RatePension2 <- SIC_RatePension2_2018*lpm
  tpceepi_s <- x*SIC_RatePension2
  return(tpceepi_s)
}
#kinnipeetud tulumaksu suurus, võtab sisendiks sissetuleku ja II sambas osalemise 
tinwh_s_2018 <- function(x, lpm=1) {
  IT_StdRate <- IT_StdRate_2018
  tsceeui_s <- tsceeui_s_2018(x)
  tpceepi_s <- tpceepi_s_2018(x, lpm)
  tinta_s <- tinta_s_2018(x)
  #maksustatav tulu
  x_sc =  x - tsceeui_s - tpceepi_s
  #tulumaks
  tinwh_s = ifelse(x_sc<=tinta_s,0, (x_sc-tinta_s)*IT_StdRate)
  return(tinwh_s)
}
tinwh_s_2017 <- function(x, lpm=1) {
  IT_StdRate <- IT_StdRate_2017
  tsceeui_s <- tsceeui_s_2017(x)
  tpceepi_s <- tpceepi_s_2017(x, lpm)
  tinta_s <- tinta_s_2017(x)
  #maksustatav tulu
  x_sc =  x - tsceeui_s - tpceepi_s
  #tulumaks
  tinwh_s = ifelse(x_sc<=tinta_s,0, (x_sc-tinta_s)*IT_StdRate)
  return(tinwh_s)
}

netopalk_s <- function(x, Year, lpm=1) {
  x - get(paste0("tsceeui_s_", Year))(x) - get(paste0("tpceepi_s_", Year))(x, lpm)  - get(paste0("tinwh_s_", Year))(x, lpm)
}

margtaxwh_s <- function(x, Year, lpm=1) {
  #kui x muutub ühe ühiku võrra, kui palju võetakse ära
  margtax = (1- (netopalk_s(x+1, Year, lpm)-netopalk_s(x, Year, lpm)) / 1)*100
  return(margtax)
}

#keskmine tulumaksumäär brutopalgast
avetaxwh_s = function(x, Year, lpm) {
  avetax= get(paste0("tinwh_s_", Year))(x, lpm)/x*100
  return(avetax)
}

#Lõplik tulumaks, aastane arvestus, võtab sisendiks kolme liiki tulud:
# palgatulud, teised tulud, kust võetakse veel tulumaksu, kolmandad tulud, kust ei x-i ja  maksuvaba miinimumi funktsiooni?
tin_s_2018 <- function(yem, yotx=0, yont=0, lpm=1) {
  IT_StdRate <- IT_StdRate_2018
  tsceeui_s <- tsceeui_s_2018(yem)
  tpceepi_s <- tpceepi_s_2018(yem, lpm)
  tinta_s <- tinta_s_2018(yem+yotx+yont)
  #maksustatav tulu
  x_sc =  yem+yotx - tsceeui_s - tpceepi_s
  #tulumaks
  tin_s = ifelse(x_sc<=tinta_s,0, (x_sc-tinta_s)*IT_StdRate)
  return(tin_s)
}

tin_s_2017 <- function(yem, yotx=0, yont=0, lpm=1) {
  IT_StdRate <- IT_StdRate_2017
  tsceeui_s <- tsceeui_s_2017(yem)
  tpceepi_s <- tpceepi_s_2017(yem, lpm)
  tinta_s <- tinta_s_2017(yem+yotx+yont)
  #maksustatav tulu
  x_sc =  yem+yotx - tsceeui_s - tpceepi_s
  #tulumaks
  tin_s = ifelse(x_sc<=tinta_s,0, (x_sc-tinta_s)*IT_StdRate)
  return(tin_s)
}

netotulu_s <- function(yem, yotx=0, yont=0, Year, lpm=1) {
  yem+yotx+yont - get(paste0("tsceeui_s_", Year))(yem) - get(paste0("tpceepi_s_", Year))(yem, lpm)  - get(paste0("tin_s_", Year))(yem, yotx, yont, lpm)
}

margtax_s <- function(yem, yotx=0, yont=0, Year, lpm=1) {
  delta=1
  #kui x muutub ühe ühiku võrra, kui palju võetakse ära
  margtax = (1- (netotulu_s(yem+delta, yotx, yont, Year, lpm)-netotulu_s(yem, yotx, yont, Year, lpm)) / delta)*100
  return(margtax)
}

#keskmine tulumaksumäär kogu maksustatavast sissetulekust
avetax_s = function(yem, yotx=0, yont=0, Year, lpm=1) {
  avetax= get(paste0("tin_s_", Year))(yem, yotx, yont, lpm)/(yem+yotx+yont)*100
  return(avetax)
}


############################################################
#teen praegu pikalt, võiks muidugi panna kokku ülemisega
#pensionäri maksutulu, töötaja töötuskindlustusmakset ei ole, II sammast võib maksta, kui juba ei saa kätte II sammast
tintapens_s_2017 <- function(pension) {
  #pensionidelt maksuvabastus, mas pensioni suurus või summa
  tintapens_s = ifelse(pension<=IT_PensionAlw_2017, pension, IT_PensionAlw_2017)
  return(tintapens_s)
}
#2018 ei ole eraldi, peaks andma nulli automaatselt
tintapens_s_2018 <- function(pension) {
  #pensionidelt maksuvabastus, mas pensioni suurus või summa
  tintapens_s = ifelse(pension<=IT_PensionAlw_2018, pension, IT_PensionAlw_2018)
  return(tintapens_s)
}
#kokku maksuvabastus,
tintap_s_2017 <- function(yem, pension) {
  #pensionidelt maksuvabastus, mas pensioni suurus või summa
  tintap_s = ifelse(pension<=IT_PensionAlw_2017, pension, IT_PensionAlw_2017)
  #töötasult maksuvabastus, kasutab ära töötasu ja selle pensioni osa, mis jäi üle pensionide maksuvabastuset
  tinta_s = ifelse(yem+max((pension-IT_PensionAlw_2017),0)<=IT_BasicAlw_2017, yem+max((pension-IT_PensionAlw_2017),0), IT_BasicAlw_2017) 
  return(tintap_s+tinta_s)
}
#2018 on samamoodi kui kõigil teistel, dubleerin nime pärast
tintap_s_2018 = tinta_s_2018

#kinni peetud tulumaks 2017, vaikimisi ei maksa enam II sambasse
tinwh_s_pensionar_2017 <- function(yem, pension, lpm=0) {
  IT_StdRate <- IT_StdRate_2017
  tpceepi_s <- tpceepi_s_2017(yem, lpm)
  tinta_s <- tintap_s_2017(yem, pension)
  #maksustatav tulu
  x_sc =  yem + pension - tpceepi_s
  #tulumaks
  tinwh_s = ifelse(x_sc<=tinta_s,0, (x_sc-tinta_s)*IT_StdRate)
  return(tinwh_s)
}
#kinnipeetud tulumaks 2018
tinwh_s_pensionar_2018 <- function(yem, pension, lpm=0) {
  IT_StdRate <- IT_StdRate_2018
  tpceepi_s <- tpceepi_s_2018(yem, lpm)
  tinta_s <- tintap_s_2018(yem+pension)
  #maksustatav tulu
  x_sc =  yem + pension - tpceepi_s
  #tulumaks
  tinwh_s = ifelse(x_sc<=tinta_s,0, (x_sc-tinta_s)*IT_StdRate)
  return(tinwh_s)
}
#praegu ei luba muid tulusid, ehkki saaks
netotulupensionar_s_2017 <- function(yem, pension, lpm=0) {
  yem+pension - tpceepi_s_2017(yem, lpm) - tinwh_s_pensionar_2017(yem, pension, lpm)
}
netotulupensionar_s_2018 <- function(yem, pension, lpm=0) {
  yem+pension - tpceepi_s_2018(yem, lpm) - tinwh_s_pensionar_2018(yem, pension, lpm)
}

avetaxwh_pensionar_s = function(yem, pension, Year, lpm) {
avetax= get(paste0("tinwh_s_pensionar_", Year))(yem, pension, lpm)/(yem+pension)*100
return(avetax)
}

margtaxwh_pensionar_s <- function(yem, pension, Year, lpm=0) {
  #kui yem muutub ühe ühiku võrra, kui palju võetakse ära
  margtax = (1- (get(paste0("netotulupensionar_s_", Year))(yem+1, pension, lpm)-get(paste0("netotulupensionar_s_", Year))(yem, pension, lpm)) / 1)*100
  return(margtax)
}



#################################################
ui <- fluidPage(
   
   # Pealkiri
   titlePanel("Väike maksukalkulaator"),
   
   navbarPage("",
              
   tabPanel("Kuupalga näitel",
            
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("kuupalk", "Sisesta kuu brutopalk", value = 1100, min=0, max = 100000) ,
        #liitunud II sambaga
        radioButtons("lpm", "Kas liitunud II sambaga?", choices = c("Jah" = 1 , "Ei" = 0), selected = 1)
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        DT::dataTableOutput("table1kuu"),
         br(),
         tags$em("Töötamise piirmaksumäär - kui palju võetakse maksudena (sotsiaalkindlustusmaksed + tulumaks) ära ühelt täiendavalt brutopalgana teenitud eurolt"),
         br(),
         tags$em("Keskmine tulumaksumäär - makstud tulumaksu suhe brutopalka"),
         br(),
         br(),
         plotOutput("joonis1kuu"), 
         br(),
         br(),
          plotOutput("joonis2kuu"),
         br(),
         br(),
          plotOutput("joonis3kuu")
         
      ) #end of mainPanel
   ) #end of sidebarLayout
   #end of tab
   ),
   
   tabPanel("Aastatulu näitel",
          sidebarLayout(
              sidebarPanel(
                numericInput("aastapalk", "Sisesta aasta brutopalk", value = 13200, min=0, max = 100000) ,
                #liitunud II sambaga
                radioButtons("aastalpm", "Kas liitunud II sambaga?", choices = c("Jah" = 1 , "Ei" = 0), selected = 1),
                numericInput("aastayotx", "Sisesta muud Eestis tulumaksuga maksustavad brutotulud aastas kokku
                              (nt vanemahüvitis, haigushüvitis, esivanemate metsa müük)", value = 0, min=0, max = 100000),
                numericInput("aastayont",
                             "Sisesta muud tulumaksuga Eestis mitte-maksustatavad tulud aastas kokku, kuid mida võetakse arvesse tulumaksuvaba tulu arvestamisel
                             (nt dividendid, välismaal teenitud tulu)", value = 0, min=0, max = 100000)
              ),
              
              mainPanel(
                DT::dataTableOutput("table1aasta"),
                br(),
                tags$em("Töötamise piirmaksumäär - kui palju võetakse maksudena (sotsiaalkindlustusmaksed + tulumaks) ära ühelt täiendavalt brutopalgana teenitud eurolt"),
                br(),
                tags$em("Keskmine tulumaksumäär - makstud tulumaksu suhe brutotulude summasse"),
                br(),
                br(),
                plotOutput("joonis1aasta")
                
              ) #end of mainPanel
            ) #end of sidebar
            #end of tab
   ),
   
   tabPanel("Pensionäri kuusissetuleku näitel",
            sidebarLayout(
              sidebarPanel(
                numericInput("kuupalkpension", "Sisesta kuu brutopalk", value = 1100, min=0, max = 100000) ,
                #liitunud II sambaga
                radioButtons("lpmpension", "Kas veel maksab II sambasse?", choices = c("Jah" = 1 , "Ei" = 0), selected = 0),
                #liitunud II sambaga
                numericInput("pension", "Sisesta kuu brutopension", value = 400, min=0, max = 10000)
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                #tableOutput("table1pension"),
                DT::dataTableOutput("table1pension"),
                br(),
                tags$em("Töötamise piirmaksumäär - kui palju võetakse maksudena (sotsiaalkindlustusmaksed + tulumaks) ära ühelt täiendavalt brutopalgana teenitud eurolt"),
                br(),
                tags$em("Keskmine tulumaksumäär - makstud tulumaksu suhe brutopalka"),
                br(),
                br(),
                plotOutput("joonis1pension"),
                br(),
                br(),
                plotOutput("joonis2pension"),
                br(),
                br(),
                plotOutput("joonis3pension")

                
              ) #end of mainPanel
            ) #end of sidebarLayout
    #end of tabPanel        
   ),
   
   tabPanel("Kontakt",
            
            mainPanel(
              "Mõeldud abivahendiks mõistmaks Eesti tulumaksusüsteemi. Ei pretendeeri absoluutsele tõele.",
              "Koostaja ei võta mingit vastutust, kui selle kasutaja satub stressi või maksuvõlgadesse.",
              "Selle rakenduse kood on kättesaadav ka ", tags$a(href="https://github.com/AndresVork/eetaxben", "githubis."),
              "Teata vigadest või ettepanekutest: andres.vork@ut.ee")
   #end of navbarpage
   )
   
   )
)

#################################################################################

server <- function(input, output) {
  
  output$table1kuu <- DT::renderDataTable( {
   lpm <- as.integer(input$lpm)
      
    tunnused <- c("Brutopalk",
                "Töötaja töötuskindlustusmakse (eur)",
                "Töötaja kogumispensionimakse (eur)",
                "Maksuvaba tulu palga järgi kuus (eur)",
                "Tulumaks palgalt kuus (eur)",
                "Netopalk kuus (eur)",
                "Töötamise piirmaksumäär (%)", 
                "Keskmine tulumaksumäär (%)"
    )
    aasta1 <- 2017
    veerg1 <- c(input$kuupalk,
                round(get(paste0("tsceeui_s_", aasta1))(input$kuupalk*12)/12,2),
                round(get(paste0("tpceepi_s_", aasta1))(input$kuupalk*12, lpm)/12,2),
                round(get(paste0("tinta_s_", aasta1))(input$kuupalk*12)/12,2),
                round(get(paste0("tinwh_s_", aasta1))(input$kuupalk*12, lpm)/12,2),
                round(netopalk_s(input$kuupalk*12, aasta1, lpm)/12,2),
                round(margtaxwh_s(input$kuupalk*12, aasta1, lpm),1),
                round(avetaxwh_s(input$kuupalk*12, aasta1, lpm),1)
    )
    aasta2 <- 2018
    veerg2 <- c(input$kuupalk,
                round(get(paste0("tsceeui_s_", aasta2))(input$kuupalk*12)/12,2),
                round(get(paste0("tpceepi_s_", aasta2))(input$kuupalk*12, lpm)/12,2),
                round(get(paste0("tinta_s_", aasta2))(input$kuupalk*12)/12,2),
                round(get(paste0("tinwh_s_", aasta2))(input$kuupalk*12, lpm)/12,2),
                round(netopalk_s(input$kuupalk*12, aasta2, lpm)/12,2),
                round(margtaxwh_s(input$kuupalk*12, aasta2, lpm),1),
                round(avetaxwh_s(input$kuupalk*12, aasta2, lpm),1)
    )
    
    df <- as.data.frame(cbind(tunnused, veerg1, veerg2, round(veerg2-veerg1,2)))  
    colnames(df) <- c("Näitaja", "2017", "2018", "Muutus")
    datatable(df, options = list(dom='t', ordering = FALSE), rownames=FALSE) %>% formatStyle("Näitaja",target= "row", backgroundColor=styleEqual("Netopalk kuus (eur)", "lightgrey"))
  })
  
  output$joonis1kuu <- renderPlot({
    z = seq(0, 2500, 1)
    df <- data.frame(z, 
                     maksuvabatulu2018=tinta_s_2018(z*12)/12, 
                     maksuvabatulu2017=tinta_s_2017(z*12)/12)
    ggplot(df) + 
      geom_line(aes(x=z, y=maksuvabatulu2017, color="2017")) +
      geom_line(aes(x=z, y=maksuvabatulu2018, color="2018"))+
      scale_x_continuous(breaks=c(0, 500, 1200, 2100, 2500)) +
      labs(x="Brutopalk kuus", y="Euro",  title="Maksuvaba tulu kuus") +
      scale_color_manual(name = "", values = c("2018" = "blue", "2017"="black")) +
      geom_point(aes(x=input$kuupalk, y=tinta_s_2018(input$kuupalk*12)/12), color="red", size=4)
  })
  
#
 output$joonis2kuu <- renderPlot({
   lpm <- as.integer(input$lpm)
   z = seq(0, 2500, 1)
   df <- data.frame(z, 
                    piirmaksumaar2018=margtaxwh_s(z*12, Year=2018, lpm =lpm), 
                    piirmaksumaar2017=margtaxwh_s(z*12, Year=2017, lpm =lpm))
   
   ggplot(df) + 
     geom_line(aes(x=z, y=piirmaksumaar2017, color="2017")) +
     geom_line(aes(x=z, y=piirmaksumaar2018, color="2018"))+
     scale_x_continuous(breaks=c(0, 500, 1200, 2100, 2500)) +
    labs(x="Brutopalk kuus", y="%",  title="Piirmaksumäär") +
     scale_color_manual(name = "", values = c("2018" = "blue", "2017"="black")) +
      geom_point(aes(x=input$kuupalk, y=margtaxwh_s(input$kuupalk*12, Year=2018, lpm=lpm)), color="red", size=4)
  })
 
 
output$joonis3kuu <- renderPlot({
   lpm <- as.integer(input$lpm)
   x = seq(0, 2500, 1)
   df <- data.frame(x, 
                    keskminemaksumaar2018=avetaxwh_s(x*12, Year=2018, lpm =lpm), 
                    keskminemaksumaar2017=avetaxwh_s(x*12, Year=2017, lpm =lpm))
   
   ggplot(df) + 
     geom_line(aes(x=x, y=keskminemaksumaar2017, color="2017")) +
     geom_line(aes(x=x, y=keskminemaksumaar2018, color="2018"))+
     scale_x_continuous(breaks=c(0, 500, 1200, 2100, 2500)) +
     labs(x="Brutopalk kuus", y="%",  title="Keskmine tulumaksumäär",
          caption = "Lõplik tulumaksumäär sõltub ka mahaarvamistest, muudest tuludest jmt") +
     scale_color_manual(name = "", values = c("2018" = "blue", "2017"="black")) +
     geom_point(aes(x=input$kuupalk, y=avetaxwh_s(input$kuupalk*12, Year=2018, lpm=lpm)), color="red", size=4)

 })
 
 # ############################################
 #aasta osa

 output$table1aasta <- DT::renderDataTable( {
   lpm <- as.integer(input$aastalpm)

   tunnused <- c("Brutotulu kokku aastas",
                 "Töötaja töötuskindlustusmakse (eur)",
                 "Töötaja kogumispensionimakse (eur)",
                 "Maksuvaba tulu aastas (eur)",
                 "Tulumaks kokku aastas (eur)",
                 "Netosissetulek kokku aastas (eur)",
                 "Töötamise piirmaksumäär (%)",
                 "Keskmine tulumaksumäär (%)"
   )
   aasta1 <- 2017
   veerg1 <- c(input$aastapalk+input$aastayotx+input$aastayont,
               round(get(paste0("tsceeui_s_", aasta1))(input$aastapalk),0),
               round(get(paste0("tpceepi_s_", aasta1))(input$aastapalk, lpm),0),
               round(get(paste0("tinta_s_", aasta1))(input$aastapalk+input$aastayotx+input$aastayont),0),
               round(get(paste0("tin_s_", aasta1))(yem=input$aastapalk, yotx = input$aastayotx, yont= input$aastayont, lpm),0),
               round(netotulu_s(yem=input$aastapalk, yotx = input$aastayotx, yont= input$aastayont, aasta1, lpm),0),
               round(margtax_s(yem=input$aastapalk, yotx = input$aastayotx, yont= input$aastayont, aasta1, lpm),1),
               round(avetax_s(yem=input$aastapalk, yotx = input$aastayotx, yont= input$aastayont, aasta1, lpm),1)
   )
   aasta2 <- 2018
   veerg2 <- c(input$aastapalk+input$aastayotx+input$aastayont,
               round(get(paste0("tsceeui_s_", aasta2))(input$aastapalk),0),
               round(get(paste0("tpceepi_s_", aasta2))(input$aastapalk, lpm),0),
               round(get(paste0("tinta_s_", aasta2))(input$aastapalk+input$aastayotx+input$aastayont),0),
               round(get(paste0("tin_s_", aasta2))(yem=input$aastapalk, yotx = input$aastayotx, yont= input$aastayont, lpm),0),
               round(netotulu_s(yem=input$aastapalk, yotx = input$aastayotx, yont= input$aastayont, aasta2, lpm),0),
               round(margtax_s(yem=input$aastapalk, yotx = input$aastayotx, yont= input$aastayont, aasta2, lpm),1),
               round(avetax_s(yem=input$aastapalk, yotx = input$aastayotx, yont= input$aastayont, aasta2, lpm),1)

   )

   df <- as.data.frame(cbind(tunnused, veerg1, veerg2, round(veerg2-veerg1,2)))  
   colnames(df) <- c("Näitaja", "2017", "2018", "Muutus")
   datatable(df, options = list(dom='t', ordering = FALSE), rownames=FALSE) %>% formatStyle("Näitaja",target= "row", backgroundColor=styleEqual("Netosissetulek kokku aastas (eur)", "lightgrey"))
 })

 output$joonis1aasta <- renderPlot({
   z = seq(0, 30000, 10)
   df <- data.frame(z,
                    maksuvabatulu2018=tinta_s_2018(z),
                    maksuvabatulu2017=tinta_s_2017(z))
   ggplot(df) +
     geom_line(aes(x=z, y=maksuvabatulu2017, color="2017")) +
     geom_line(aes(x=z, y=maksuvabatulu2018, color="2018"))+
     scale_x_continuous(breaks=c(0, 500*12, 1200*12, 2100*12, 2500*12)) +
     labs(x="Sissetulek aastas", y="Euro",  title="Maksuvaba tulu aastas") +
     scale_color_manual(name = "", values = c("2018" = "blue", "2017"="black")) +
     geom_point(aes(x=input$aastapalk+input$aastayotx+input$aastayont, y=tinta_s_2018(input$aastapalk+input$aastayotx+input$aastayont)), color="red", size=4)
 })


 #
 output$joonis2aasta <- renderPlot({
   lpm <- as.integer(input$aastalpm)
   z = seq(0, 30000, 10)
   df <- data.frame(z,
                    piirmaksumaar2018=margtax_s(yem=z, yotx = input$aastayotx, yont= input$aastayont, Year=2018, lpm=lpm),
                    piirmaksumaar2017=margtax_s(yem=z, yotx = input$aastayotx, yont= input$aastayont, Year=2017, lpm=lpm))

   ggplot(df) +
     geom_line(aes(x=z, y=piirmaksumaar2017, color="2017")) +
     geom_line(aes(x=z, y=piirmaksumaar2018, color="2018"))+
     scale_x_continuous(breaks=c(0, 500*12, 1200*12, 2100*12, 2500*12)) +
     labs(x="Sissetulek aastas", y="%",  title="Piirmaksumäär") +
     scale_color_manual(name = "", values = c("2018" = "blue", "2017"="black")) +
     geom_point(aes(x=input$aastapalk+input$aastayotx+input$aastayont, y=margtax_s(yem=input$aastapalk, yotx = input$aastayotx, yont= input$aastayont, Year=2017, lpm=lpm)), color="red", size=4)
 })

 output$joonis3aasta <- renderPlot({
   lpm <- as.integer(input$aastalpm)
   z = seq(0, 30000, 10)
   df <- data.frame(z,
                    keskminemaksumaar2018=avetax_s(yem=z-input$aastayotx-input$aastayont, yotx = input$aastayotx, yont= input$aastayont, Year=2018, lpm=lpm),
                    keskminemaksumaar2017=avetax_s(yem=z-input$aastayotx-input$aastayont, yotx = input$aastayotx, yont= input$aastayont, Year=2017, lpm=lpm))

   ggplot(df) +
     geom_line(aes(x=z, y=keskminemaksumaar2017, color="2017")) +
     geom_line(aes(x=z, y=keskminemaksumaar2018, color="2018"))+
     scale_x_continuous(breaks=c(0, 500*12, 1200*12, 2100*12, 2500*12)) +
     labs(x="Sissetulek aastas", y="%",  title="Keskmine tulumaksumäär",
          caption = "Lõplik tulumaksumäär sõltub ka mahaarvamistest, muudest tuludest jmt") +
     scale_color_manual(name = "", values = c("2018" = "blue", "2017"="black")) +
     geom_point(aes(x=input$aastapalk+input$aastayotx+input$aastayont, y=avetax_s(yem=input$aastapalk, yotx = input$aastayotx, yont= input$aastayont, Year=2018, lpm=lpm)), color="red", size=4)

 })
 
 
 ##################################
 #Pensionäride osa
 ##################################
 
  output$table1pension <- DT::renderDataTable( {
   lpm <- as.integer(input$lpmpension)
   
   tunnused <- c("Brutopalk",
                 "Brutopension",
                 "Töötaja kogumispensionimakse (eur)",
                 "Rakenduv maksuvaba tulu pensionidelt (eur)",
                 "Rakenduv maksuvaba tulu kokku (eur)",
                 "Tulumaks kokku kuus (eur)",
                 "Netotulu kuus (eur)",
                 "Töötamise piirmaksumäär (%)", 
                 "Keskmine tulumaksumäär (%)"
   )
   aasta1 <- 2017
   veerg1 <- c(input$kuupalkpension,
               input$pension,
               round(get(paste0("tpceepi_s_", aasta1))(input$kuupalkpension*12, lpm)/12,2),
               round(get(paste0("tintapens_s_", aasta1))(input$pension*12)/12,2),
               round(get(paste0("tintap_s_", aasta1))(input$kuupalkpension*12, input$pension*12)/12,2),
               round(get(paste0("tinwh_s_pensionar_", aasta1))(input$kuupalkpension*12, input$pension*12, lpm)/12,2),
               round(netotulupensionar_s_2017(input$kuupalkpension*12, input$pension*12, lpm)/12,2),
               round(margtaxwh_pensionar_s(input$kuupalkpension*12, input$pension*12, aasta1, lpm),1),
               round(avetaxwh_pensionar_s(input$kuupalkpension*12, input$pension*12, aasta1, lpm),1)
   )
   aasta2 <- 2018
   veerg2 <- c(input$kuupalkpension,
               input$pension,
               round(get(paste0("tpceepi_s_", aasta2))(input$kuupalkpension*12, lpm)/12,2),
               round(get(paste0("tintapens_s_", aasta2))(input$pension*12)/12,2),
               round(get(paste0("tintap_s_", aasta2))(input$kuupalkpension*12+input$pension*12)/12,2),
               round(get(paste0("tinwh_s_pensionar_", aasta2))(input$kuupalkpension*12, input$pension*12, lpm)/12,2),
               round(netotulupensionar_s_2018(input$kuupalkpension*12, input$pension*12, lpm)/12,2),
               round(margtaxwh_pensionar_s(input$kuupalkpension*12, input$pension*12, aasta2, lpm),1),
               round(avetaxwh_pensionar_s(input$kuupalkpension*12, input$pension*12, aasta2, lpm),1)
   )
   
   df <- as.data.frame(cbind(tunnused, veerg1, veerg2, round(veerg2-veerg1,2)))  
   colnames(df) <- c("Näitaja", "2017", "2018", "Muutus")
   datatable(df, options = list(dom='t', ordering = FALSE), rownames=FALSE) %>% formatStyle("Näitaja",target= "row", backgroundColor=styleEqual("Netotulu kuus (eur)", "lightgrey"))
 })
 
 
 output$joonis1pension <- renderPlot({
   z = seq(0, 2500, 1)
   df <- data.frame(z, 
                    maksuvabatulu2018=tintap_s_2018(z*12+input$pension*12)/12, 
                    maksuvabatulu2017=tintap_s_2017(z*12,input$pension*12)/12)
   ggplot(df) + 
     geom_line(aes(x=z, y=maksuvabatulu2017, color="2017")) +
     geom_line(aes(x=z, y=maksuvabatulu2018, color="2018"))+
     scale_x_continuous(breaks=c(0, 500, 1200, 2100, 2500)) +
     labs(x="Brutopalk kuus", y="Euro",  title=paste0("Maksuvaba tulu kokku pensioni ", input$pension, " eurot kuus korral sõltuvana töötasust")) +
     scale_color_manual(name = "", values = c("2018" = "blue", "2017"="black")) +
     geom_point(aes(x=input$kuupalkpension, y=tintap_s_2018(input$kuupalkpension*12+input$pension*12)/12), color="red", size=4)
 })
 
 
 output$joonis2pension <- renderPlot({
   lpm <- as.integer(input$lpmpension)
   z = seq(0, 2500, 1)
   df <- data.frame(z, 
                    piirmaksumaar2018=margtaxwh_pensionar_s(yem=z*12, pension=input$pension*12, Year=2018, lpm =lpm), 
                    piirmaksumaar2017=margtaxwh_pensionar_s(yem=z*12, pension=input$pension*12, Year=2017, lpm =lpm))
   
   ggplot(df) + 
     geom_line(aes(x=z, y=piirmaksumaar2017, color="2017")) +
     geom_line(aes(x=z, y=piirmaksumaar2018, color="2018"))+
     scale_x_continuous(breaks=c(0, 500, 1200, 2100, 2500)) +
     labs(x="Brutopalk kuus", y="%",  title=paste0("Piirmaksumäär pensioni ", input$pension, " eurot kuus korral sõltuvana töötasust")) +
     scale_color_manual(name = "", values = c("2018" = "blue", "2017"="black")) +
     geom_point(aes(x=input$kuupalkpension, y=margtaxwh_pensionar_s(input$kuupalk*12, pension=input$pension*12, Year=2018, lpm=lpm)), color="red", size=4)
 })
 
 output$joonis3pension <- renderPlot({
   lpm <- as.integer(input$lpmpension)
   x = seq(0, 2500, 1)
   df <- data.frame(x, 
                    keskminemaksumaar2018=avetaxwh_pensionar_s(yem=x*12, pension=input$pension*12, Year=2018, lpm =lpm), 
                    keskminemaksumaar2017=avetaxwh_pensionar_s(yem=x*12, pension=input$pension*12, Year=2017, lpm =lpm))
   
   ggplot(df) + 
     geom_line(aes(x=x, y=keskminemaksumaar2017, color="2017")) +
     geom_line(aes(x=x, y=keskminemaksumaar2018, color="2018"))+
     scale_x_continuous(breaks=c(0, 500, 1200, 2100, 2500)) +
     labs(x="Brutopalk kuus", y="%",  title=paste0("Keskmine tulumaksumäär pensioni ", input$pension, " eurot kuus korral sõltuvana töötasust"),
          caption = "Lõplik tulumaksumäär sõltub ka mahaarvamistest, muudest tuludest jmt") +
     scale_color_manual(name = "", values = c("2018" = "blue", "2017"="black")) +
     geom_point(aes(x=input$kuupalkpension, y=avetaxwh_pensionar_s(yem=input$kuupalkpension*12, pension=input$pension*12, Year=2018, lpm=lpm)), color="red", size=4)
   
 })
 
 
}

# Run the application 
shinyApp(ui = ui, server = server)


