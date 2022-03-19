#load the relevant libraries
library(shiny)  #for web applications
library(shinydashboard)
library(DataExplorer)
library(scales)
library(dplyr)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(treemapify)
library(plotly)


#set the working directory
setwd("C:/Users/monic/Desktop")

# read the data from the directory
terrorism <- read.csv("globalterrorism.csv",stringsAsFactors = FALSE, header = TRUE)


############################################################################################################################
# The customizations for this dashboard are : 3 value boxes, two ggplots, dashboard icon on the left #
############################################################################################################################

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "GLOBAL TERRORISM(1970-2019)", titleWidth=350)  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
   
  )
  )
  
frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)


frow2 <- fluidRow(
  
  box(
    title = "Total Attacks Vs Total Fatalities"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("totalattacksanddeaths", height = "350px")
  )
  
  ,box(
    title = "Top Targets"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("toptargets", height = "350px")
  ) 
)


# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='blue')


# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values on the boxes on top
  total.attacks <- nrow(terrorism)
  total.wounds <- sum(terrorism[, 'nwound'], na.rm = TRUE)
  total.kills <- sum(terrorism[, 'nkill'], na.rm = TRUE)
  
  options(dplyr.summarise.inform = FALSE)
  
  Totalattacks<-terrorism %>%
    group_by(iyear) %>%
    summarise(iyear,attacks = count(eventid))
  
  Totalattacks<-terrorism %>%
    group_by(iyear) %>%
    filter(!is.na(eventid)) %>% 
    dplyr::summarise(eventid = n())
  
   totalkilled <- terrorism %>%
   # select(iyear, gname,nkill)%>%
    group_by(iyear) %>%
    filter(!is.na(nkill)) %>% 
    dplyr::summarise(nkill=sum(nkill))
   
   
   
   totalattackskilled <- merge(Totalattacks,totalkilled,by="iyear")
   
   
   totaltargets <- terrorism %>%
     select(targtype1_txt,nkill)%>%
     group_by(targtype1_txt,nkill) %>%
     filter(!is.na(nkill)) %>% 
     dplyr::summarise(nkill=sum(nkill))
   
   
   totaltargets1 <- totaltargets %>%
     select(targtype1_txt,nkill)%>%
     group_by(targtype1_txt) %>%
     filter(!is.na(targtype1_txt)) %>% 
     dplyr::summarise(nkill=sum(nkill))
   

  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.attacks, format="d", big.mark=',')
      ,'Total Attacks'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total.wounds, format="d", big.mark=',')
      ,'Total Wounded'
      ,icon = icon("stats",lib='glyphicon')
     ,color = "yellow")
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(total.kills, format="d", big.mark=',')
      ,'Total Fatalities'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")
  })
  
  #creating the plotOutput content

  output$totalattacksanddeaths <- renderPlotly({
    
     ggplotly(ggplot(data=totalattackskilled, aes(x=iyear, y=eventid, group = 1,
                                             text = paste("Year: ", iyear,
                                                          "<br>Total Incidents: ", eventid,
                                                          "<br>Total Deaths: ", nkill))) +
      geom_bar(stat="identity",fill = "tan1")+
      geom_line(aes(x=iyear, y=nkill),color="red")+
      scale_y_continuous(sec.axis = sec_axis(~ ., name = "Number of Deaths")) +
      labs(y = "Number of Attacks/Fatalities", x = "Year")+
      annotate("segment", x = 2008, xend = 2014, y = 40000, yend = 44524, colour = "red") +
      annotate("text", x = 1997, y = 38500, label = "Total Fatalities Peaked in year 2014",colour = "red",size=3)+
      annotate("segment", x = 1975, xend = 1985, y = 15000, yend = 2900, colour = "orange") +
      annotate("text", x = 1975, y = 16000, label = "Total Attacks",colour = "orange",size=3),tooltip = "text")
    
#    ggplotly(p,tooltip = "text")
     
  })
  
  output$toptargets <- renderPlot({
      ggplot(totaltargets1, aes(area = nkill, fill = nkill,label= paste(targtype1_txt,"\n", prettyNum(nkill,big.mark=",")))) +
        geom_treemap()+theme(legend.position = "none")+
      geom_treemap_text(colour="white",place="center",size=15)+
      scale_fill_gradient(low = "tan2", high = "red2")
    })
}
  
shinyApp(ui, server)

