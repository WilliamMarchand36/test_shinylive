#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# shinylive::export(appdir = "myApp", destdir = "docs")
# httpuv::runStaticServer("docs/", port=8008)

library(shiny)
library(sf)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(forcats)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinydashboard)


fr_contours <- ne_countries(country = 'france', scale = "small", returnclass = 'sf')
fr_contours_proj <- st_transform(fr_contours, 27572)

data_placette <- read.csv("data_plots_2008_2022_all_v2.csv", header = TRUE)
modalites_tauxgui <- read.csv("Modalites_TAUXGUIT.csv", header = TRUE, sep = ";")

# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$link(rel = "stylesheet", type = "text/css", href = "mistle_style.css"),
  
    # Application title
    titlePanel("Mistletoe in french forests"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        selectInput(
          "ssp",
          "Choose the mistletoe subspecies",
          choices = c("album", "abietis", "austriacum")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(box(plotlyOutput("distPlot")))
          ,
          fluidRow(box(plotOutput("distMap")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  select_data_barplot <- select_data <- reactive({
    if(input$ssp == "album"){
      subdata <- data_placette[!(data_placette$u_txguialbum %in% c("X")), c("npp", "xl", "yl", "u_txguialbum")]
      names(subdata)[4] <- "tx_gui"
      subdata_sf <- st_as_sf(subdata, coords = c("xl", "yl"), crs = 27572)
    }else if(input$ssp == "abietis"){
      subdata <- data_placette[!(data_placette$u_txguiabiet %in% c("X")), c("npp", "xl", "yl", "u_txguiabiet")]
      names(subdata)[4] <- "tx_gui"
      subdata_sf <- st_as_sf(subdata, coords = c("xl", "yl"), crs = 27572)
    }else{
      subdata <- data_placette[!(data_placette$u_txguiaustr %in% c("X")), c("npp", "xl", "yl", "u_txguiaustr")]
      names(subdata)[4] <- "tx_gui"
      subdata_sf <- st_as_sf(subdata, coords = c("xl", "yl"), crs = 27572)
    }
    return(subdata_sf)
  })
  
    output$distPlot <- renderPlotly({
      req(input$ssp)
      
        data <- subset(select_data_barplot(), tx_gui != "0")
        data <- left_join(data, modalites_tauxgui, by = c("tx_gui" = "mode"))
        data$tx_gui <- as.numeric(data$tx_gui)
        data$libelle <- factor(data$libelle,
                               levels = unique(data$libelle[order(data$tx_gui)])
                               )
        # draw the histogram with the specified number of bins
        p <- ggplot(data, aes(y=fct_rev(libelle)))+
          geom_bar(aes(fill=..count..), color = "black")+
          scale_fill_gradient(name = "%", low="white", high="red3")+
          labs(x="Plot number", y="% infested trees")+
          theme_bw()
        
        ggplotly(p)
    })
    
    outputOptions(output, "distPlot", priority = 10)
    
    output$distMap <- renderPlot({
      
      data <- select_data()
      # data <- left_join(data, modalites_tauxgui, by = c("tx_gui" = "mode"))
      # data$tx_gui <- as.numeric(data$tx_gui)
      # data$libelle <- factor(data$libelle,
      #                        levels = unique(data$libelle[order(data$tx_gui)])
      # )
      data$is_gui <- ifelse(data$tx_gui == 0, "absent", "present")
      data$is_gui <- factor(data$is_gui, levels = c("absent", "present"))
      data <- data[order(data$is_gui), ]
      
      p <- ggplot(data)+
        geom_sf(aes(color = is_gui))+
        scale_color_manual(name = "mistletoe", values = c("absent" = "grey75", "present" = "orangered"))+
        geom_sf(data=fr_contours_proj, fill = "transparent")+
        coord_sf(xlim = c(75100, 1194200), ylim = c(1621600, 2671600))+
        theme_minimal()+
        theme(axis.text = element_blank())
      p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
