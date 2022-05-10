#
# This is a Shiny web application.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders) #for loading screen
library(colourpicker) #for color options
library(tuneR) #for loading WAV
library(tidyverse) #for ggplot and other tools
library(here) #for unbreakable paths
library(seewave) #for transforming sound
library(viridis) #for color palette

# Options for Spinner

options(spinner.color="#0275D8", 
        spinner.color.background="#ffffff", 
        spinner.size=0.3)

### UI
ui <- 
    
    
    
    fluidPage(

    # Application title
    titlePanel("seewave GUI (dev)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "soundfile", 
                "Upload your WAV file", 
                accept = ".wav",
                placeholder = "single-channel WAV file up to 300MB")
            ,
            selectInput("graph",
                        "Choose an output:", 
                        choices = c("Oscillogram", "Spectrogram", "Power Spectrum (average)"))
            ,
            helpText("You can fine-tune with the options below",
                     "or just clik on 'Update' to render your plot")
            ,
            #sliderInput("lenght",
                        #"Time range (s)",
                        #min = 0, 
                        #max = 2,
                        #value = c(0,2),
                        #step = 0.01,
                        #ticks = F)
            #,
            conditionalPanel(
                condition = "input.graph == 'Spectrogram' || input.graph == 'Power Spectrum (average)'",
                selectInput("wn", "Window:", c("Bartlett" = "bartlett",
                                               "Blackman" = "blackman",
                                               "Flattop" = "flattop",
                                               "Hamming" = "hamming",
                                               "Hanning" = "hanning",
                                               "Rectangle" = "rectangle"),
                            selected = "hamming")
            ,
            numericInput("wl", "Window lenght (bins):",
                         512,
                         min = 64,
                         max = 2048,
                         step = 64,)
        ,
        sliderInput("ovlp",
                     "Overlap (%)",
                     value = 90,
                     50,
                     99,
                     step = 1,
                     ticks = FALSE)
            )
        ,
        selectInput("thm", "Theme:", c("White" = "white",
                                       "Dark" = "black",
                                       "Custom" = "custom"),
                    selected = "white")
        ,
        conditionalPanel(
            condition = "input.thm == 'custom'",
        colourInput("bkgcol", 
                    "Background color",
                    "white",
                    palette = "limited")
        ,
        colourInput("lncol", 
                    "Lines & Text color",
                    "black",
                    palette = "limited")
        )
        ,
        conditionalPanel(
            condition = "input.thm == 'custom' && input.graph != 'Spectrogram'",
        colourInput("fillcol", 
                    "Fill color",
                    "black",
                    palette = "limited")
        )
        ,
        conditionalPanel(
            condition = "input.thm == 'custom' && input.graph == 'Spectrogram'",
        selectInput("specfill", "Palette",
                    c("Magma" = "A",
                      "Inferno" = "B",
                      "Plasma" = "C",
                      "Viridis" = "D",
                      "Cividis" = "E",
                      "Rocket" = "F",
                      "Mako" = "G",
                      "Turbo" = "H"
        )
        )
        ,
        checkboxInput("invertfill",
                      "Invert palette",
                      value = FALSE,
                      width = NULL)
        )
        ,
        actionButton("update",
                     "Update",
                     icon("refresh")
                     )
        ,
        helpText("Render your plot")
        )
    ,
        # plot output
        mainPanel(
            tabsetPanel(
                tabPanel("Plot",
            withSpinner(
                plotOutput("plot"),
                type = 5),#spinner style
            #downloadButton(outputId = "dpdf", 
             #              label = " PDF",
              #             icon = shiny::icon("download"))
            ),
           # tabPanel("Measure",
            #         helpText("Coming soon!"))
        )
    )
))

### Server
server <- function(input, output) {
    options(shiny.maxRequestSize = 300*1024^2)
    
    output$keepAlive <- renderText({
        req(input$count)
        paste("keep alive ", input$count)
    })
   
     oscil <- eventReactive(input$update,{
        
         wav <- readWave(input$soundfile$datapath)
         
         par(bg = ifelse(input$thm == "custom",
                         input$bkgcol,
                         ifelse(input$thm == "white",
                                "white", "black")))
        oscillo(wav, 
                f = wav@samp.rate,
                from = NULL, 
                to = NULL,
                tlab = "Time (s)",
                alab = "Amplitude",
                fastdisp = T,
                colwave = ifelse(input$thm == "custom",
                                 input$fillcol,
                                 ifelse(input$thm == "white",
                                        "black", "white")), 
                coltitle = NULL,
                cextitle = 1.2,
                fonttitle = 2,
                collab = ifelse(input$thm == "custom",
                                input$lncol,
                                ifelse(input$thm == "white",
                                       "black", "white")),
                cexlab = 1,
                fontlab = 1,
                colline = ifelse(input$thm == "custom",
                                 input$lncol,
                                 ifelse(input$thm == "white",
                                        "black", "white")),
                colaxis = ifelse(input$thm == "custom",
                                 input$lncol,
                                 ifelse(input$thm == "white",
                                        "black", "white")),
                cexaxis = 1,
                fontaxis = 1,
                coly0 = ifelse(input$thm == "custom",
                               input$lncol,"gray"),
                bty = "o")
    })

    spect <- eventReactive(input$update,{
        
        wav <- readWave(input$soundfile$datapath)
        
        ggspectro(wave = wav,
                  f = wav@samp.rate,
                  ovlp=input$ovlp,
                  wl = input$wl,
                  wn = input$wn,
                  fftw = FALSE,
                  fastdisp = T,
                  norm = T)+ 
            scale_x_continuous(expand = c(0,0))+
            scale_y_continuous(expand = c(0,0), position = "left")+
            geom_raster(aes(fill=amplitude), hjust = 0, vjust = 0, interpolate = T)+
            scale_fill_gradientn(colours = viridis(10,
                                                   alpha = 1,
                                                   begin = 0,
                                                   end = 1,
                                                   direction = ifelse(input$invertfill == T,
                                                                      -1,
                                                                      1),
                                                   option = ifelse(input$thm == "custom",
                                                                   input$specfill,
                                                                   "D")),
                                 name = "Amplitude \n (dB)",
                                 na.value = "transparent",
                                 limits = c(-45,0))+
            labs(title = NULL,
                 subtitle = NULL,
                 caption = NULL)+
            theme(panel.grid.major.y = element_line(color=ifelse(input$thm == "custom",
                                                                 input$lncol,
                                                                 ifelse(input$thm == "white",
                                                                        "black", "white")),
                                                    linetype = "dotted"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(fill=ifelse(input$thm == "custom",
                                                              input$bkgcol,
                                                              ifelse(input$thm == "white",
                                                                     "white", "black"))),
                  panel.border = element_rect(linetype = "solid", fill = NA, color = ifelse(input$thm == "custom",
                                                                                            input$lncol,
                                                                                            ifelse(input$thm == "white",
                                                                                                   "black", "white"))),
                  axis.line = element_blank(),
                  legend.background = element_rect(fill=ifelse(input$thm == "custom",
                                                               input$bkgcol,
                                                               ifelse(input$thm == "white",
                                                                      "white", "black"))),
                  legend.key.width = unit(30, "native"),
                  legend.title = element_text(size=16, color=ifelse(input$thm == "custom",
                                                                    input$lncol,
                                                                    ifelse(input$thm == "white",
                                                                           "black", "white"))),
                  legend.text = element_text(size=16, color=ifelse(input$thm == "custom",
                                                                   input$lncol,
                                                                   ifelse(input$thm == "white",
                                                                          "black", "white"))),
                  plot.background = element_rect(fill=ifelse(input$thm == "custom",
                                                             input$bkgcol,
                                                             ifelse(input$thm == "white",
                                                                    "white", "black"))),
                  plot.margin = margin(1,0,1,1, "lines"),
                  axis.title = element_text(size=16, color = ifelse(input$thm == "custom",
                                                                    input$lncol,
                                                                    ifelse(input$thm == "white",
                                                                           "black", "white"))),
                  axis.text = element_text(size=16, color = ifelse(input$thm == "custom",
                                                                   input$lncol,
                                                                   ifelse(input$thm == "white",
                                                                          "black", "white"))),
                  axis.text.x = element_text(size=14, color = ifelse(input$thm == "custom",
                                                                     input$lncol,
                                                                     ifelse(input$thm == "white",
                                                                            "black", "white"))),
                  axis.ticks = element_line(color=ifelse(input$thm == "custom",
                                                         input$lncol,
                                                         ifelse(input$thm == "white",
                                                                "black", "white"))),
                  plot.title = element_text(size=18, color = ifelse(input$thm == "custom",
                                                                    input$lncol,
                                                                    ifelse(input$thm == "white",
                                                                           "black", "white"))),
                  plot.subtitle = element_text(size=14, color = input$lncol, face = "italic"),
                  plot.caption = element_text(size=11, color = input$lncol, vjust = -1, hjust = 1.50, face = "italic")
            )
    })
    
    pwrspect <- eventReactive(input$update,{
        
        wav <- readWave(input$soundfile$datapath)
        
        par(bg = ifelse(input$thm == "custom",
                             input$bkgcol,
                             ifelse(input$thm == "white",
                                    "white", "black")))
        ms<-meanspec(wave = wav,
                 f = wav@samp.rate,
                 ovlp=input$ovlp,
                 wl = input$wl,
                 wn = input$wn,
                 fftw = FALSE,
                 fastdisp = T,
                 norm = T,
             at = NULL, from = NULL, to = NULL,
             identify = FALSE,
             col = ifelse(input$thm == "custom",
                          input$fillcol,
                          ifelse(input$thm == "white",
                                 "black", "white")),
             cex = 1,
             flab = "Frequency (kHz)",
             alab = "Amplitude",
             flim = NULL,
             alim = NULL,
             col.lab = ifelse(input$thm == "custom",
                              input$fillcol,
                              ifelse(input$thm == "white",
                                     "black", "white")),
             col.axis = ifelse(input$thm == "custom",
                               input$fillcol,
                               ifelse(input$thm == "white",
                                      "black", "white")),
             fg = ifelse(input$thm == "custom",
                         input$fillcol,
                         ifelse(input$thm == "white",
                                "black", "white")))
        polygon(rbind(c(0, 0), ms), col = ifelse(input$thm == "custom",
                                                 input$fillcol,
                                                 ifelse(input$thm == "white",
                                                        "black", "white")))
    })
    
    # Return the requested graph
    graphInput <- reactive({
        switch(input$graph,
               "Oscillogram" = oscil(),
               "Spectrogram" = spect(),
               "Power Spectrum (average)" = pwrspect()
        )
    })
    
    output$plot <- renderPlot({
        p <- graphInput()
        print(p)
    })
    #output$dpdf <- downloadHandler(
     #   filename = function() { 
      #      paste('plot', '.png', sep='')
       # },
        #content = function(file) {
         #   png(file)
          #  print(graphInput())
           # dev.off()
    #    },
     #   contentType="image/png")
}

# Run the application 
shinyApp(ui = ui, server = server)
