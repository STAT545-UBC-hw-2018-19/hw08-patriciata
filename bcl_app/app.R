#
# This is a Shiny web application by Patricia T. Angkiriwang, 2018/11
# Data downloaded from: https://deanattali.com/files/bcl-data.csv
# BC Liquor data compiled by Dean Attali
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

# Notes:
# Cmd+I indents code one line at a time

library(shiny)
library(shinythemes) # for use of existing/ pre-build shiny themes
library(shinyjs) # to toggle, change UI features on the fly
library(DT) # datatable package for better viz of data frames
library(tidyverse) # to filter data, ggplot2 for histogram
library(networkD3) # builds networks with support for html output

## READ IN BCL DATA --------
# Read in CSV
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)
# Get list of all drink types and countries for user input choices
drink_choices <- unique(bcl$Type)
country_choices <- unique(bcl$Country)


## DEFINE UI (user interface) for application ----------
# (note: can only R code that outputs html)

ui <- fluidPage(theme = shinytheme("paper"), # https://rstudio.github.io/shinythemes/
                useShinyjs(),  # Include shinyjs. See: https://deanattali.com/shinyjs/advanced#usage-basic
                # Title
                titlePanel("BC Liquor App",
                           windowTitle = "BC Liquor"),
                sidebarLayout( # https://shiny.rstudio.com/articles/layout-guide.html
                    # Sidebar
                    sidebarPanel(
                        sliderInput("priceInput", "Select your desired price range.",
                                    min = 0, max = 100, value = c(15, 30), pre="$"),
                        checkboxGroupInput("typeInput", "Select your alcoholic beverage type",
                                           choices = drink_choices, selected = drink_choices),
                        selectInput("countryInput", "Select your countries of interest",
                                    choices = country_choices, multiple = TRUE),
                        sliderInput("binwidthInput", "Binwidth for histogram:",
                                    min = 1, max = 15, value = 1, round = TRUE) # round to nearest integer
                    ),
                    # Main panel
                    mainPanel(
                        # Tabs
                        tabsetPanel(id = "tabs",
                            # Data tab
                            tabPanel("Explore Data",
                                     br(), # line break
                                     textOutput("selected_var"),
                                     br(),
                                     plotOutput("price_hist"),
                                     dataTableOutput("bcl_data",width="70%")
                                     ),
                            # Network visualization tab
                            tabPanel("Network Visualization",
                                     forceNetworkOutput("network_viz")
                                     )
                        )
                    )
                )
)

## DEFINE SERVER LOGIC TO FEED INTO UI --------
server <- function(input, output, session) {

    observeEvent(input$tabs, { # Do this if tabs selected changes
        # Only show binwidth slider only if the data tab is selected
        if (input$tabs=="Explore Data"){
            show("binwidthInput")  # shinyjs function
        } else{
            hide("binwidthInput")  # shinyjs function
        }

    })

    observeEvent(input$priceInput,{ # Do this if priceInput changes
        # The maximum on the binwidth slider should only reach 0.5*range of data
        # (ie. histogram should have at least two bins)
        updateSliderInput(session, "binwidthInput", max = (input$priceInput[2]-input$priceInput[1])/2)
    })

    bcl_filtered <- reactive({ # Filter data, store as reactive variable
        # Only evaluate the dataframe if at least one drink type is selected
        if (length(input$typeInput)>0){
            if (length(input$countryInput)>0){ # if at least one country is selected
                data <- bcl %>% filter(Price < input$priceInput[2],
                                       Price > input$priceInput[1],
                                       Type == input$typeInput,
                                       Country %in% input$countryInput)
            }
            else { # if no countries selected, use all countries (don't filter)
                data <- bcl %>% filter(Price < input$priceInput[2],
                                       Price > input$priceInput[1],
                                       Type == input$typeInput)
            }
            # But if the filtered data frame is empty, return NULL
            if (nrow(data)==0){
                data <- NULL
            }
        } else { # If no drinks selected, return NULL
            data <- NULL
        }
    return(data)
    }) # note that any calls to this variable (now a function) needs to have brackets: "bcl_filtered()"

    countries_text <- reactive({ # Creative reactive variable to use in output$selectedVar
        if (length(input$countryInput)>0){
            paste0(input$countryInput, collapse = ", ") # country inputs
        } else { # display "all countries" if no particular countries are selected
            "ALL COUNTRIES"
        }
    })

    output$selected_var <- renderText({ # Text output to display (what's selected?)
        if (!is.null(bcl_filtered())){ # Do not display if filtered data = NULL
            paste0("Displaying histogram of ", paste0(input$typeInput,collapse=", "), # alcohol type inputs
                   " in the price range $", paste0(input$priceInput,collapse="-"), # price inputs
                   " from ", countries_text(), # use reactive text above for countries selected
                   ".")
        }
    })

    output$price_hist <- renderPlot( # Price histogram
        if (!is.null(bcl_filtered())){ # Do not display if filtered data = NULL
            bcl_filtered() %>% # Create histogram
                ggplot(aes(Price)) +
                geom_histogram(binwidth = input$binwidthInput, # set binwidth according to slider
                               boundary = input$priceInput[1]) + # set bins to start at LHS of histogram
                xlim(input$priceInput[1],input$priceInput[2])
        }
    )

    output$bcl_data <- renderDataTable( # Data frame (use DT for display)
        bcl_filtered() #https://shiny.rstudio.com/articles/datatables.html
    )

    network_graph <- reactive({ # Create network graph as reactive variable
        NULL
        if (!is.null(bcl_filtered())){ # Only do this if filtered data is not NULL
            ## PROCESS DATA FOR NETWORK VISUALIZATION --------
            # Combine alcohol Type and Subtype if not redundant
            # (e.g. into "SPIRITS - DARK", but not "WINE - TABLE WINE RED")
            network_data <- bcl_filtered() %>%
                mutate(Drink = ifelse(Type %in% Subtype, Subtype, str_c(Type," - ",Subtype)))

            drinks <- network_data %>%
                distinct(Drink) %>%
                rename(name = Drink) %>%
                mutate(group = "Drink")

            countries <- network_data %>%
                distinct(Country) %>%
                rename(name = Country) %>%
                mutate(group = "Country")

            nodes <- full_join(countries, drinks, by = c("name","group")) %>%
                # Create an ID for each node
                rowid_to_column("id") %>% mutate(id=id-1) %>% # Subtract 1: numbering with Network D3 needs to start from 0
                # Create new column for node size
                mutate(plot_size = ifelse(group=="Country",2, 1))

            edges <- network_data %>%
                group_by(Country, Drink) %>%
                # Weight each link by the number of that subtype associated with that country
                summarise(weight = n()) %>%
                ungroup() %>%
                # Switch names of countries to ID numbers
                left_join(nodes, by = c("Country" = "name")) %>%
                rename(from = id) %>%
                # Switch names of alcohols to ID numbers
                left_join(nodes, by = c("Drink" = "name")) %>%
                rename(to = id) %>%
                select(from, to, weight)

            # Create force directed network plot
            forceNetwork(Links = edges, Nodes = nodes,
                         Source = 'from', Target = 'to', Value='weight',
                         NodeID = 'name', Group = 'group', charge = -20,
                         Nodesize = 'plot_size', radiusCalculation = JS("3*(1+d.nodesize)"),
                         fontSize=12, fontFamily = "sans-serif", legend = TRUE, bounded = TRUE,
                         opacityNoHover = 0.7, opacity = 0.7, linkColour = "#999",
                         linkDistance = 70,
                         colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"))
        }
    })

    output$network_viz <- renderForceNetwork( # Network graph
        network_graph()
    )
}

## RUN THE APPLICATION ------
shinyApp(ui = ui, server = server)

