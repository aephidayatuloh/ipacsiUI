library(shiny)
library(shinyalert)
library(bs4Dash)
library(DT)
library(plotly)
library(readxl)
library(shinyWidgets)

ui <- bs4DashPage(navbar = bs4DashNavbar(skin = "light", status = "white", 
                                         leftUi = fluidRow(icon("check-double"), h4("Respondents Satisfaction Index Analysis", style="font-weight:bold;color:#349eeb;margin-top:3px;")),
                                         rightUi = actionBttn(inputId = "faq", label = NULL, style = "jelly", icon = icon("question-circle"), color = "primary", size = "sm")
                                         ), 
                  sidebar = bs4DashSidebar(inputId = "tabs", disable = TRUE), 
                  body = bs4DashBody(
                    useShinyalert(),
                    br(),
                    fluidRow(
                      column(3,
                             bs4Card(width = 12, title = "Data", status = "primary", closable = FALSE,
                                     fileInput("data", "Upload Data", width = "100%",
                                               accept = c('.xls', '.xlsx')
                                               ),
                                     column(12, 
                                            # fluidRow(
                                                     awesomeCheckbox(inputId = "firstrow", label = "First row as column name", value = TRUE, width = "100%"),
                                                     awesomeCheckbox(inputId = "firstcol", label = "First column as Respondent ID", value = TRUE, width = "100%")
                                            # )
                                     ),
                                     column(12,
                                            uiOutput("sheets")
                                     ),
                                     actionBttn(inputId = "submit", label = "Process", block = TRUE,
                                                style = "simple", color = "primary", icon = icon("cogs"))
                                     )
                             ),
                      column(2,
                             uiOutput("index")
                             ),
                      column(7,
                             uiOutput("quadrant")
                             )
                      
                    )
                  ), 
                  controlbar = NULL, 
                  footer = bs4DashFooter(copyrights = HTML("&copy; 2020 Aep Hidayatuloh")), 
                  title = "Customer Satisfaction and  Importance-Performance Index Analysis", 
                  old_school = FALSE, 
                  sidebar_mini = FALSE, 
                  sidebar_collapsed = TRUE, 
                  controlbar_collapsed = TRUE, 
                  enable_preloader = FALSE)

server <- function(input, output, session){
  source("ipacsi.R")
  source("quadplot.R")
  
  observeEvent(input$data, {
    updateSelectInput(session, "performance", "Performance Sheet", choices = excel_sheets(input$data$datapath), selected = excel_sheets(input$data$datapath)[2])
    updateSelectInput(session, "importance", "Importance Sheet", choices = excel_sheets(input$data$datapath), selected = excel_sheets(input$data$datapath)[1])
    
  })
  ic <- eventReactive(input$submit, {
    if(is.null(input$data)){
      shinyalert(type = "error", title = "Ooops", text = "There is no data uploaded")
      return(NULL)
    } else if(input$performance == input$importance){
      shinyalert(type = "error", title = "Ooops", text = "Performance and Importance sheet name can not be the same.")
      return(NULL)
    } else {
      shinyalert(type = "", title = "Please wait...", 
                 text = "Your request will be done in minutes", 
                 closeOnEsc = FALSE, closeOnClickOutside = FALSE, 
                 showConfirmButton = FALSE)
      ic <- ipacsi(input$data$datapath, performance = input$performance, importance = input$importance, 
             firstrow = input$firstrow, firstcol = input$firstcol)
      
      shinyalert(type = "success", title = "Done!", 
                 text = "", timer = 3*1000,
                 closeOnEsc = FALSE, closeOnClickOutside = FALSE)
      return(ic)
    }
  })
  
  observeEvent(input$data, {
    output$sheets <- renderUI({
      if(!is.null(input$data)){
        tagList(
          selectInput("performance", "Performance Sheet", choices = excel_sheets(input$data$datapath), selected = excel_sheets(input$data$datapath)[2]),
          selectInput("importance", "Importance Sheet", choices = excel_sheets(input$data$datapath), selected = excel_sheets(input$data$datapath)[1])
        )
      } else {
        return(NULL)
      }
    })
  })
  
  observeEvent(input$submit, {
    output$index <- renderUI({
      if(!is.null(ic())){
        avg <- ic()$Average
        tagList(
          bs4ValueBox(value = h3(round(avg$Performance, 2), style = "font-weight:bold;"), 
                     subtitle = "Performance Index", width = 12, 
                     status = "primary", icon = "chart-line"),
          bs4ValueBox(value = h3(round(avg$Importance, 2), style = "font-weight:bold;"), 
                     subtitle = "Importance Index", width = 12, 
                     status = "success", icon = "check-double"),
          bs4ValueBox(value = h3(paste0(round(avg$Conformity*100, 2), "%"), style = "font-weight:bold;"), 
                     subtitle = "Conformity", width = 12, 
                     status = "info", icon = "thumbs-up")
          
        )
      } else {
        return(NULL)
      }
    })
  })
  
  
  # output$perf <- renderInfoBox({
  #   req(input$data)
  #   avg <- ic()$Average
  #   perf <- avg$Performance
  #   # avg
  #   bs4InfoBox(value = h3(round(perf, 2), style = "font-weight:bold;"), 
  #              title = "Performance Index", width = 4, 
  #              status = "primary", icon = "chart-line")
  # })
  # 
  # output$imp <- renderInfoBox({
  #   req(input$data)
  #   avg <- ic()$Average
  #   imp <- avg$Importance
  #   # avg
  #   bs4InfoBox(value = h3(round(imp, 2), style = "font-weight:bold;"), 
  #              title = "Importance Index", width = 4, 
  #              status = "success", icon = "check-double")
  # })
  # 
  # output$conf <- renderInfoBox({
  #   req(input$data)
  #   avg <- ic()$Average
  #   conf <- avg$Conformity
  #   # avg
  #   bs4InfoBox(value = h3(paste0(round(conf*100, 2), "%"), style = "font-weight:bold;"), 
  #              title = "Conformity", width = 4, 
  #              status = "info", icon = "thumbs-up")
  # })
  
  plots <- reactive({
    if(is.null(ic())){
      return(NULL)
    } else {
      quadplot(ic())
    }
  })
  
  output$plot <- renderPlotly({
    if(is.null(ic())){
      return(NULL)
    } else {
      # g <- quadplot(ic())
      ggplotly(plots()) %>% 
        config(displayModeBar = FALSE)
        # plotly::config(displaylogo = FALSE, 
        #                modeBarButtonsToRemove = c("select2d", "zoom2d", "zoomIn2d", "lasso2d", "autoScale2d", 
        #                                           "toggleSpikelines", "hoverCompareCartesian", "hoverClosestCartesian"))
    }
  })
  
  observeEvent(input$submit, {
    output$quadrant <- renderUI({
      if(!is.null(ic())){
        bs4Card(width = 12, title = "Quadrant", status = "primary", closable = FALSE,
                uiOutput("savebtn"),
                plotlyOutput("plot")
        )
      } else {
        return(NULL)
      }
    })
  })
  
  observeEvent(input$submit, {
    # req(input$data)
    output$savebtn <- renderUI({
      if(is.null(ic())){
        return(NULL)
      } else {
        downloadBttn(outputId = "savepng", label = "Save Plot", 
                     style = "bordered", color = "primary", size = "sm")
      }
    })
  })
  
  output$savepng <- downloadHandler(
    filename = "quadplot.png", 
    content = function(con){
      ggsave(filename = con, plot = plots())
    }
  )
  
  observeEvent(input$faq, {
    shinyalert(title = "Get Started",
               text = "<ul style = 'text-align:left;list-style-type:disc;'>
               <li>Input file must an Excel (*.xls or *.xlsx) file.</li> 
               <li>It at least consist of two sheets.</li> 
               <li>Each sheet represent data for performance and importance survey result.</li>
               </ul>",
               type = "warning", html = TRUE
               )
    })
}

shinyApp(ui, server)
