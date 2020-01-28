library(shiny)
library(shinyalert)
library(bs4Dash)
library(plotly)
library(readxl)
library(shinyWidgets)

ui <- bs4DashPage(navbar = bs4DashNavbar(skin = "light", status = "white", 
                                         leftUi = fluidRow(icon("check-double"), h4("Respondents Satisfaction Index Analysis", style="font-weight:bold;color:#349eeb;margin-top:3px;")),
                                         rightUi = fluidRow(downloadBttn(outputId = "template", label = "Template", style = "unite", color = "default", size = "xs", block = FALSE, no_outline = TRUE), actionBttn(inputId = "faq", label = NULL, style = "unite", icon = icon("question-circle"), color = "default", size = "xs"), style="margin-right:5px;")
                                         ), 
                  sidebar = bs4DashSidebar(inputId = "tabs", disable = TRUE), 
                  body = bs4DashBody(
                    useShinyalert(),
                    br(),
                    fluidRow(
                      column(3,
                             bs4Card(width = 12, status = "primary", closable = FALSE, collapsible = FALSE,
                                     title = "Data",
                                                      # downloadBttn(outputId = "template", label = "Template", class = "btn btn-info", 
                                                      #              style = "height: 35px;background:#fff;color:#4dc3eb;font-weight:bold;")),
                                     fileInput("data", NULL, width = "100%", buttonLabel = "Upload...",
                                               accept = c('.xls', '.xlsx')
                                               ),
                                     uiOutput("sheets")
                                     )
                             ),
                      column(3,
                             uiOutput("index")
                             ),
                      column(6,
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
                awesomeCheckbox(inputId = "firstrow", label = "First row as column name", value = TRUE, width = "100%"),
                awesomeCheckbox(inputId = "firstcol", label = "First column as Respondent ID", value = TRUE, width = "100%"),
                selectInput("performance", "Performance Sheet", choices = excel_sheets(input$data$datapath), selected = excel_sheets(input$data$datapath)[2]),
                selectInput("importance", "Importance Sheet", choices = excel_sheets(input$data$datapath), selected = excel_sheets(input$data$datapath)[1]),
                actionBttn(inputId = "submit", label = "Process", block = TRUE,
                           style = "simple", color = "primary", icon = icon("cogs"), size = "sm")
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
        bs4Card(width = 12, height = 420, title = "Index", status = "primary", closable = FALSE, collapsible = FALSE,
                bs4InfoBox(value = h3(round(avg$Performance, 2), style = "font-weight:bold;"), 
                           title = "Performance Index", width = 12, 
                           status = "primary", icon = "chart-line"),
                br(),
                bs4InfoBox(value = h3(round(avg$Importance, 2), style = "font-weight:bold;"), 
                           title = "Importance Index", width = 12, 
                           status = "success", icon = "check-double"),
                br(),
                bs4InfoBox(value = h3(paste0(round(avg$Conformity*100, 2), "%"), style = "font-weight:bold;"), 
                           title = "Conformity", width = 12, 
                           status = "info", icon = "thumbs-up"),
                br()
        )
      } else {
        return(NULL)
      }
    })
  })
  
  
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
      ggplotly(plots(), height = 360, width = 600)  %>% 
        # layout(height = 360, width = 600) %>% 
        config(displayModeBar = FALSE)
    }
  })
  
  observeEvent(input$submit, {
    output$quadrant <- renderUI({
      if(!is.null(ic())){
        bs4Card(width = 12, height = 420, 
                title = "Quadrant", status = "primary", closable = FALSE, collapsible = FALSE,
                fluidRow(uiOutput("savebtn"), uiOutput("savetblui")),
                div(style="margin-top:5px;", plotlyOutput("plot"))
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
                     style = "unite", color = "default", size = "xs", )
      }
    })
  })

  observeEvent(input$submit, {
    # req(input$data)
    output$savetblui <- renderUI({
      if(is.null(ic())){
        return(NULL)
      } else {
        downloadBttn(outputId = "savetbl", label = "Save Table", 
                     style = "unite", color = "default", size = "xs", )
      }
    })
  })
  
  output$savepng <- downloadHandler(
    filename = "quadplot.png", 
    content = function(con){
      ggsave(filename = con, plot = plots())
    }
  )
  
  output$template <- downloadHandler(
    filename = "questionnaire.xlsx",
    content = function(con){
      file.copy("questionnaire.xlsx", to = con)
    }
  )
  
  output$savetbl <- downloadHandler(
    filename = "Result.csv",
    content = function(con){
      write.csv(ic()$Result, file = con, row.names = FALSE)
    }
  )
  
  observeEvent(input$faq, {
    shinyalert(title = "Get Started",
               text = "<ul style = 'text-align:left;list-style-type:disc;'>
               <li>Input file must an Excel (*.xls or *.xlsx) file.</li> 
               <li>At least consist of two sheets.</li> 
               <li>Each sheet represent data for performance and importance survey result.</li>
               </ul>
               <br/>",
               type = "warning", html = TRUE
               )
    })
}

shinyApp(ui, server)
