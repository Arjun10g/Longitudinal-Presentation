library(shiny)
library(tidyverse)
library(lme4)
# ui.R

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #333;
          color: #fff;
        }
        .navbar {
          background-color: #222;
        }
        .navbar-default .navbar-nav > li > a {
          color: #fff;
        }
        .navbar-default .navbar-brand {
          color: #fff;
        }
        .sidebar {
          background-color: #444;
        }
        .well {
          background-color: #555;
        }
        .btn-primary {
          background-color: #007bff;
          border-color: #007bff;
        }
        .btn-primary:hover {
          background-color: #0056b3;
          border-color: #0056b3;
        }
        .btn-danger {
          background-color: #dc3545;
          border-color: #dc3545;
        }
        .btn-danger:hover {
          background-color: #bd2130;
          border-color: #bd2130;
        }
        #resultText_n {
          font-size: 18px;
          font-weight: bold;
          color: #dc3545; /* Red color for emphasis */
          text-align:center;
          padding-top:20%;
        }
      ")
    )
  ),
  titlePanel("Longitudinal Models"),
  tabsetPanel(
    tabPanel('FAQ',
             div(class = "faq-item",
                 h4("What is this app about?"),
                 p("This Shiny app demonstrates how to create an interactive interface for exploring and comparing linear mixed-effects models (MLM) and linear models (LM) for longitudinal data."),
                 p("Users can adjust parameters like the number of timepoints, subjects, and variations to observe the impact on the generated data and model outputs.")
             ),
             
             div(class = "faq-item",
                 h4("How do I use the Longitudinal Modelling tab?"),
                 p("Navigate to the 'Longitudinal Modelling' tab to explore and compare MLM and LM."),
                 p("Adjust sliders for 'Timepoint Count,' 'Number of Subjects,' 'Subject Intercept Variation,' 'Subject Slope Variation,' and 'Residual Variation' to modify the generated data."),
                 p("Click the 'Show Output' button to display the mixed-effects model random effects and linear model residual standard error.")
             ),
             
             div(class = "faq-item",
                 h4("What do the output tables represent?"),
                 p("The 'out_compare' table displays the estimates and standard errors for both the MLM and LM models, including intercepts and slopes."),
                 p("The 'rand' output shows the random effects for the MLM and the residual standard error for the LM.")
             ),
             
             div(class = "faq-item",
                 h4("How can I customize this app?"),
                 p("Feel free to modify the code to add more features or improve the visual appearance."),
                 p("You can extend the FAQ, include additional tabs, or enhance the user interface based on your specific requirements.")
             )
             
                 
                ),
    tabPanel("Longitudinal Modelling",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("timepoint", "Timepoint Count",min = 3,max = 30,value = 3,step = 1),
                 sliderInput("subjects", "Number of Subjects",min = 2,max = 100,value = 2,step = 1),
                 sliderInput("intercept", "Subject Intercept Variation", min = 0.1,max = 5,value = 1,step = 0.5),
                 sliderInput("slope", "Subject Slope Variation", min = 0.1,max = 5,value = 1,step = 0.5),
                 sliderInput("residual", "Residual Variation", min = 0.1,max = 5,value = 1,step = 0.5),
                 actionButton("showOutputBtn", "Show Output", class = "btn-primary"),
                 width = 12),
               mainPanel(
                 tableOutput("out_compare"),
                 verbatimTextOutput('rand'),
                 width = 12
               )
             )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$showOutputBtn,{
    data <- reactive({
      n <- input$timepoint * input$subjects
      intercepts <- rnorm(input$subjects,mean = 0 ,sd = input$intercept)
      slopes <- rnorm(input$subjects, mean = 0, sd = input$slope)
      timepoints <- rep(1:input$timepoint, times = input$subjects)
      subjects <- rep(1:input$subjects, each = input$timepoint)
      y <- 1 + rep(intercepts, each = input$timepoint) + rep(slopes, each = input$timepoint) * (1:input$timepoint) + rnorm(n)
      data.frame(timepoints, subjects, y)
    })
    
    model <- reactive({
      lmer(y ~ timepoints + (1 + (timepoints | subjects)), data = data()) %>% summary()
    })
    
    # Linear model for comparison
    lm_model <- reactive({
      lm(y ~ timepoints, data = data()) %>% summary()
    })
    
    # Combined coefficients for display
    combined_coefficients <- reactive({
      full_join(
        model()$coefficients %>% as.data.frame(),
        lm_model()$coefficients %>% as.data.frame()
      )
    })
    
    # Output summary of combined coefficients
    output$out_compare <- renderTable({
      combined_coefficients() %>% mutate(Model = rep(c('MLM', 'LM'), times = 2),
                                         Coef = rep(c('Intercept', 'Slope'), times = 2)) %>% relocate(c(Model,Coef), .before = everything())
    })
    
    output$rand <- renderPrint({
      list(MLM = model()$varcor,LM = lm_model()$sigma)
    })
  })
  
  
}

shinyApp(ui, server)

