#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tibble)
library(ggplot2)
library(dplyr)
library(broom)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Collider bias demo"),
   
   # Sidebar with a slider input for number of bins 
   verticalLayout(
     h4("Background and causal model"),
     textOutput("introText"),
     br(),
   sidebarLayout(
      sidebarPanel(
         sliderInput("bp_cad",
                     "Influence of BP pathway on CAD:",
                     min = -2,
                     max = 2,
                     step = 0.1,
                     value = 1),
         sliderInput("bp_outcome",
                     "Influence of BP pathway on outcome:",
                     min = -2,
                     max = 2,
                     step = 0.1,
                     value = 0),
         sliderInput("ldl_cad",
                     "Influence of LDL on CAD:",
                     min = -2,
                     max = 2,
                     step = 0.1,
                     value = 2),
         sliderInput("ldl_outcome",
                     "Influence of LDL on outcome:",
                     min = -2,
                     max = 2,
                     step = 0.1,
                     value = 1),
         sliderInput("cad_outcome",
                     "Influence of CAD on outcome:",
                     min = -2,
                     max = 2,
                     step = 0.1,
                     value = 0),
         sliderInput("noise",
                     "Noise:",
                     min = 0,
                     max = 10,
                     step = 0.5,
                     value = 1),
         sliderInput("threshold",
                     "Threshold on CAD for patient selection:",
                     min = -10,
                     max = 10,
                     step = 0.5,
                     value = -10)
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        verticalLayout(
          imageOutput("DAG_image"),
          plotOutput("colliderPlot"),
          tableOutput("linearFit")
        )
      )
   )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  sampleData <- function(N, bp_cad, bp_outcome, ldl_cad, ldl_outcome, cad_outcome, noise) {
    BP_score = rnorm(N)
    LDL_score = rnorm(N)
    cad_score = bp_cad * BP_score + ldl_cad * LDL_score + noise * rnorm(N)
    outcome_score = bp_outcome * BP_score + ldl_outcome * LDL_score + cad_outcome * cad_score + noise * rnorm(N)
    
    data = tibble(BP_score, LDL_score, cad_score, outcome_score)
    
  }
  
  getSampleData <- reactive({
    data = sampleData(10000, input$bp_cad, input$bp_outcome, input$ldl_cad, input$ldl_outcome, input$cad_outcome, input$noise) 
  })
   
   output$colliderPlot <- renderPlot({
      # generate sample data based on input
     data = getSampleData()
     # data = data %>% filter(cad_score > input$threshold)

     ggplot(data, aes(x=BP_score, outcome_score, colour = cad_score > input$threshold)) +
       geom_point(size=0.4, alpha=0.5) +
       geom_smooth(method = "lm") +
       theme_bw() +
       scale_color_manual(values=c("black", "blue"))
   })
   
   output$linearFit <- renderTable({
     lm(outcome_score ~ BP_score, data = getSampleData()) %>% tidy() %>% filter(term=="BP_score") %>% 
       mutate(term=NULL, model="outcome ~ BP") %>% select(model, estimate, std.error, p.value) %>%
       bind_rows(
         lm(outcome_score ~ BP_score + cad_score, data = getSampleData()) %>% tidy() %>% filter(term=="BP_score") %>%
           mutate(term=NULL, model="outcome ~ BP + CAD") %>% select(model, estimate, std.error, p.value),
         lm(outcome_score ~ BP_score + cad_score, data = getSampleData() %>% filter(cad_score > input$threshold)) %>% 
           tidy() %>% filter(term=="BP_score") %>%
           mutate(term=NULL, model="outcome ~ BP | CAD cases only") %>% select(model, estimate, std.error, p.value)
       )
   })
   
   output$DAG_image = renderImage(list(src="dag.png", alt = "Here you should see the causal DAG.", height="70%"), deleteFile = F)
   
   output$introText = renderText(paste0("We will use this causal model to demonstrate collider bias. Assume we would like to estimate the ",
                                        "effect of blood pressure on the outcome (e.g. mortality and hospitalizations) in patients ",
                                        "that already have CAD (in our model that is ",
                                        "patients with a CAD score higher than the given threshold). ",
                                        "But this introduces a collider bias, because LDL is also influencing CAD and outcome. ",
                                        "In the graph you see the patients with and without CAD and the correlation of blood pressure ",
                                        "and outcome. In the table you see the results of different linear models. Adjusting for ",
                                        "LDL would solve the issue here, but we assume we have no measurement for that. ",
                                        "(We might actually need a measurement of the lifetime LDL burden, which is not easy to get. ",
                                        "Furthermore, there may be additional unknown factors influencing CAD and the outcome."))
}

# Run the application 
shinyApp(ui = ui, server = server)

