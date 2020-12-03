

library(shiny)
library(shinydashboard)
library(kableExtra)



ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Iwi cultural well-being from Te Kupenga",
                  titleWidth = 450),
  
  dashboardSidebar(
      uiOutput("measure_ui"),
      uiOutput("region_ui"),
      uiOutput("iwi_ui")
  ),
  dashboardBody(
      
  tabBox(
       tabPanel("Graph", plotOutput("bar_plot",  height = "600px")),
       tabPanel("Table",  
                h3(htmlOutput("table_title")),
                htmlOutput("kable_output")),
    width = 10,
    height = 700
      )
    )
  )


server = function(input, output, session) {
  
  output$measure_ui <- 
    renderUI({
      q_list <- unique(dat_final$Question)
      
      selectInput("measure", "Select measure:", q_list, selected = q_list[1])
  })
  
   output$region_ui <- 
    renderUI({
      req(input$measure)

      
      region_list <- dat_final %>% 
        filter(Question == input$measure) %>% 
        pull(Region) %>%
        unique()
      
      selectInput("region", "Select region / rohe:", 
                     region_list, selected = region_list[1])
  })
  
  output$iwi_ui <- 
    renderUI({
      req(input$measure)
      req(input$region)
            
      iwi_list <- dat_final %>% 
        filter(Question == input$measure, Region == input$region) %>% 
        pull(Iwi) %>%
        unique()
       
      selectInput("iwi", "Select iwi:", iwi_list, selected = iwi_list[1])
    })
  
  
  data_final <- 
    reactive({
      
      req(input$measure)
      req(input$region)
      req(input$iwi)
      
      dat_final %>% 
        filter(Question == input$measure, Region == input$region, Iwi == input$iwi)
    })
  
  
  output$bar_plot <- renderPlot({
    
    g <-
      ggplot(req(data_final()), aes(x = Categories, y = Percent, fill = Categories)) +
      geom_col() +
      theme_bw() +
      scale_y_continuous(labels = scales::percent) +
      guides(fill = "none")  +
      scale_x_discrete(labels = function(x) str_wrap(x, width =10)) +
      labs(title = unique(data_final()$Question),
           subtitle = unique(data_final()$Iwi),
           caption = "Source: Statistics New Zealand") +
      theme(text = element_text(size = 15))
    
    g
    
  })
  
  output$table_title <- renderText({
    
    paste0(unique(data_final()$Question), "<br> Iwi: ",
           unique(data_final()$Iwi), "<br> Region:",
           unique(data_final()$Region))
  })

  output$kable_output <- renderText({
    
    data_final() %>% 
      select(Categories, Percent) %>%
      mutate(Percent = scales::percent(Percent, accuracy = 0.1)) %>% 
      kable() %>%
      kable_styling("hover", full_width = FALSE, font_size = 15)
  })
}

shiny::shinyApp(ui, server)
