## ------------------------------------------------------------------------
library(tidyverse)
library(shiny)

## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
generate_mutation_data <- function(idf, prob_mutation = 0.33, prob_repair = 0.3, prob_death = 0.2, num_generations = 10) {
  factor_lvls <- c("green", "yellow", "red", "black")
  mutation_df <- bind_rows(
  idf,
  map_df(2:num_generations, function(gen_num) {
  idf %>% mutate(GENERATION = gen_num,
                           potential_mutation = ifelse(runif(n(), 0, 1) < prob_mutation, 1, 0), 
                           NUM__ = runif(n(), 0, 1),
                           outcome = ifelse(!potential_mutation, 
                                                 potential_mutation, 
                                                 ifelse(NUM__ < prob_repair, 0, 
                                                        ifelse(NUM__ > 1-prob_death, NA_real_, potential_mutation))))
})) %>% 
  group_by(CELL_NUM) %>%
  mutate(
    num_mutations = cumsum(potential_mutation),
    outcome_mutations = cumsum(outcome), 
         
         color = parse_factor(case_when(
           is.na(outcome_mutations) ~ "black",
           outcome_mutations < 1 ~ "green",
           outcome_mutations <= 2 ~ "yellow",
           TRUE ~ "red"
         ), factor_lvls))
  return(mutation_df)
}




## ------------------------------------------------------------------------
base_theme <- function (legend_text = 10, legend_title = 12, axis_title_x = 12, 
    axis_title_y = axis_title_x, axis_text_x = 10, axis_text_y = axis_text_x, 
    strip_text_x = 12, strip_text_y = strip_text_x) 
{
    ggplot2::theme(legend.text = ggplot2::element_text(size = legend_text), 
        legend.title = ggplot2::element_text(size = legend_title), 
        axis.title.x = ggplot2::element_text(size = axis_title_x, 
            face = "bold"), axis.title.y = ggplot2::element_text(size = axis_title_y, 
            face = "bold"), axis.text.x = ggplot2::element_text(color = "black", 
            size = axis_text_x), axis.text.y = ggplot2::element_text(color = "black", 
            size = axis_text_y), strip.text.x = ggplot2::element_text(color = "black", 
            size = strip_text_x, face = "bold"), strip.text.y = ggplot2::element_text(color = "black", 
            size = strip_text_y, face = "bold"))
}
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_cells", "Number of Cells", value = 10, min = 2, max = 20, step = 1),
      sliderInput("num_generations", "Number of Generations", value = 10, min = 2, max = 20, step = 1),
      sliderInput("prob_mutation", "Probability of Initial Mutation", value = 0.3, min = 0, max = 1, step = 0.05),
      sliderInput("prob_repair", "Probability of Repair", value = 0.3, min = 0, max = 1, step = 0.05),
      sliderInput("prob_death", "Probability of Death after Mutation", value = 0.2, min = 0, max = 1, step = 0.05),
      actionButton("rerun", "rerun simulation")


    ),
    mainPanel(
      plotOutput("p_mutations", height = "80vh")
    )
  )
)
server <- function(input, output) {
  initial_cells <- reactive({
      input$rerun
       as_data_frame(expand.grid(GENERATION = 1, CELL_NUM = 1:input$num_cells, outcome = 0, potential_mutation = 0))
  })
  mutation_data <- reactive({
    validate(need(input$prob_repair + input$prob_death < 1, "Total probability of repair and death Must be less than 1"))
    generate_mutation_data(initial_cells(), input$prob_mutation, input$prob_repair, input$prob_death, input$num_generations) 
  })
  
  output$p_mutations <- renderPlot({
  mutation_data() %>%
    ggplot(aes(x = factor(GENERATION), y = factor(CELL_NUM))) +
    geom_point(size = 10, alpha = 0.3, aes(color = color)) + 
    geom_text(aes(label = glue::glue("{ifelse(is.na(outcome_mutations), '', num_mutations)}{ifelse(potential_mutation & !is.na(outcome_mutations), '*', '')}"))) +

    theme_bw() +
    scale_color_manual(name = "Outcome", values = c("green" = "#9BCA3E", "yellow" = "#FEEB51", "red" = "#ED5314", "black" = "black"), 
                       labels = c("black" = "dead", "green" = "normal","yellow" = "medium risk", "red" = "high risk")) +
    labs(x = "Generation", y = "Cell Number", title = "Number of Sustained Mutations", subtitle = "* - mutation occurred") + base_theme()
  }) 
}

## ------------------------------------------------------------------------
shinyApp(ui, server)

