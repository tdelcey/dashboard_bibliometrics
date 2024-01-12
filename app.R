#package
library(tidyverse)
library(tidytext)
library(stringr)
library(dplyr)
library(data.table)
library(DT)
library(here)
library(shiny)
library(shinyWidgets)

data_app <- "data"

# load and prepare data
list_ggraph <- readRDS(here("list_ggraph.rds"))
tfidf_bigram <- readRDS(here("tfidf_bigram.rds"))
nodes_filtered <- readRDS(here("nodes_filtered.rds")) 

# dashboard with shiny 

# initialization for networks 
years <- names(list_ggraph)
min_year <-  names(list_ggraph)[[1]]
max_year <-  names(list_ggraph)[[length(list_ggraph)]]

ui <- fluidPage(
  titlePanel("Bibliometric analysis dashboard"),
  tabsetPanel(
    # First main panel: Cluster Analysis
    tabPanel("Cluster Analysis",
             fluidRow(
               column(12, align = "center",
                      div(style = "margin-top: 20px;"),
                      sidebarPanel(
                        selectInput(
                          "clusterSelect",
                          "Select Cluster:",
                          choices = unique(nodes_filtered$dynamic_cluster_leiden)
                        ),
                        div(style = "margin-top: 20px;")
                      )
               ),
               column(12, align = "center",
                      mainPanel(
                        tabsetPanel(
                          tabPanel("List of documents", div(style = "margin-top: 20px;"), DTOutput("list_doc")),
                          tabPanel("TF-IDF", div(style = "margin-top: 20px;"), plotOutput("plottfidf")),
                          tabPanel("Journal", div(style = "margin-top: 20px;"), plotOutput("plotjournal")),
                          tabPanel("Citations", div(style = "margin-top: 20px;"), plotOutput("plotcitations")),
                          tabPanel("Distribution", div(style = "margin-top: 20px;"), plotOutput("distribution"))
                          # ... other tabs ...
                        )
                      )
               )
             )
    ),
    # Second main panel: Network Specialization
    tabPanel(
      "Network spacialization",
      fluidRow(
        column(12, align = "center",
               div(style = "margin-top: 20px;"),
               mainPanel(
                 width = 12,  
                 sliderTextInput(
                   "year",
                   "Select Year:",
                   choices = years,
                   selected = names(list_ggraph)[[1]]
                 ),
                 div(style = "margin-top: 20px;"),
                 girafeOutput("network"),
                 tabPanel("Information on node selected", DTOutput("node_info"))
                 # other tabs
               )
        )
      )
    )
  )
)



server <- function(input, output) {
  #network
  output$network <- renderGirafe({
    list_ggraph[[as.character(input$year)]]
  })
  
  
  #create a reactive value 
  selected_nodes <- reactiveValues(data = list())
  
  observeEvent(input$network_selected, {
    node_ids <- input$network_selected
    if (!is.null(node_ids)) {
      selected_data <- nodes_filtered %>%
        ungroup() %>%
        select(source_id, display_name, first_author, year, so, cited_by_count, doi) %>%
        unique() %>%
        filter(source_id %in% node_ids)
      
      selected_nodes$data <- selected_data
    }
  })
  
  output$node_info <- renderDT({
    if (length(selected_nodes$data) == 0) return(NULL)
    datatable(bind_rows(selected_nodes$data), options = list(pageLength = 5))
  })

# top citations
output$plotcitations <- renderPlot({
  nodes_filtered %>%
    group_by(dynamic_cluster_leiden) %>%
    slice_max(cited_by_count, n = 10)  %>%
    select(dynamic_cluster_leiden,
           short_label_node,
           cited_by_count,
           cluster_label,
           color,
           doi) %>%
    unique %>%
    filter(dynamic_cluster_leiden == input$clusterSelect) %>%
    ggplot(aes(
      x = tidytext::reorder_within(short_label_node, cited_by_count, cluster_label),
      y = cited_by_count,
      fill = color
    )) +
    geom_col(show.legend = FALSE) +
    scale_fill_identity() +
    coord_flip() +
    tidytext::scale_x_reordered() +
    #facet_wrap(~ cluster_label, scales = "free") +
    labs(title = "Most cited papers",
         x = "",
         y = "Number of citations",
         y = NULL) +
    theme_classic(base_size = 9) +
    theme(axis.text.x = element_text(size = 20), text = element_text(size = 20))
},

width = 900, height = 600)

#tf idf
output$plottfidf <- renderPlot({
  tfidf_bigram %>%
    group_by(dynamic_cluster_leiden) %>%
    slice_max(tf_idf, n = 15) %>%
    left_join(select(
      nodes_filtered,
      c(dynamic_cluster_leiden, color, cluster_label)
    )
    %>%  unique,
    by = "dynamic_cluster_leiden") %>%
    filter(dynamic_cluster_leiden == input$clusterSelect) %>%
    ggplot(aes(
      x = tidytext::reorder_within(token, tf_idf, cluster_label),
      y = tf_idf,
      fill = color
    )) +
    geom_col(show.legend = FALSE) +
    scale_fill_identity() +
    coord_flip() +
    tidytext::scale_x_reordered() +
    #facet_wrap(~ cluster_label, scales = "free") +
    labs(title = "Top tf-idf unigram & bigram",
         x = "",
         y = "tf-idf",
         y = NULL) +
    theme_classic(base_size = 9) +
    theme(axis.text.x = element_text(size = 20), text = element_text(size =
                                                                       30))
},

width = 900, height = 600)

# #field based on openalex concept
# output$plotfield <- renderPlot({
#   nodes_filtered %>%
#     select(source_id, concepts, dynamic_cluster_leiden, color) %>%
#     unnest(concepts, names_sep = "_") %>%
#     filter(concepts_level == 1) %>%
#     group_by(dynamic_cluster_leiden, concepts_display_name) %>%
#     reframe(cluster_concepts_score = mean(concepts_score),
#             color = color) %>%
#     unique() %>%
#     group_by(dynamic_cluster_leiden) %>%
#     slice_max(cluster_concepts_score, n = 10) %>%
#     filter(dynamic_cluster_leiden == input$clusterSelect) %>%
#     ggplot(aes(
#       x = tidytext::reorder_within(
#         concepts_display_name,
#         cluster_concepts_score,
#         dynamic_cluster_leiden
#       ),
#       y = cluster_concepts_score,
#       fill = color
#     )) +
#     geom_col(show.legend = FALSE) +
#     scale_fill_identity() +
#     coord_flip() +
#     tidytext::scale_x_reordered() +
#     #facet_wrap(~ cluster_label, scales = "free") +
#     labs(title = "Disciplines in the cluster",
#          x = "",
#          y = "Number of documents",
#          y = NULL) +
#     theme_classic(base_size = 9) +
#     theme(text = element_text(size = 20))
# },
#
# width = 900, height = 600)

#top journal
output$plotjournal <- renderPlot({
  nodes_filtered %>%
    select(so, dynamic_cluster_leiden, color) %>%
    add_count(so, dynamic_cluster_leiden) %>%
    unique() %>%
    group_by(dynamic_cluster_leiden) %>%
    slice_max(n, n = 10) %>%
    filter(dynamic_cluster_leiden == input$clusterSelect) %>%
    ggplot(aes(
      x = tidytext::reorder_within(so, n, dynamic_cluster_leiden),
      y = n,
      fill = color
    )) +
    geom_col(show.legend = FALSE) +
    scale_fill_identity() +
    coord_flip() +
    tidytext::scale_x_reordered() +
    #facet_wrap(~ cluster_label, scales = "free") +
    labs(title = "Top journal",
         x = "",
         y = "Number of documents",
         y = NULL) +
    theme_classic(base_size = 9) +
    theme(text = element_text(size = 20))
},

width = 900, height = 600)

# list of papers sorted by presence and citations
output$list_doc <- renderDT({
  datatable(
    nodes_filtered %>% filter(dynamic_cluster_leiden == input$clusterSelect) %>%
      select(
        display_name,
        first_author,
        year,
        so,
        presence_rate_in_cluster,
        cited_by_count
      ) %>%
      arrange(-presence_rate_in_cluster,-cited_by_count)
    ,
    options = list(pageLength = 5)
  )
})

# distribution over time
output$distribution <- renderPlot({
  nodes_filtered %>%
    ungroup() %>%
    add_count(year) %>%
    add_count(dynamic_cluster_leiden, year) %>%
    select(dynamic_cluster_leiden, year, cluster_label, color, n, nn) %>%
    mutate(n_year = nn / n * 100) %>%
    unique %>%
    filter(dynamic_cluster_leiden == input$clusterSelect) %>%
    ggplot(aes(
      x = as.integer(year),
      y = n_year,
      fill = color
    )) +
    geom_col(show.legend = FALSE) +
    scale_fill_identity() +
    labs(title = "Distribution of documents over the period (% of total documents)",
         x = "",
         y = "%",
         y = NULL) +
    theme_classic(base_size = 9) +
    theme(text = element_text(size = 20))
},

width = 900, height = 600)
}

app <- shinyApp(ui, server)

shiny::runApp(app)



#publish app and update app

# rsconnect::deployApp( local_path_of_your_app )

shiny::runGitHub( "dashboard_bibliometrics", "tdelcey", ref = "main")
