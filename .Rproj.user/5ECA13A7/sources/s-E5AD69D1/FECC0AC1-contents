

library(shiny)
library(visNetwork)
library(igraph)
library(reshape2)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(stringr)
library(CooRnet)
#library(googledrive)


#runGitHub( "coord_net_dashboard", "lrossi79")




data <-  readRDS("data/dashboard_data.rds")
g <- data[[1]]
V(g)$color <- "gray"
top_url <- data[[2]]
top_url_tab <- top_url
top_url$cooR.account.name <- strsplit(top_url$cooR.account.name,",")
top_url <- unnest(data = top_url,cols = cooR.account.name)
top_url$cooR.account.name <- str_trim(top_url$cooR.account.name)
top_url$cooR.account.name <- gsub(pattern = "\"",replacement = "",x = top_url$cooR.account.name)


net <- list(timestamp=E(g)$t_coord_share)
names(net) <- as.data.frame(as.character(E(g)))
net <- melt(net,)
net <- net[c(2,1)]
names(net) <- c("eid","time")
net$time <- as_datetime(net$time)
start_t <-  min(net$time)-days(1)
end_t <-  max(net$time)+days(1)
min_t <- min(net$time)-days(2)
max_t <- max(net$time)+days(2)



# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
                 titlePanel("Network of coordinated behaviour"),
                
                 sidebarLayout(
                     sidebarPanel(
                         img(src='logo-coornet.png', align = "left", width="80%"),
                          dateRangeInput("dateRange", 
                                         "Date Range",
                                         start = start_t,
                                         end = end_t,
                                         min= min_t,
                                         max=max_t
                                         ),
                        br(),
                         selectInput('news', 
                                     'Select News',
                                     choices = c("All", unique(top_url$expanded)),
                                     selected = "All",
                                     selectize=FALSE),
                         
                         sliderInput("degree",
                                      "Min Degree:",
                                      min = 1,
                                      max = max(degree(g)),
                                      value = 0),
                         
                         sliderInput("repetition",
                                     "Repetition:",
                                     min = 1,
                                     max = max(E(g)$weight),
                                     value = 0),
                         
                         
                         #data description
                         tags$div(class="header", checked=NA,
                                  tags$h6("Facebbok entities (Pages or Public Groups) that have shared, in a continuous coordinated way, news stories (in Italian) containing the keywords:  coronavirus OR covid-19 OR SARS-CoV-2. Details on the methods:"),
                                  tags$a(href="https://github.com/fabiogiglietto/CooRnet", "CooRnet"))
                                  ),
                     
                     mainPanel(
                         
                         tabsetPanel(type = "tabs",
                                     tabPanel( "Network",visNetworkOutput("network",height = "800")),
                                     tabPanel("News stories", DT::dataTableOutput("details")),
                                     #tabPanel("test", DT::dataTableOutput("test")),
                                     tabPanel("Timeline", plotOutput("barplot"), verbatimTextOutput("dateRangeText"))
                                     
                                     
                         )
                         
                     )
                 ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    g2 = reactive({
        selected <- top_url$cooR.account.name[top_url$expanded == input$news]
        net2 <- net %>% filter(time >= as.character(input$dateRange[1]), time < as.character(input$dateRange[2]))
        g3 <- subgraph.edges(g,eids = E(g)[E(g) %in% net2$eid & E(g)$weight >= input$repetition ],delete.vertices = T)
        if(input$news == "All"){induced_subgraph(graph = g3,vids = V(g3)[V(g3)$degree >= input$degree])}
        else if(input$news != "All"){
            V(g3)[V(g3)$account.name %in% selected]$color <- "red"
            induced_subgraph(graph = g3,vids = V(g3)[V(g3)$degree >= input$degree])}
        
        
    })
    
    
    t2= reactive({
        t2 <- net %>% filter(time >= as.character(input$dateRange[1]), time < as.character(input$dateRange[2]))
        #t2$time <- as.Date(t2$time)
        t2$time_c <- ceiling_date(t2$time,unit = "hour")
        t2 <-  t2 %>% dplyr::group_by(time_c) %>% dplyr::summarize(count = n())
        
    })
    

    
    
    
    output$network <- renderVisNetwork({

        nodes <- data.frame(id=V(g2())$name,
                            label=V(g2())$account.name,
                            color=V(g2())$color,
                            component=V(g2())$component,
                            title=paste0(V(g2())$account.name, " - AVG. subscribers=", V(g2())$avg.account.subscriberCount, " - verified=",verified=V(g2())$account.verified),
                            font.size=V(g2())$degree*2)

        edges <- as.data.frame(as_edgelist(g2()))
        edges$weight <- E(g2())$weight
        edges$weight <- (edges$weight/max(edges$weight))+10
        colnames(edges) <- c("from", "to", "width")

        visNetwork(nodes, edges) %>%
            visOptions(highlightNearest = TRUE, selectedBy = "component") %>%
            visIgraphLayout(layout = "layout_nicely", physics = F,type = "full")
    })
    
    
    output$barplot <- renderPlot({p <- ggplot(t2())+geom_line(mapping = aes(x = time_c,y = count))
                                    print(p)
    })
    
   
    output$details <- DT::renderDataTable({
        DT::datatable(top_url_tab, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
        #top_url_tab
        })
    
    output$test <- DT::renderDataTable({
        DT::datatable(t2())
        #top_url_tab
    })
    


    output$dateRangeText  <- renderText({
        paste("Selected period from",  as.character(input$dateRange[1]), "to" , as.character(input$dateRange[2]))
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
