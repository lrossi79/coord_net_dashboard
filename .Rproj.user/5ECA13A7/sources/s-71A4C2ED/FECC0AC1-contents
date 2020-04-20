

library(shiny)
library(visNetwork)
library(igraph)
library(reshape2)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)


#runGitHub( "coord_net_dashboard", "lrossi79")

#data <-  read_rds("D:/cononavirus phase2/ct_shares.df.rds.df")
data <-  readRDS("data/output.rds")
g <- data[[2]]
V(g)$selected <- 0



net <- list(timestamp=E(g)$t_coord_share)
names(net) <- as.data.frame(as.character(E(g)))
net <- melt(net,)
net <- net[c(2,1)]
names(net) <- c("eid","time")
net$time <- as_datetime(net$time)

t <- net
t$time <- as.Date(t$time)
t <- t %>% dplyr::group_by(time) %>%
    dplyr::summarize(GroupCount = n())

page_details <- data.frame(component=V(g)$component, name=V(g)$account.name, verified=V(g)$account.verified,degree=V(g)$degree)
shares <- as.data.frame(data[[1]])

url_shared <- unique(shares$title[shares$account.name %in% V(g)$account.name])

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
                 titlePanel("Network of coordinated behaviour"),
                 sidebarLayout(
                     sidebarPanel(
                         img(src='logo-coornet.png', align = "left", width="70%"),
                         dateRangeInput("daterange4", "",
                                        start = min(net$time),
                                        end = max(net$time)
                                        ),
                             
                          sliderInput("degree",
                                      "Min Degree:",
                                      min = 1,
                                      max = max(degree(g)),
                                      value = 0),
                         #selectInput("component",
                         #            "Select Component:",
                         #            choices = c("All",unique(V(g)$component)),
                         #            selected = "All",
                         #            multiple = F),
                         selectInput('news', 
                                     'Select News',
                                     choices = c("All", url_shared),
                                     selected = "All",
                                     selectize=FALSE)
                         
                         ),
                     
                     mainPanel(
                         
                         tabsetPanel(type = "tabs",
                                     tabPanel( "Network",visNetworkOutput("network")),
                                     tabPanel("Timeline", plotOutput(outputId = "barplot")),
                                     tabPanel("Details", DT::dataTableOutput("details"))
                         )
                         
                     )
                 ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    g2 = reactive({
        selected <- unique(shares$account.name[shares$title == input$news & shares$account.name %in% V(g)$account.name])
        net2 <- subset(net, time >= as.character(input$daterange4[1]) & time <= as.character(input$daterange4[2]))
        g3 <- subgraph.edges(g,eids = net2$eid,delete.vertices = T)
        #if(input$component=="All"){induced_subgraph(graph = g3,vids = V(g3)[V(g3)$degree >= input$degree])}
        #else if(input$component!="All"){induced_subgraph(graph = g3,vids = V(g3)[V(g3)$degree >= input$degree & V(g3)$component == input$component ])}
        if(input$news == "All"){induced_subgraph(graph = g3,vids = V(g3)[V(g3)$degree >= input$degree])}
        else if(input$news != "All"){induced_subgraph(graph = g3,vids = V(g3)$name[V(g3)$account.name %in% selected])}
        
        
    })
    
    
    t3= reactive({
        t2 <- subset(t,time >= as.character(input$daterange4[1]) & time <= as.character(input$daterange4[2]))
    })
    

    

    
    
    
    
   
    
    
    
    
    output$network <- renderVisNetwork({
        
        nodes <- data.frame(id=V(g2())$name, label=V(g2())$account.name, group=V(g2())$component)
        edges <- as.data.frame(as_edgelist(g2()))
        edges$weight <- E(g2())$weight
        colnames(edges) <- c("from", "to", "width")
        
        visNetwork(nodes, edges) %>%
            visIgraphLayout()
    })
    
    
    output$barplot <- renderPlot({ggplot(t3(),mapping = (aes(x = time,y = GroupCount)))+geom_line()+theme_light()})
    
   
    output$details <- DT::renderDataTable({page_details})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
