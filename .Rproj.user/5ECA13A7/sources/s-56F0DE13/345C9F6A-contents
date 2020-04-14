library(shiny)
library(visNetwork)
library(igraph)
library(reshape2)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)


#runGitHub( "coord_net_dashboard", "lrossi79")
g <-  readRDS("data/hcn.rds")



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




shinyServer(function(input, output) {

    
    g2 = reactive({
        net2 <- subset(net, time >= as.character(input$daterange4[1]) & time <= as.character(input$daterange4[2]))
        g3 <- subgraph.edges(g,eids = net2$eid,delete.vertices = T)
        induced_subgraph(graph = g3,vids = V(g3)[V(g3)$degree >= input$degree])
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
    
    
    
})

