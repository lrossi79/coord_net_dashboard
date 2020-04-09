

library(shiny)
library(visNetwork)
library(igraph)
library(visNetwork)





shinyServer(function(input, output) {

    
    g2 = reactive({
        net2 <- subset(net, time >= as.character(input$daterange4[1]) & time <= as.character(input$daterange4[2]))
        g3 <- subgraph.edges(g,eids = net2$eid,delete.vertices = T)
        induced_subgraph(graph = g3,vids = V(g3)[V(g3)$degree > input$degree])
    })
    

    output$network <- renderVisNetwork({
        
        nodes <- data.frame(id=V(g2())$name, label=V(g2())$account.name)
        
        edges <- as.data.frame(as_edgelist(g2()))
        edges$weight <- E(g2())$weight
        colnames(edges) <- c("from", "to", "width")
        
        visNetwork(nodes, edges) %>%
            visIgraphLayout()
    })
    
    
    
    
})

