library(shiny)
library(visNetwork)
library(igraph)
library(reshape2)
library(lubridate)
library(readr)
library(ggplot2)

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


shinyUI( fluidPage(theme = "bootstrap.css",
         titlePanel("Network of coordinated behaviour"),
        sidebarLayout(
            sidebarPanel(
              img(src='logo-coornet.png', align = "left", width="70%"),
                dateRangeInput("daterange4", "",
                               start = min(net$time),
                               end = max(net$time)),
#                               min= min(net$time),
#                               max=max(net$time)),
                sliderInput("degree",
                            "Min Degree:",
                            min = 1,
                            max = max(degree(g)),
                            value = 0)
            ),
      
        mainPanel(
          
          tabsetPanel(type = "tabs",
                      tabPanel( "Network",visNetworkOutput("network")),
                      tabPanel("Timeline", plotOutput(outputId = "barplot"))
          )
          
                           )
    )))




