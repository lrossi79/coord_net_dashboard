
library(igraph)
library(shiny)
library(reshape2)
library(lubridate)
library(readr)


g = read_rds("hcn.rds")



net <- list(timestamp=E(g)$t_coord_share)
names(net) <- as.data.frame(as.character(E(g)))
net <- melt(net,)
net <- net[c(2,1)]
names(net) <- c("eid","time")
net$time <- as_datetime(net$time)


shinyUI( fluidPage(
        titlePanel("Network of coordinated behaviour"),
        sidebarLayout(
            sidebarPanel(
                dateRangeInput("daterange4", "Date range:",
                               start = min(net$time),
                               end = max(net$time)),
#                               min= min(net$time),
#                               max=max(net$time)),
                sliderInput("degree",
                            "Min Degree:",
                            min = 1,
                            max = max(degree(g)),
                            value = 1)
            ),
      
        mainPanel(visNetworkOutput("network"),width = 12)
    )))




