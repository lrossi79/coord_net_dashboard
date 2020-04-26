data <- readRDS("../../../Downloads/output (1).rds")
top_url <- get_top_coord_urls(data)
dashboard_data <- list(data[[2]],top_url)
saveRDS(dashboard_data,"dashboard_data.rds")