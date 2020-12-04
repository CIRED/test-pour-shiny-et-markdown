# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
percent_map <- function(var, color, legend.title, min = 0, max = 100) {

  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

  # plot choropleth map
  map("county", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
    paste0(min + inc, " %"),
    paste0(min + 2 * inc, " %"),
    paste0(min + 3 * inc, " %"),
    paste0(max, " % or more"))
  
  legend("bottomleft", 
    legend = legend.text, 
    fill = shades[c(1, 25, 50, 75, 100)], 
    title = legend.title)
}

map_idefese <- function(scenario_ici,difference_entre_all_scenarios){
  difference_entre_all_scenarios_ici<-difference_entre_all_scenarios %>% 
    filter(scenario==scenario_ici)
  
  mapi<-ggplot()+
    geom_sf(data=difference_entre_all_scenarios_ici,aes(fill=diff_with_2017),size = 0.1) +
    scale_fill_distiller(type = "div",
                         palette='RdBu',direction =-1, #pour que rouge = negatif et bleu = positif
                         #palette='Spectral',
                         limit =  max(abs(difference_entre_all_scenarios$diff_with_2017),na.rm = TRUE) * c(-1, 1),
                         #limit =  c(-50,50)
                         na.value = NA)+
    #facet_wrap('scenario')+
    xlab("") + ylab("") +
    theme(panel.background = element_rect(fill = 'white'),axis.ticks = element_blank(),axis.text = element_blank(),strip.text.x = element_text(size = 5))+
    labs(fill = "Variation (%)")+
    ggtitle("Service de retention d'eau en cas d'inondation")
  
  plot(mapi)
  
  
}