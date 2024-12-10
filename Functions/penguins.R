## ---------------------------
##
## Script name: penguins.r
##
## Purpose of script: 
##      # A file of functions for the Palmer Penguins dataset
##
## Author: anonymous
##
## Date Created: 2024-12-10
##
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

### a function to run all cleaning functions
cleaning <- function(Dataset, column_names){
  Output <- Dataset %>% 
    remove_empty_columns_rows() %>% 
    select(-starts_with(column_names)) %>% 
    clean_names() %>% 
    shorten_species() %>% 
    shorten_sex() %>% 
  return(Output)
}

### a function to create a scatterplot, for both exploratory and statistic purposes
scatterplot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Col, Ctitle, Title, 
                        Stat,  file, Height, Width, Scaling){
  species_colours <- c("Adelie" = "darkorange",
                       "Chinstrap" = "purple",
                       "Gentoo" = "cyan4")
  Chin <- Data %>% filter(species == "Chinstrap")
  Others <- Data %>% filter(species %in% c("Adelie","Gentoo"))

  plot <- ggplot(Data, aes(x = Xaxis, y = Yaxis, colour = Col)) +
    geom_point(size = 0.8) +
    (if (Stat == TRUE) { 
      geom_smooth(data =  Chin, method = "lm", aes(x = flipper_length_mm, y = body_mass_g), 
                  se = FALSE, linetype = "solid", size = 3, colour = "black") 
    }) +
    (if (Stat == TRUE) { 
      geom_smooth(data = Others, method = "lm", aes(x = flipper_length_mm, y = body_mass_g,group = 1), 
                  se = FALSE, linetype = "solid", colour = "#606060") 
    }) +
    scale_y_break(c(100, 150), space = 0, scale = 5) +
    theme_bw() +
    theme(axis.text.y   = element_text(size = 12, color = "black"),
          axis.text.x   = element_text(size = 12, color = "black", 
                                       angle = 45, hjust = 1),
          axis.title.y  = element_text(size = 14),
          axis.title.x  = element_text(size = 14),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 0.1),
          panel.border = element_rect(colour = "black", fill = NA,
                                      size = 1),
          plot.title = element_text(hjust = 0.02, vjust = 0.1,
                                    size = 16),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.background = element_rect(fill = "white", colour = NA),
          legend.box.background = element_rect(color = "black", size = 0.5)
          ) + 
    scale_colour_manual(values = species_colours) +
    xlab(Xtitle) +
    ylab(Ytitle) +
    labs(colour = Ctitle) +
    ggtitle(Title) +
    xlim(0,6500) +
    ylim(0,250)
  svglite(here("Figures", file), width = Width,
          height = Height,
          scaling = Scaling)
  print(plot)
  dev.off()
}