library(lubridate)
library(extrafont)
library(RColorBrewer)

# suppressWarnings(font_import(pattern = "times", prompt = F))


theme_set <- function(){
     theme_classic()+
          theme(plot.caption = element_text(face = "bold", size = 14, vjust = 0, 
                                            family = "Times New Roman", hjust = 0),
                plot.title.position = "plot", 
                plot.caption.position =  "plot",
                plot.title = element_text(face = "bold", size = 14, family = "Times New Roman", hjust = 0),
                legend.text = element_text(face = 'bold', size = 12, family = 'Times New Roman'),
                legend.title = element_text(face = 'bold', size = 12, family = 'Times New Roman'),
                legend.box.background = element_rect(fill = "transparent", colour = 'transparent'),
                legend.background = element_rect(fill = "transparent", colour = 'transparent'),
                axis.title.x = element_text(face = 'bold', size = 12, color = 'black', family = 'Times New Roman'),
                axis.title.y = element_text(face = 'bold', size = 12, color = 'black', family = 'Times New Roman'),
                axis.text.x = element_text(size = 12, color = 'black', family = 'Times New Roman'),
                axis.text.y = element_text(size = 12, color = 'black', family = 'Times New Roman'))
}

theme_plot <- function(){
     theme_classic()+
          theme(plot.caption = element_text(face = "bold", size = 16, vjust = 0, 
                                            family = "Times New Roman", hjust = 0),
                plot.title.position = "plot", 
                plot.caption.position =  "plot",
                plot.title = element_text(face = "bold", size = 18, family = "Times New Roman", hjust = 0),
                legend.text = element_text(face = 'bold', size = 14, family = 'Times New Roman'),
                legend.title = element_text(face = 'bold', size = 14, family = 'Times New Roman'),
                legend.box.background = element_rect(fill = "transparent", colour = 'transparent'),
                legend.background = element_rect(fill = "transparent", colour = 'transparent'),
                axis.title.x = element_text(face = 'bold', size = 16, color = 'black', family = 'Times New Roman'),
                axis.title.y = element_text(face = 'bold', size = 16, color = 'black', family = 'Times New Roman'),
                axis.text.x = element_text(size = 14, color = 'black', family = 'Times New Roman'),
                axis.text.y = element_text(size = 14, color = 'black', family = 'Times New Roman'))
}

func_rmse <-
     # actual_val is the actual valeu,
     # fit_val is the value fitted by model
     function(actual_val, fit_val) {
          sqrt(
               mean((as.numeric(fit_val) - as.numeric(actual_val))^2, na.rm = TRUE)
          )
     }


fill_color <- paletteer_d("ggsci::nrc_npg")
fill_color <- fill_color[c(4, 1, 3, 5, 8:10)]
