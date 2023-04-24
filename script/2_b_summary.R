
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(patchwork)
library(Cairo)
library(ggpubr)
library(ggforce)
library(paletteer)

Sys.setlocale(locale = 'en')

# data load ---------------------------------------------------------------

source('./script/theme_set.R')

scientific_10 <- function(x) {
     parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x))))
}

datafile_class <- read.xlsx('./data/disease_class.xlsx')
datafile_class <- read.xlsx('./outcome/model_select_A.xlsx',
                            sheet = 'result') |>
     select(D, Final) |>
     left_join(datafile_class, by = c(D = 'diseasename')) |> 
     rename(Method = 'Final') |> 
     filter(!is.na(class))
datafile_class$id <- 1:nrow(datafile_class)
file_list <- list.files(path = './outcome/data/',
                        pattern = '^A_',
                        full.names = T)

disease_list <- c('百日咳', '丙肝', '戊肝', '布病', '登革热', 
                  '肺结核', '风疹', '急性出血性结膜炎', '甲肝', 
                  '痢疾', '淋病', '流行性出血热', '艾滋病',
                  '流行性腮腺炎', '梅毒', '疟疾', '其它感染性腹泻病',
                  '伤寒+副伤寒', '乙肝', '手足口病', '猩红热',
                  '乙型脑炎', '包虫病', '斑疹伤寒')
disease_name <- c('Pertussis', 'HCV', 'HEV',
                  'Brucellosis', 'Dengue fever', 'Tuberculosis',
                  'Rubella', 'Acute hemorrhagic conjunctivitis', 'HAV',
                  'Dysentery', 'Gonorrhea', 'HFRS',
                  'AIDS', 'Mumps',
                  'Syphilis', 'Malaria', 'Other infectious diarrhea',
                  'Typhoid fever and paratyphoid fever', 'HBV', 'HFMD',
                  'Scarlet fever', 'Japanese encephalitis', 'Hydatidosis', 'Typhus')

datafile_class <- data.frame(disease_list = disease_list,
                             disease_name = disease_name) |> 
     right_join(datafile_class, by = c('disease_name' = 'D')) |> 
     mutate(disease_name = factor(disease_name,
                                  levels = c('HBV', 'HCV', 'Syphilis', 'AIDS', 'Gonorrhea',
                                             'HAV', 'HFMD', 'HEV', 'Other infectious diarrhea', 'Typhoid fever and paratyphoid fever', 'Acute hemorrhagic conjunctivitis', 'Dysentery',
                                             'Dengue fever', 'Brucellosis', 'Malaria', 'Japanese encephalitis', 'HFRS', 'Hydatidosis', 'Typhus',
                                             'Rubella', 'Mumps', 'Pertussis', 'Tuberculosis', 'Scarlet fever'))) |> 
     arrange(class, disease_name)

# data clean --------------------------------------------------------------

datafile_analysis <- map_dfr(file_list, 
                             read.xlsx,
                             detectDates = T)
datafile_analysis[datafile_analysis < 0] <- 0
datafile_analysis <- datafile_analysis |> 
     group_by(disease_1) |> 
     summarise(mean = sum(mean),
               lower = sum(lower_95, na.rm = F),
               upper = sum(upper_95, na.rm = F),
               value = sum(value)) |> 
     left_join(datafile_class,
               by = c(disease_1 = "disease_list")) |> 
     select(disease_name, mean, upper, lower, value, class) |> 
     mutate(disease_name = fct_rev(disease_name))

# plot 1 ------------------------------------------------------------------

datafile_forecast <- datafile_analysis |> 
     select(-value) |> 
     mutate(type = 'Forecasted')
datafile_observe <- datafile_analysis |> 
     select(disease_name, value, class) |> 
     mutate(upper = NA,
            lower = NA,
            .before = value) |> 
     rename(mean = 'value') |> 
     mutate(type = 'Observed')
datafile_plot <- rbind(datafile_forecast,
                       datafile_observe)

fig1 <- ggplot(data = datafile_plot)+
     geom_col(mapping = aes(x = disease_name,
                            y = mean,
                            fill = type),
              position=position_dodge())+
     geom_errorbar(mapping = aes(x = disease_name,
                                 y = mean,
                                 ymin = lower,
                                 ymax = upper,
                                 group = type),
                   width = 0.2,
                   position=position_dodge(.9),
                   show.legend = F)+
     coord_flip()+
     scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                        breaks = c(0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8),
                        limits = c(0, NA),
                        label = scientific_10,
                        expand = expansion(mult = c(0, 0.15)))+
     scale_x_discrete()+
     scale_fill_manual(values = c(Forecasted = "#F39B7FFF", Observed = '#3C5488FF'))+
     facet_col(vars(class),
               scales = 'free_y',
               space = 'free',
               labeller = function(labels) list(c('A', 'C', 'E', 'G')))+
     theme_bw()+
     theme(legend.position = 'bottom',
           strip.background = element_blank(),
           strip.text.x = element_text(angle = 0,
                                       face = "bold", 
                                       size = 14,
                                       hjust = 0))+
     labs(x = NULL,
          y = 'Incidence (Monthly)',
          fill = NULL)

# plot 2 ------------------------------------------------------------------

datafile_plot <- datafile_analysis |> 
     mutate(mean_change = (value - mean)/mean,
            down = if_else(mean_change < 0,
                           0,
                           1),
            label = paste0(round(abs(mean_change)*100, 2), '%'),
            lower_change = (lower - mean)/lower,
            upper_change = (upper - mean)/upper)

fig2 <- ggplot(data = datafile_plot)+
     geom_hline(yintercept = 0,
                color = 'black')+
     geom_col(mapping = aes(x = disease_name,
                            y = mean_change,
                            fill = as.character(down)),
              position=position_dodge(),
              show.legend = F)+
     geom_label(mapping = aes(x = disease_name,
                              y = mean_change,
                              label = label,
                              hjust = ifelse(down == 0,
                                             down - 0.1,
                                             down + 0.1)),
                fill = 'white')+
     coord_flip(ylim = c(-1, 0.25))+
     scale_y_continuous(label = scales::percent,
                        expand = expansion(mult = c(0, 0)))+
     scale_x_discrete()+
     scale_fill_manual(values = c('1' = "#E64B35FF", '0' = '#00A087FF'))+
     facet_col(vars(class),
               scales = 'free_y',
               space = 'free',
               labeller = function(labels) list(c('B', 'D', 'F', 'H')))+
     theme_bw()+
     theme(legend.position = 'bottom',
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank(),
           strip.background = element_blank(),
           strip.text.x = element_text(angle = 0,
                                       face = "bold", 
                                       size = 14,
                                       hjust = 0))+
     labs(x = NULL,
          y = 'The change of incidence (Monthly)')

fig1 + fig2 + plot_layout(widths = c(2, 1.7))

ggsave('./outcome/publish/fig3.pdf',
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 10)

