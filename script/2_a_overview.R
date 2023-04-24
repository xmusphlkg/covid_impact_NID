
# packages ----------------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggsci)
library(paletteer)
library(patchwork)

# data --------------------------------------------------------------------

source('./script/theme_set.R')
fill_color <- paletteer_d("ggsci::nrc_npg")

scientific_10 <- function(x) {
     parse(text = gsub("[+]", "", gsub("1e", "10^", scales::scientific_format()(x))))
}

datafile_analysis <- read.xlsx('./data/Nation.xlsx', detectDates = T)
datafile_class <- read.xlsx("./data/disease_class.xlsx", detectDates = T)

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

split_date_1 <- as.Date("2019/12/15")
split_date_2 <- as.Date("2022/11/15")
split_date_3 <- as.Date("2023/1/15")

# bubble plot -------------------------------------------------------------

datafile_plot <- datafile_analysis |> 
     filter(disease_1 %in% disease_list) |> 
     select(date, disease_1, value) |> 
     mutate(disease = factor(disease_1,
                             levels = disease_list,
                             labels = disease_name),
            phase = case_when(date < split_date_1 ~ 'Pre-epidemic',
                              date > split_date_1 & date < split_date_2 ~ 'Epidemic',
                              date > split_date_2 & date < split_date_3 ~ 'Pandemic',),
            phase = factor(phase,
                           levels = c('Pre-epidemic', 'Epidemic', 'Pandemic'))) |> 
     left_join(datafile_class, by = c('disease' = 'diseasename')) |> 
     mutate(class = factor(class,
                           levels = c("Blood borne and sexually transmitted diseases",
                                      "Intestinal infectious diseases",
                                      "Respiratory infectious disease",
                                      "Natural focus disease")))

datafile_bubble <- datafile_plot |> 
     group_by(disease, class, level) |> 
     summarise(value = sum(value),
               .groups = 'drop')
datafile_legend <- data.frame(
     disease = LETTERS[1:4],
     class = 'legend',
     level = 'A',
     value = c(2e4, 2e5, 2e6, 2.6e7)
)

write.csv(rbind(datafile_bubble, datafile_legend),
          './outcome/publish/fig1.csv',
          quote = F,
          row.names = F)

# lineplot ----------------------------------------------------------------

datafile_plot <- datafile_plot  |> 
     group_by(phase, date, class) |> 
     summarise(value = sum(value),
               .groups = 'drop')

fig2 <- ggplot(data = datafile_plot)+
     geom_col(mapping = aes(x = date,
                            y = value,
                            fill = class),
              show.legend = F,
              position = 'fill')+
     scale_fill_manual(values = fill_color)+
     scale_y_continuous(expand = c(0, 0))+
     scale_x_date(expand = expansion(add = c(15, 90)),
                  date_breaks = '1 years',
                  date_labels = '%Y')+
     theme_plot()+
     labs(x = 'Date',
          y = 'Proportion of diseases',
          title = 'C')

fig1 <- ggplot(data = datafile_plot)+
     geom_rect(data = data.frame(start_date = split_date_3,
                                 end_date = split_date_3+90), 
               aes(xmin = start_date, 
                   xmax = end_date), 
               ymin = -Inf, 
               ymax = Inf, 
               fill = "grey",
               alpha = 0.2,
               show.legend = F)+
     annotate('text',
              x = split_date_3,
              y = 9e5,
              label = "Endemic",
              family = "Times New Roman",
              angle = 90,
              vjust = 1,
              hjust = 1)+
     geom_rect(data = data.frame(start_date = split_date_2,
                                 end_date = split_date_3), 
               aes(xmin = start_date, 
                   xmax = end_date), 
               ymin = -Inf, 
               ymax = Inf, 
               fill = "#E9E29CFF",
               alpha = 0.2,
               show.legend = F)+
     annotate('text',
              x = split_date_2,
              y = 9e5,
              label = "Pandemic",
              family = "Times New Roman",
              angle = 90,
              vjust = 1,
              hjust = 1)+
     geom_rect(data = data.frame(start_date = split_date_1,
                                 end_date = split_date_2), 
               aes(xmin = start_date, 
                   xmax = end_date), 
               ymin = -Inf, 
               ymax = Inf, 
               fill = "#EEB479FF",
               alpha = 0.2,
               show.legend = F)+
     annotate('text',
              x = split_date_1,
              y = 9e5,
              label = "Epidemic",
              family = "Times New Roman",
              vjust = 1,
              hjust = 0)+
     geom_rect(data = data.frame(start_date = min(datafile_plot$date)-15,
                                 end_date = split_date_1), 
               aes(xmin = start_date, 
                   xmax = end_date), 
               ymin = -Inf, 
               ymax = Inf, 
               fill = "#91D1C2FF",
               alpha = 0.2,
               show.legend = F)+
     annotate('text',
              x = min(datafile_plot$date)-15,
              y = 9e5,
              label = "Pre-epidemic",
              family = "Times New Roman",
              vjust = 1,
              hjust = 0)+
     geom_line(mapping = aes(x = date,
                             y = value,
                             color = class))+
     scale_color_manual(values = fill_color)+
     scale_y_continuous(expand = c(0, 0),
                        trans = 'log10',
                        label = scientific_10,
                        limits = c(1e3, 1e6),
                        breaks = c(1e3, 1e4, 1e5, 1e6))+
     scale_x_date(expand = expansion(add = c(15, 0)),
                  date_breaks = '1 years',
                  date_labels = '%Y')+
     theme_plot()+
     theme(legend.position = 'bottom')+
     labs(x = NULL,
          y = "Monthly incidence",
          color = NULL,
          title = 'B')

fig1 + fig2 + plot_layout(ncol = 1)

ggsave(filename = './outcome/publish/fig1.pdf',
       width = 14,
       height = 8,
       device = cairo_pdf,
       family = "Times New Roman")

ggsave(filename = './outcome/publish/fig1.png',
       width = 14,
       height = 8)
