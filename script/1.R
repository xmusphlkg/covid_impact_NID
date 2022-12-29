
# loading packages --------------------------------------------------------

library(tidyverse)
library(stats)
library(tseries)
library(astsa)
library(forecast)

library(openxlsx)
library(jsonlite)

loadfonts("pdf")
library(patchwork)
library(Cairo)

# data --------------------------------------------------------------------

remove(list = ls())

source('theme_set.R')
# load(file = '../data/source/source.RData')
datafile_manual <- read.xlsx('./data/df_load_202205.xlsx', sheet = "Sheet 1")
datafile_manual$date <- convertToDate(datafile_manual$date)

datafile_analysis <- datafile_manual %>% 
     filter(religion == '全国' & type == 'inci' & disease_1 != 'remove')


split_date <- as.Date("2019/12/1")

disease_list <- c('百日咳', '丙肝', '戊肝', '病毒性肝炎', '布病', '登革热', 
                  '肺结核', '风疹', '急性出血性结膜炎', '甲肝', 
                  '痢疾', '淋病', '流行性出血热', '流行性感冒',
                  '流行性腮腺炎', '麻疹', '梅毒', '疟疾', '其它感染性腹泻病',
                  '伤寒+副伤寒', '乙肝', '手足口病', '猩红热')
disease_name <- c('Pertussis', 'HCV', 'HEV','Viral hepatitis',
                  'Brucellosis', 'Dengue fever', 'Tuberculosis',
                  'Rubella', 'Acute hemorrhagic conjunctivitis', 'HAV',
                  'Dysentery', 'Gonorrhea', 'HFRS',
                  'Influenza', 'Mumps', 'Measles',
                  'Syphilis', 'Malaria', 'Other infectious diarrhea',
                  'Typhoid fever and paratyphoid fever', 'HBV', 'HFMD',
                  'scarlet fever'
)

i <- 17

# one plot ----------------------------------------------------------------

patch_plot <- function(i){
     datafile_single <- datafile_analysis %>% 
          filter(disease_1 == disease_list[i]) %>% 
          select(date, disease_1, value) %>% 
          complete(
               date = seq.Date(
                    from = min(date),
                    to = max(date),
                    by = 'month'
               ),
               fill = list(value = 0,
                           disease_1 = disease_list[i])
          )
     
     ## simulate date before 2020
     df_simu <- datafile_single  %>% 
          arrange(date) %>% 
          unique() %>% 
          filter(date <= split_date)%>% 
          select(value)
     
     ts_simu_1 <- df_simu %>% 
          ts(frequency = 12,
             start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                       as.numeric(format(min(datafile_single$date), "%m"))))
     
     ## simulate date after 2020
     df_simu <- datafile_single  %>% 
          arrange(date) %>% 
          unique() %>%
          filter(date >= split_date)%>% 
          select(value)
     
     ts_simu_2 <- df_simu %>% 
          ts(frequency = 12,
             start = c(2019, 12))
     
     
     mod <- auto.arima(ts_simu_1, seasonal = T)
     mod_2 <- auto.arima(ts_simu_2, seasonal = F)
     
     # outcome <- forecast(mod, h = length(ts_simu_2) - 1)
     outcome <- forecast(mod, h = length(ts_simu_2) + 35)
     outcome_1 <- forecast(mod_2, h = 36)
     
     outcome_plot_1 <- data.frame(
          date = zoo::as.Date(time(outcome$x)),
          simu = as.numeric(as.matrix(outcome$x)),
          fit = as.numeric(as.matrix(outcome$fitted))
     )
     outcome_plot_2 <- data.frame(
          date = zoo::as.Date(time(outcome$mean)),
          mean = as.matrix(outcome$mean),
          lower_80 = as.matrix(outcome$lower[,1]),
          lower_95 = as.matrix(outcome$lower[,2]),
          upper_80 = as.matrix(outcome$upper[,1]),
          upper_95 = as.matrix(outcome$upper[,2])
     )
     # outcome_plot_2[outcome_plot_2 < 0] <- 0
     
     outcome_plot_3 <- data.frame(
          date = zoo::as.Date(time(outcome_1$x)),
          simu = as.numeric(as.matrix(outcome_1$x)),
          fit = as.numeric(as.matrix(outcome_1$fitted))
     )
     outcome_plot_4 <- data.frame(
          date = zoo::as.Date(time(outcome_1$mean)),
          mean = as.matrix(outcome_1$mean),
          lower_80 = as.matrix(outcome_1$lower[,1]),
          lower_95 = as.matrix(outcome_1$lower[,2]),
          upper_80 = as.matrix(outcome_1$upper[,1]),
          upper_95 = as.matrix(outcome_1$upper[,2])
     )
     # outcome_plot_4[outcome_plot_4 < 0] <- 0
     
     outcome_plot_1_2_link <- data.frame(
          date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
          value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
                    outcome_plot_2[1, 'mean'])
     )
     
     outcome_plot_3_4_link <- data.frame(
          date = c(max(outcome_plot_3$date), min(outcome_plot_4$date)),
          value = c(outcome_plot_3[nrow(outcome_plot_3), 'fit'],
                    outcome_plot_4[1, 'mean'])
     )
     
     max_value <- max(c(max(outcome_plot_1[,-1]),
                        max(outcome_plot_2[,-1]),
                        max(outcome_plot_3[,-1]),
                        max(outcome_plot_4[,-1])))
     
     min_value <- min(c(min(outcome_plot_1[,-1]),
                        min(outcome_plot_2[,-1]),
                        min(outcome_plot_3[,-1]),
                        min(outcome_plot_4[,-1])))
     
     ## save outcome
     df_out_temp <- list(
          disease_name = c(disease_list[i], disease_name[i]),
          outcome_before = summary(mod),
          # outcome_all_defore = mod,
          shapiro_before = shapiro.test(mod$residuals)$p.value,
          residual_before = Box.test(mod$residuals, lag = 24, type = "Ljung-Box")$p.value,
          outcome_after = summary(mod_2),
          # outcome_all_after = mod_2,
          shapiro_after = shapiro.test(mod_2$residuals)$p.value,
          residual_after = Box.test(mod_2$residuals, lag = 24, type = "Ljung-Box")$p.value
     )
     df_out_temp <- capture.output(df_out_temp)
     
     # df_out_temp <- cat(disease_list[i], df_out_temp, paste0('./outcome/', disease_name[i],'.txt'), 
     # sep="\n", append=TRUE)
     # write.table(df_out_temp, paste0('./outcome/', disease_name[i],'.txt'),
     #             quote = F, row.names = F)
     
     # write_json(df_out_temp[[2]], path = paste0('./outcome/', disease_name[i],'.json'))
     # write.xlsx(list(outcome_plot_1, outcome_plot_2, outcome_plot_3, outcome_plot_4),
     #            file = paste0('./outcome/', disease_name[i],'.xlsx'), overwrite = T)
     
     ## plot outcome
     fig1 <- ggplot()+
          # geom_line(mapping = aes(x = date, y = simu, colour = 'Real'), 
          #           size = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    size = 0.7, data = filter(datafile_single, date <= split_date))+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    size = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    size = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    size = 0.7, data = outcome_plot_2)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          geom_vline(xintercept = min(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          scale_x_date(expand = c(0, 31),
                       date_breaks = "2 years",
                       date_labels = '%Y')+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          # Link = 'black',
                          # Link = '#DC0000B2',
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2')
          )+
          theme_set()+
          theme(legend.position = 'bottom',
                plot.margin = margin(0, 0, 0, 0))+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0('Fig. ', LETTERS[i], '1:', disease_name[i]))
     fig1
     # assign(paste0('fig_outcome_1_', i), fig1)
     
     fig2 <- ggplot()+
          geom_line(mapping = aes(x = date, y = simu, colour = 'Observed'), 
                    size = 0.7, data = outcome_plot_3, show.legend = F)+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    size = 0.7, data = outcome_plot_3, show.legend = F)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    size = 0.7, data = outcome_plot_3_4_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    size = 0.7, data = outcome_plot_4, show.legend = F)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                      data = outcome_plot_4, alpha = 0.3, show.legend = F)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                      data = outcome_plot_4, alpha = 0.3, show.legend = F)+
          geom_vline(xintercept = min(outcome_plot_3_4_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          scale_x_date(expand = c(0, 31),
                       date_breaks = "2 years",
                       date_labels = '%Y')+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2')
          )+
          theme_set()+
          theme(legend.position = 'bottom',
                plot.margin = margin(0, 0, 0, 20))+
          labs(x = "Date",
               y = NULL,
               color = '',
               title = paste0('Fig. ', LETTERS[i], '2'))
     
     # assign(paste0('fig_outcome_2_', i), fig2)
     
     fig <- (fig1 + fig2) + 
          plot_layout(ncol = 2, byrow = F, width = c(19, 5), tag_level = 'new')
     # plot_annotation(i)& theme(plot.title = element_text(hjust = 0))
     return(fig)
}

# bind plot ---------------------------------------------------------------

plot <- do.call(wrap_plots, lapply(1:length(disease_list), patch_plot))

# patchwork <- (fig_patchwork_1) /
#      (fig_patchwork_2) /
#      (fig_patchwork_3)

fig <- plot+
     plot_layout(ncol = 1, byrow = F)+
     plot_layout(guides = "collect")& 
     theme(legend.position = 'bottom')




ggsave('./fig/20220830.pdf', fig, height = 75, width = 12, limitsize = FALSE, device = cairo_pdf)
# ggsave('./test5.tiff', height = 75, width = 12, limitsize = FALSE)
# ggsave('./test5.eps', height = 75, width = 12, limitsize = FALSE, device = cairo_ps)



df <- data.frame(
     disease_list,
     disease_name,
     LETTERS[1:length(disease_list)]
)


















