
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(jsonlite)
library(lubridate)

library(stats)
library(tseries)
library(astsa)
library(forecast)
library(greyforecasting)
# library(opera)
library(forecastHybrid)

# loadfonts("pdf")
library(patchwork)
library(Cairo)
library(ggpubr)
library(paletteer)
library(caret)

library(doParallel)

set.seed(202208)

remove(list = ls())

# data load ---------------------------------------------------------------

source('./script/theme_set.R')

datafile_analysis <- read.xlsx('./data/Nation.xlsx', detectDates = T)

split_date_1 <- as.Date('2009/4/1')
split_date_2 <- as.Date('2010/4/1')
start_date <- as.Date('2006/1/1')

train_length <- interval(start_date, split_date_1) %/% months(1)
forcast_length <- interval(split_date_1, split_date_2) %/% months(1)

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

scientific_10 <- function(x) {
     x[x == 0] <- NA
     text <- gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))
     text[is.na(text)] <- 0
     parse(text = text)
}

# data clean --------------------------------------------------------------

i <- 5

auto_select_function <- function(i){
     set.seed(202304)
     datafile_single <- datafile_analysis %>% 
          filter(disease_1 == disease_list[i] & 
                      date <= split_date_2) %>% 
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
     
     ## simulate date before April 2009
     df_simu <- datafile_single  %>% 
          arrange(date) %>% 
          unique() %>% 
          filter(date <= split_date_1)%>% 
          select(value)
     
     max_case <- max(df_simu)
     
     ts_obse_1 <- df_simu %>% 
          ts(frequency = 12,
             start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                       as.numeric(format(min(datafile_single$date), "%m"))))
     
     ts_train_1 <- ts_obse_1
     
     # ARIMA -------------------------------------------------------------------
     
     mod <- auto.arima(ts_train_1, seasonal = T)
     outcome <- forecast(mod, h = forcast_length)
     
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
     
     outcome_plot_1_2_link <- data.frame(
          date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
          value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
                    outcome_plot_2[1, 'mean'])
     )
     
     max_value <- max(c(max(outcome_plot_1[,-1]), max(outcome_plot_2[,-1])), max_case)
     min_value <- min(c(min(outcome_plot_1[,-1]), min(outcome_plot_2[,-1])))
     
     fit_goodness <- data.frame(
          Method = 'SARIMA',
          Train = postResample(outcome_plot_1$simu, outcome_plot_1$fit)
     )
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    linewidth = 0.7, data = filter(datafile_single, date <= split_date_1))+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    linewidth = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    linewidth = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    linewidth = 0.7, data = outcome_plot_2)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="1 years"))+
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2'),
               limits = c("Observed", "Fitted", "Forecasted")
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[5], ': ', "SARIMA"))
     
     fig_arima_1 <- fig1
     
     # GreyModel ---------------------------------------------------------------
     
     mod_3 <- gm(ts_train_1, term = forcast_length)
     
     outcome_plot_1 <- data.frame(
          date = zoo::as.Date(time(mod_3$data)),
          simu = as.numeric(as.matrix(mod_3$data)),
          fit = as.numeric(as.matrix(mod_3$fitted))
     )
     outcome_plot_2 <- data.frame(
          date = seq.Date(split_date_1, by = 'month', length.out = forcast_length),
          mean = as.matrix(mod_3$forecasts)
     )
     
     outcome_plot_1_2_link <- data.frame(
          date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
          value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
                    outcome_plot_2[1, 'mean'])
     )
     
     max_value <- max(c(max(outcome_plot_1[,-1]), max(outcome_plot_2[,-1])), max_case)
     min_value <- min(c(min(outcome_plot_1[,-1]), min(outcome_plot_2[,-1])))
     
     fit_goodness <- fit_goodness |> 
          rbind(
               data.frame(
                    Method = 'Grey Model',
                    Train = postResample(outcome_plot_1$simu, outcome_plot_1$fit)
               )
          )
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    linewidth = 0.7, data = filter(datafile_single, date <= split_date_1))+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    linewidth = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    linewidth = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    linewidth = 0.7, data = outcome_plot_2)+
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="1 years"))+
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2'),
               limits = c("Observed", "Fitted", "Forecasted")
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[1], ': ', "Grey Model"))
     
     fig_grey_1 <- fig1
     
     # NNET --------------------------------------------------------------------
     
     mod_5 <- nnetar(ts_train_1)
     
     outcome_2 <- forecast(mod_5, h = forcast_length)
     
     outcome_plot_1 <- data.frame(
          date = zoo::as.Date(time(outcome_2$x)),
          simu = as.numeric(as.matrix(outcome_2$x)),
          fit = as.numeric(as.matrix(outcome_2$fitted))
     )
     outcome_plot_2 <- data.frame(
          date = zoo::as.Date(time(outcome_2$mean)),
          mean = as.matrix(outcome_2$mean)
     )
     
     outcome_plot_1_2_link <- data.frame(
          date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
          value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
                    outcome_plot_2[1, 'mean'])
     )
     
     max_value <- max(c(max(outcome_plot_1[,-1], na.rm = T), max(outcome_plot_2[,-1], na.rm = T)), max_case)
     min_value <- min(c(min(outcome_plot_1[,-1], na.rm = T), min(outcome_plot_2[,-1], na.rm = T)))
     
     fit_goodness <- fit_goodness |> 
          rbind(
               data.frame(
                    Method = 'Neural Network',
                    Train = postResample(outcome_plot_1$simu[!is.na(outcome_plot_1$fit)], outcome_plot_1$fit[!is.na(outcome_plot_1$fit)])
                    )
          )
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    linewidth = 0.7, data = filter(datafile_single, date <= split_date_1))+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    linewidth = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    linewidth = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    linewidth = 0.7, data = outcome_plot_2)+
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="1 years"))+
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2'),
               limits = c("Observed", "Fitted", "Forecasted")
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[2], ': ', "Neural Network"))
     
     
     fig_nnet_1 <- fig1
     
     # STL ---------------------------------------------------------------------
     
     outcome <- tryCatch(stlf(ts_train_1, lambda=0, h=forcast_length), error = function(e) NULL)
     stl_outcome <- outcome
     
     if(is.null(outcome)){
          fig_stl_1 <- ggplot()+
               theme_void()+
               theme(plot.title.position = "plot", 
                     plot.title = element_text(face = "bold", size = 14, hjust = 0))+
               labs(x = "Date",
                    y = 'Cases',
                    color = '',
                    title = paste0(LETTERS[3], ': ', "STL"))
     } else {
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
          
          outcome_plot_1_2_link <- data.frame(
               date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
               value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
                         outcome_plot_2[1, 'mean'])
          )
          
          max_value <- max(c(max(outcome_plot_1[,-1]), max(outcome_plot_2[,-1])), max_case)
          min_value <- min(c(min(outcome_plot_1[,-1]), min(outcome_plot_2[,-1])))
          
          fit_goodness <- fit_goodness |> 
               rbind(
                    data.frame(
                         Method = 'STL',
                         Train = postResample(outcome_plot_1$simu, outcome_plot_1$fit)
                         )
               )
          
          fig1 <- ggplot()+
               geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                         linewidth = 0.7, data = filter(datafile_single, date <= split_date_1))+
               geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                         linewidth = 0.7, data = outcome_plot_1)+
               geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                         linewidth = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
               geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                         linewidth = 0.7, data = outcome_plot_2)+
               geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                           data = outcome_plot_2, alpha = 0.3, show.legend = F)+
               geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                           data = outcome_plot_2, alpha = 0.3, show.legend = F)+
               geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                          linetype = 'longdash')+
               geom_hline(yintercept = 0, show.legend = F)+
               annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
               coord_cartesian(ylim = c(0, NA))+
               scale_x_date(expand = expansion(add = c(0, 31)),
                            date_labels = '%Y',
                            breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="1 years"))+
               scale_y_continuous(expand = c(0, 0),
                                  label = scientific_10,
                                  breaks = pretty(c(min_value, max_value, 0)),
                                  limits = range(pretty(c(min_value, max_value, 0))))+
               scale_color_manual(
                    values = c(Fitted = "#00A087B2",
                               Forecasted = "#DC0000B2",
                               Observed = '#3C5488B2'),
                    limits = c("Observed", "Fitted", "Forecasted")
               )+
               theme_set()+
               theme(legend.position = 'bottom')+
               labs(x = "Date",
                    y = 'Cases',
                    color = '',
                    title = paste0(LETTERS[3], ': ', "STL"))
          
          fig_stl_1 <- fig1
     }
     
     # ETS ---------------------------------------------------------------------
     
     outcome <- forecast(ets(ts_train_1), h=forcast_length)
     
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
     
     outcome_plot_1_2_link <- data.frame(
          date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
          value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
                    outcome_plot_2[1, 'mean'])
     )
     
     max_value <- max(c(max(outcome_plot_1[,-1]), max(outcome_plot_2[,-1])), max_case)
     min_value <- min(c(min(outcome_plot_1[,-1]), min(outcome_plot_2[,-1])))
     
     fit_goodness <- fit_goodness |> 
          rbind(
               data.frame(
                    Method = 'ETS',
                    Train = postResample(outcome_plot_1$simu, outcome_plot_1$fit)
                    )
          )
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    linewidth = 0.7, data = filter(datafile_single, date <= split_date_1))+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    linewidth = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    linewidth = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    linewidth = 0.7, data = outcome_plot_2)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="1 years"))+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             label = scientific_10,
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2'),
               limits = c("Observed", "Fitted", "Forecasted")
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[4], ': ', "ETS"))
     
     fig_ets_1 <- fig1
     
     # Mixture ts --------------------------------------------------------------
     
     mods <- data.frame(ARIMA = forecast(mod, h = forcast_length)$mean,
                        ETS = forecast(ets(ts_train_1), h=forcast_length)$mean,
                        STL = as.numeric(if (is.null(stl_outcome)) NA else stlf(ts_train_1, lambda=0, h=forcast_length)$mean))
     # mods$value <- as.matrix(ts_test_1)
     
     mod7 <- hybridModel(ts_train_1, 
                         models = c('aesn'),
                         a.args = list(seasonal = T),
                         weights="equal", parallel=TRUE, num.cores = 10)
     outcome <- forecast(mod7, h = forcast_length)
     
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
     
     outcome_plot_1_2_link <- data.frame(
          date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
          value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
                    outcome_plot_2[1, 'mean'])
     )
     
     max_value <- max(c(max(outcome_plot_1[,-1], na.rm = T), max(outcome_plot_2[,-1], na.rm = T)), max_case)
     min_value <- min(c(min(outcome_plot_1[,-1], na.rm = T), min(outcome_plot_2[,-1], na.rm = T)))
     
     fit_goodness <- fit_goodness |> 
          rbind(
               data.frame(
                    Method = 'Hybrid',
                    Train = postResample(outcome_plot_1$simu[!is.na(outcome_plot_1$fit)], outcome_plot_1$fit[!is.na(outcome_plot_1$fit)])
                    )
          )
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    linewidth = 0.7, data = filter(datafile_single, date <= split_date_1))+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    linewidth = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    linewidth = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    linewidth = 0.7, data = outcome_plot_2)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="1 years"))+
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2'),
               limits = c("Observed", "Fitted", "Forecasted")
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[6], ': ', "Hybrid Model"))
     
     fig_hyb_1 <- fig1
     
     
     # Combined plot ------------------------------------------------------------
     
     df_mods <- data.frame(
          grey = as.matrix(mod_3$forecasts),
          arima = mods$ARIMA,
          nnet = as.numeric(outcome_2$mean),
          ets = mods$ETS,
          stl = mods$STL,
          hybrid = outcome_plot_2$mean,
          date = seq.Date(split_date_1, by = 'month', length.out = forcast_length)
     )
     
     max_value <- max(df_mods[,-7], datafile_single$value, max_case, na.rm = T)
     min_value <- min(df_mods[,-7], datafile_single$value, na.rm = T)
     
     legend <- c('Observed', 'Grey Model', 'Neural Network',
                 'STL', 'ETS', 'SARIMA', 'Hybrid Model')
     names(fill_color) <- legend
     legend <- if(is.null(stl_outcome)) legend[-4] else legend
     
     fig1 <- ggplot(data = df_mods)+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    linewidth = 0.7, data = filter(datafile_single, date <= split_date_1))+
          geom_line(mapping = aes(x = date, y = arima, colour = 'SARIMA'),
                    linewidth = 0.7)+
          geom_line(mapping = aes(x = date, y = grey, colour = 'Grey Model'),
                    linewidth = 0.7)+
          geom_line(mapping = aes(x = date, y = nnet, colour = 'Neural Network'),
                    linewidth = 0.7)+
          geom_line(mapping = aes(x = date, y = ets, colour = 'ETS'),
                    linewidth = 0.7)+
          geom_line(mapping = aes(x = date, y = stl, colour = 'STL'),
                    linewidth = 0.7)+
          geom_line(mapping = aes(x = date, y = hybrid, colour = 'Hybrid Model'),
                    linewidth = 0.7)+
          geom_vline(xintercept = min(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="1 years"),
                       date_labels = '%Y')+
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(values = fill_color,
                             limits = legend)+
          theme_set()+
          theme(legend.position = 'right',
                plot.margin = margin(5, 20, 5, 5))+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[7]))+
          guides(color = guide_legend(ncol = 1))
     
     datafile_table <- fit_goodness |>
          mutate(Method = factor(Method,
                                 levels = c('Grey Model', 'Neural Network', 'STL',
                                            'ETS', 'SARIMA', 'Hybrid'),
                                 labels = c('Grey Model', 'Neural Network', 'STL',
                                            'ETS', 'SARIMA', 'Hybrid Model*')),
                 Train = round(Train, 2)) |> 
          arrange(Method)
     datafile_table[is.na(datafile_table)] <- ""
     datafile_table$Index <- str_remove_all(rownames(datafile_table), "[0-9]+")
     
     table1 <- ggtexttable(datafile_table[datafile_table$Index == "RMSE", 1:2],
                           rows = NULL,
                           cols = c('Method', 'Train'),
                           theme = ttheme("blank", base_size = 10, padding = unit(c(20, 5), "mm"))) |>
          tab_add_hline(at.row = nrow(datafile_table)/3+1, row.side = "bottom", linewidth = 1) |>
          tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) |> 
          tab_add_title(paste0(LETTERS[8], " : RMSE of Models"), face = "bold", size = 14) |> 
          tab_add_footnote('*Hybrid: Combined SARIMA, ETS, STL\nand Neural Network model', 
                           just = "left",hjust = 1,size = 10)
     table2 <- ggtexttable(datafile_table[datafile_table$Index == "Rsquared", 1:2],
                           rows = NULL,
                           cols = c('Method', 'Train'),
                           theme = ttheme("blank", base_size = 10, padding = unit(c(20, 5), "mm"))) |>
          tab_add_hline(at.row = nrow(datafile_table)/3+1, row.side = "bottom", linewidth = 1) |>
          tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) |> 
          tab_add_title(paste0(LETTERS[9], " : R-squared of Models"), face = "bold", size = 14) |> 
          tab_add_footnote('*Hybrid: Combined SARIMA, ETS, STL\nand Neural Network model', 
                           just = "left",hjust = 1,size = 10)
     table3 <- ggtexttable(datafile_table[datafile_table$Index == "MAE", 1:2],
                           rows = NULL,
                           cols = c('Method', 'Train'),
                           theme = ttheme("blank", base_size = 10, padding = unit(c(20, 5), "mm"))) |>
          tab_add_hline(at.row = nrow(datafile_table)/3+1, row.side = "bottom", linewidth = 1) |>
          tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) |> 
          tab_add_title(paste0(LETTERS[10], " : MAE of Models"), face = "bold", size = 14) |> 
          tab_add_footnote('*Hybrid: Combined SARIMA, ETS, STL\nand Neural Network model', 
                           just = "left",hjust = 1,size = 10)
     
     # save --------------------------------------------------------------------
     
     fig_ts <- fig_grey_1 + fig_nnet_1 + fig_stl_1 + fig_ets_1 + fig_arima_1 + fig_hyb_1+
          plot_layout(ncol = 2, guides = 'collect')&
          theme(legend.position = 'bottom',
                plot.margin = margin(5, 15, 5, 5))
     
     # fig <- cowplot::plot_grid(fig_ts, fig1, fig_table, ncol = 1, rel_heights = c(3, 1, 1))
     fig <- ggarrange(fig_ts,
                      fig1, 
                      ggarrange(table1, table2, table3, ncol = 3), 
                      ncol = 1, 
                      heights = c(3, 1, 1))
     
     ggsave(filename = paste0('./outcome/appendix/figure/1_flu_epidemic/', disease_name[i],'.pdf'),
            fig,
            width = 14, height = 15, family = "Times New Roman",
            limitsize = FALSE, device = cairo_pdf)
     ggsave(filename = paste0('./outcome/appendix/figure/1_flu_epidemic/', disease_name[i],'.png'),
            fig,
            width = 14, height = 15,
            bg = "white",
            limitsize = FALSE)
     fit_goodness$disease <- disease_name[i]
     
     return(fit_goodness)
}

# auto_select_function(5)

# run model ---------------------------------------------------------------

i <- 6
# lapply(1:26, auto_select_function)
# auto_select_function(6)

cl <- makeCluster(24)
registerDoParallel(cl)
clusterEvalQ(cl, {
     library(tidyverse)
     library(stats)
     library(tseries)
     library(astsa)
     library(forecast)
     library(greyforecasting)
     # library(opera)
     library(forecastHybrid)
     library(caret)
     
     library(ggpubr)
     # loadfonts("pdf")
     library(patchwork)
     library(Cairo)
     library(paletteer) 
     library(lubridate)
     set.seed(20230424)
     
     split_date_1 <- as.Date('2009/4/1')
     split_date_2 <- as.Date('2010/4/1')
     start_date <- as.Date('2006/1/1')
     
     train_length <- interval(start_date, split_date_1) %/% months(1)
     forcast_length <- interval(split_date_1, split_date_2) %/% months(1)
})

clusterExport(cl, c('datafile_analysis', 'disease_list', 'disease_name', 
                    'fill_color', 'func_rmse', 'theme_set', 'scientific_10'), 
              envir = environment())
outcome <- parLapply(cl, 1:24, auto_select_function)
stopCluster(cl)

datafile_outcome <- do.call('rbind', outcome)
write.xlsx(datafile_outcome, './outcome/appendix/model/index/1_flu_epidemic.xlsx')

