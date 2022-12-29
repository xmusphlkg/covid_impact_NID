
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(jsonlite)

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

library(doParallel)

set.seed(202208)

# data load ---------------------------------------------------------------

source('theme_set.R')

datafile_manual <- read.xlsx('./data/df_load_20220902.xlsx', sheet = "Sheet 1")
datafile_manual$date <- convertToDate(datafile_manual$date)

datafile_analysis <- datafile_manual %>% 
     filter(religion == '全国' & type == 'inci' & disease_1 != 'remove') |> 
     filter(date >= as.Date('2008/01/01'))

split_date <- as.Date("2019/12/1")
train_length <- 12*10
test_length <- 12*2
forcast_length <- 12+12+5

disease_list <- c('百日咳', '丙肝', '戊肝', '布病', '登革热', 
                  '肺结核', '风疹', '急性出血性结膜炎', '甲肝', 
                  '痢疾', '淋病', '流行性出血热', '艾滋病',
                  '流行性腮腺炎', '麻疹', '梅毒', '疟疾', '其它感染性腹泻病',
                  '伤寒+副伤寒', '乙肝', '手足口病', '猩红热',
                  '乙型脑炎', '包虫病', '斑疹伤寒')
disease_name <- c('Pertussis', 'HCV', 'HEV',
                  'Brucellosis', 'Dengue fever', 'Tuberculosis',
                  'Rubella', 'Acute hemorrhagic conjunctivitis', 'HAV',
                  'Dysentery', 'Gonorrhea', 'HFRS',
                  'AIDS', 'Mumps', 'Measles',
                  'Syphilis', 'Malaria', 'Other infectious diarrhea',
                  'Typhoid fever and paratyphoid fever', 'HBV', 'HFMD',
                  'scarlet fever', 'Japanese encephalitis', 'Hydatidosis', 'Typhus')

# data clean --------------------------------------------------------------

i <- 5

auto_select_function <- function(i){
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
     
     max_case <- max(df_simu)
     
     ts_obse_1 <- df_simu %>% 
          ts(frequency = 12,
             start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                       as.numeric(format(min(datafile_single$date), "%m"))))
     
     ts_train_1 <- head(ts_obse_1, train_length)
     ts_test_1 <- tail(ts_obse_1, test_length)
     
     # ARIMA -------------------------------------------------------------------
     
     mod <- auto.arima(ts_train_1, seasonal = T)
     outcome <- forecast(mod, h = test_length)
     
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
          Train = func_rmse(outcome_plot_1$simu, outcome_plot_1$fit),
          Test = func_rmse(ts_test_1, outcome_plot_2$mean),
          'Train and Test' = func_rmse(ts_obse_1, c(outcome_plot_1$fit, outcome_plot_2$mean))
     )
     
     fig1 <- ggplot()+
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
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          annotate('text', x = median(outcome_plot_2$date), y = Inf, label = 'Test Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="2 years"))+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2')
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[5], ': ', "SARIMA"))
     
     fig_arima_1 <- fig1
     
     # GreyModel ---------------------------------------------------------------
     
     mod_3 <- gm(ts_train_1, term = test_length)
     
     outcome_plot_1 <- data.frame(
          date = zoo::as.Date(time(mod_3$data)),
          simu = as.numeric(as.matrix(mod_3$data)),
          fit = as.numeric(as.matrix(mod_3$fitted))
     )
     outcome_plot_2 <- data.frame(
          date = seq.Date(as.Date('2018/01/01'), by = 'month', length.out = test_length),
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
                    Train = func_rmse(outcome_plot_1$simu, outcome_plot_1$fit),
                    Test = func_rmse(ts_test_1, outcome_plot_2$mean),
                    'Train and Test' = func_rmse(ts_obse_1, c(outcome_plot_1$fit, outcome_plot_2$mean))
               )
          )
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    size = 0.7, data = filter(datafile_single, date <= split_date))+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    size = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    size = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    size = 0.7, data = outcome_plot_2)+
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          annotate('text', x = median(outcome_plot_2$date), y = Inf, label = 'Test Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="2 years"))+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2')
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
     
     outcome_2 <- forecast(mod_5, h = test_length)
     
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
                    Train = func_rmse(outcome_plot_1$simu, outcome_plot_1$fit),
                    Test = func_rmse(ts_test_1, outcome_plot_2$mean),
                    'Train and Test' = func_rmse(ts_obse_1, c(outcome_plot_1$fit, outcome_plot_2$mean)))
          )
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    size = 0.7, data = filter(datafile_single, date <= split_date))+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    size = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    size = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    size = 0.7, data = outcome_plot_2)+
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          annotate('text', x = median(outcome_plot_2$date), y = Inf, label = 'Test Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="2 years"))+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2')
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[2], ': ', "Neural Network"))
     
     
     fig_nnet_1 <- fig1
     
     # STL ---------------------------------------------------------------------
     
     outcome <- tryCatch(stlf(ts_train_1, lambda=0, h=test_length), error = function(e) NULL)
     stl_outcome <- outcome
     
     if(is.null(outcome)){
          fig_stl_1 <- ggplot()+
               theme_set()+
               theme(legend.position = 'bottom')+
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
                         Train = func_rmse(outcome_plot_1$simu, outcome_plot_1$fit),
                         Test = func_rmse(ts_test_1, outcome_plot_2$mean),
                         'Train and Test' = func_rmse(ts_obse_1, c(outcome_plot_1$fit, outcome_plot_2$mean)))
               )
          
          fig1 <- ggplot()+
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
               geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                          linetype = 'longdash')+
               geom_hline(yintercept = 0, show.legend = F)+
               annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
               annotate('text', x = median(outcome_plot_2$date), y = Inf, label = 'Test Database', vjust = 1)+
               coord_cartesian(ylim = c(0, NA))+
               scale_x_date(expand = expansion(add = c(0, 31)),
                            date_labels = '%Y',
                            breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="2 years"))+
               scale_y_continuous(expand = c(0, 0),
                                  breaks = pretty(c(min_value, max_value, 0)),
                                  limits = range(pretty(c(min_value, max_value, 0))))+
               scale_color_manual(
                    values = c(Fitted = "#00A087B2",
                               Forecasted = "#DC0000B2",
                               Observed = '#3C5488B2')
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
     
     outcome <- forecast(ets(ts_train_1), h=test_length)
     
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
                    Train = func_rmse(outcome_plot_1$simu, outcome_plot_1$fit),
                    Test = func_rmse(ts_test_1, outcome_plot_2$mean),
                    'Train and Test' = func_rmse(ts_obse_1, c(outcome_plot_1$fit, outcome_plot_2$mean)))
          )
     
     fig1 <- ggplot()+
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
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          annotate('text', x = median(outcome_plot_2$date), y = Inf, label = 'Test Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="2 years"))+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2')
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[4], ': ', "ETS"))
     
     fig_ets_1 <- fig1
     
     # Mixture ts --------------------------------------------------------------
     
     mods <- data.frame(ARIMA = forecast(mod, h = test_length)$mean,
                        ETS = forecast(ets(ts_train_1), h=test_length)$mean,
                        STL = as.numeric(if (is.null(stl_outcome)) NA else stlf(ts_train_1, lambda=0, h=test_length)$mean))
     # mods$value <- as.matrix(ts_test_1)
     
     mod7 <- hybridModel(ts_train_1, 
                         models = c('aesn'),
                         a.args = list(seasonal = T),
                         weights="equal", parallel=TRUE, num.cores = 10)
     outcome <- forecast(mod7, h = test_length)
     
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
                    Train = func_rmse(outcome_plot_1$simu, outcome_plot_1$fit),
                    Test = func_rmse(ts_test_1, outcome_plot_2$mean),
                    'Train and Test' = func_rmse(ts_obse_1, c(outcome_plot_1$fit, outcome_plot_2$mean)))
          )
     
     fig1 <- ggplot()+
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
          geom_vline(xintercept = max(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          annotate('text', x = median(outcome_plot_2$date), y = Inf, label = 'Test Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="2 years"))+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(
               values = c(Fitted = "#00A087B2",
                          Forecasted = "#DC0000B2",
                          Observed = '#3C5488B2')
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[6], ': ', "Hybrid"))
     
     fig_hyb_1 <- fig1
     
     
     # Combined plot ------------------------------------------------------------
     
     df_mods <- data.frame(
          grey = as.matrix(mod_3$forecasts),
          arima = mods$ARIMA,
          nnet = as.numeric(outcome_2$mean),
          ets = mods$ETS,
          stl = mods$STL,
          hybrid = outcome_plot_2$mean,
          date = seq.Date(as.Date('2018/01/01'), by = 'month', length.out = test_length)
     )
     
     max_value <- max(df_mods[,-7], datafile_single$value, max_case, na.rm = T)
     min_value <- min(df_mods[,-7], datafile_single$value, na.rm = T)
     
     names(fill_color) <- c('Observed', 'Grey Model', 'Neural Network',
                            'STL', 'ETS', 'SARIMA', 'Hybrid')
     
     fig1 <- ggplot(data = df_mods)+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    size = 0.7, data = filter(datafile_single, date <= split_date))+
          geom_line(mapping = aes(x = date, y = arima, colour = 'SARIMA'),
                    size = 0.7)+
          geom_line(mapping = aes(x = date, y = grey, colour = 'GreyModel'),
                    size = 0.7)+
          geom_line(mapping = aes(x = date, y = nnet, colour = 'NNET'),
                    size = 0.7)+
          geom_line(mapping = aes(x = date, y = ets, colour = 'ETS'),
                    size = 0.7)+
          geom_line(mapping = aes(x = date, y = stl, colour = 'STL'),
                    size = 0.7)+
          geom_line(mapping = aes(x = date, y = hybrid, colour = 'Hybrid'),
                    size = 0.7)+
          geom_vline(xintercept = min(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train Database', vjust = 1)+
          annotate('text', x = median(outcome_plot_2$date), y = Inf, label = 'Test Database', vjust = 1)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="2 years"),
                       date_labels = '%Y')+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(values = fill_color)+
          theme_set()+
          theme(legend.position = 'bottom',
                plot.margin = margin(5, 20, 5, 5))+
          labs(x = "Date",
               y = 'Cases',
               color = '',
               title = paste0(LETTERS[7]))+
          guides(color = guide_legend(nrow = 1))
     
     datafile_table <- fit_goodness |>
          mutate(Method = factor(Method,
                                 levels = c('Grey Model', 'Neural Network', 'STL',
                                            'ETS', 'SARIMA', 'Hybrid'),
                                 labels = c('Grey Model', 'Neural Network', 'STL',
                                            'ETS', 'SARIMA', 'Hybrid*')),
                 Train = round(Train),
                 Test = round(Test),
                 Train.and.Test = round(Train.and.Test)) |> 
          arrange(Method)
     datafile_table[is.na(datafile_table)] <- ""
     
     table <- ggtexttable(datafile_table,
                          rows = NULL,
                          cols = c('Method', 'Train', 'Test', 'Train and Test'),
                          theme = ttheme("blank", base_size = 10, padding = unit(c(5, 5), "mm"))) |>
          tab_add_hline(at.row = nrow(datafile_table)+1, row.side = "bottom", linewidth = 1) |>
          tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) |> 
          tab_add_title(LETTERS[8], face = "bold", size = 14) |> 
          tab_add_footnote('*Hybrid: Combined SARIMA, ETS, STL\nand Neural Network model', 
                           just = "left",hjust = 1,size = 10)
     
     fig_com <- fig1 + table + plot_layout(widths = c(1.7, 1))
     
     # save --------------------------------------------------------------------
     
     fig_ts <- fig_grey_1 + fig_nnet_1 + fig_stl_1 + fig_ets_1 + fig_arima_1 + fig_hyb_1+
          plot_layout(ncol = 2, guides = 'collect')&
          theme(legend.position = 'bottom',
                plot.margin = margin(5, 15, 5, 5))
     
     cowplot::plot_grid(fig_ts, fig_com, ncol = 1, rel_heights = c(3, 1))
     
     ggsave(filename = paste0('./fig/20221013/A_', disease_name[i],'.pdf'),
            width = 14, height = 15, family = "Times New Roman",
            limitsize = FALSE, device = cairo_pdf)
     fit_goodness$disease <- disease_name[i]
     
     return(fit_goodness)
}

# run model ---------------------------------------------------------------

i <- 6
# lapply(1:26, auto_select_function)
# auto_select_function(6)

cl <- makeCluster(10)
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
     
     # loadfonts("pdf")
     library(patchwork)
     library(Cairo)
     library(ggpubr)
     library(paletteer) 
     set.seed(202208)
     
     
     split_date <- as.Date("2019/12/1")
     train_length <- 12*10
     test_length <- 12*2
     forcast_length <- 12+12+5
})

clusterExport(cl, c('datafile_analysis', 'disease_list', 'disease_name', 
                    'fill_color', 'func_rmse', 'theme_set'), 
              envir = environment())
outcome <- parLapply(cl, 1:25, auto_select_function)
stopCluster(cl)

datafile_outcome <- do.call('rbind', outcome)
write.xlsx(datafile_outcome, './outcome/model_select_A.xlsx')

