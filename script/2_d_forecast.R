
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

Sys.setlocale("LC_TIME","English")

source('theme_set.R')

datafile_manual <- read.xlsx('./data/df_load_20220902.xlsx', sheet = "Sheet 1")
datafile_manual$date <- convertToDate(datafile_manual$date)

datafile_analysis <- datafile_manual %>% 
     filter(religion == '全国' & type == 'inci' & disease_1 != 'remove') |> 
     filter(date >= as.Date('2008/01/01'))

datafile_class <- read.xlsx('./data/disease_class.xlsx')

datafile_class_A <- read.xlsx('./data/model_select_A.xlsx') |> 
     filter(!is.na(best_Method)) |> 
     left_join(datafile_class, by = c(disease = 'diseasename')) |> 
     arrange(class, disease)

datafile_class_B <- read.xlsx('./data/model_select_B.xlsx') |> 
     filter(!is.na(best_Method)) |> 
     left_join(datafile_class, by = c(disease = 'diseasename')) |> 
     arrange(class, disease)

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

datafile_class_A <- data.frame(disease_list = disease_list,
                             disease_name = disease_name) |> 
     right_join(datafile_class_A, by = c('disease_name' = 'disease')) |> 
     arrange(class, disease_name)
datafile_class_A$id <- 1:nrow(datafile_class_A)

datafile_class_B <- data.frame(disease_list = disease_list,
                               disease_name = disease_name) |> 
     right_join(datafile_class_B, by = c('disease_name' = 'disease')) |> 
     arrange(class, disease_name)
datafile_class_B$id <- 1:nrow(datafile_class_B)

split_date <- as.Date("2019/12/1")
train_length <- 12*12
test_length <- 0
forcast_length <- test_length+12+12+5+12*3
date_value <- seq.Date(as.Date('2022-6-1'), as.Date('2025-5-1'), 'month')

# data clean --------------------------------------------------------------

index <- 20

auto_analysis_function <- function(index){
     
     set.seed(20220812)
     
     datafile_single <- datafile_analysis %>% 
          filter(disease_1 == datafile_class_A$disease_list[index]) %>% 
          select(date, disease_1, value) %>% 
          complete(
               date = seq.Date(
                    from = min(date),
                    to = max(date),
                    by = 'month'
               ),
               fill = list(value = 0,
                           disease_1 = datafile_class_A$disease_list[index])
          )
     
     ## simulate date before 2020
     df_simu_A <- datafile_single  %>% 
          arrange(date) %>% 
          unique() %>% 
          filter(date <= split_date)%>% 
          select(value)
     df_simu_B <- datafile_single  %>% 
          arrange(date) %>% 
          unique() %>% 
          select(value)
     
     ts_obse_A <- df_simu_A %>% 
          ts(frequency = 12,
             start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                       as.numeric(format(min(datafile_single$date), "%m"))))
     ts_obse_B <- df_simu_B %>% 
          ts(frequency = 12,
             start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                       as.numeric(format(min(datafile_single$date), "%m"))))
     
     ts_train_A <- head(ts_obse_A, train_length)
     # ts_test_A <- tail(ts_obse_A, test_length)
     ts_train_B <- ts_obse_B
     
     # Select Method ------------------------------------------------------------
     
     if (datafile_class_A$Method[index] == 'SARIMA'){
          mod <- auto.arima(ts_train_A, seasonal = T)
          outcome <- forecast(mod, h = forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          ) |> 
               filter(date >= as.Date('2022-06-01'))
     }
     
     if (datafile_class_A$Method[index] == 'STL'){
          outcome <- tryCatch(stlf(ts_train_A, lambda=0, h=forcast_length), error = function(e) NULL)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          ) |> 
               filter(date >= as.Date('2022-06-01'))
     }
     
     if (datafile_class_A$Method[index] == 'ETS'){
          outcome <- forecast(ets(ts_train_A), h=forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          ) |> 
               filter(date >= as.Date('2022-06-01'))
     }
     
     if (datafile_class_A$Method[index] == 'Neural Network'){
          mod_5 <- nnetar(ts_train_A)
          
          outcome_2 <- forecast(mod_5, h = forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome_2$mean)),
               mean = as.matrix(outcome_2$mean),
               lower_80 = NA,
               lower_95 = NA,
               upper_80 = NA,
               upper_95 = NA
          ) |> 
               filter(date >= as.Date('2022-06-01'))
     }
     
     if (datafile_class_A$Method[index] == 'Grey Model'){
          mod_3 <- gm(ts_train_A, term = forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = date_value,
               mean = tail(as.matrix(mod_3$forecasts), 12),
               lower_80 = NA,
               lower_95 = NA,
               upper_80 = NA,
               upper_95 = NA
          )
     }
     
     if (datafile_class_A$Method[index] == 'Hybrid'){
          mod7 <- hybridModel(ts_train_A, 
                              models = c('aesn'),
                              a.args = list(seasonal = T),
                              weights="equal", parallel=TRUE, num.cores = 10)
          outcome <- forecast(mod7, h = forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          ) |> 
               filter(date >= as.Date('2022-06-01'))
     }
     
     max_case <- max(outcome_plot_2$mean)

     # select model 2 ----------------------------------------------------------
     
     if (datafile_class_B$Method[index] == 'SARIMA'){
          mod <- auto.arima(ts_train_B, seasonal = T)
          outcome <- forecast(mod, h = 12*3)
          
          outcome_plot_3 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          )
     }
     
     if (datafile_class_B$Method[index] == 'STL'){
          outcome <- tryCatch(stlf(ts_train_B, lambda=0, h=12*3), error = function(e) NULL)
          
          outcome_plot_3 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          )
     }
     
     if (datafile_class_B$Method[index] == 'ETS'){
          outcome <- forecast(ets(ts_train_B), h=12*3)
          
          outcome_plot_3 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          )
     }
     
     if (datafile_class_B$Method[index] == 'Neural Network'){
          mod_5 <- nnetar(ts_train_B)
          
          outcome_2 <- forecast(mod_5, h = 12*3)
          
          outcome_plot_3 <- data.frame(
               date = zoo::as.Date(time(outcome_2$mean)),
               mean = as.matrix(outcome_2$mean),
               lower_80 = NA,
               lower_95 = NA,
               upper_80 = NA,
               upper_95 = NA
          )
     }
     
     if (datafile_class_B$Method[index] == 'Grey Model'){
          mod_3 <- gm(ts_train_B, term = 12*3)
          
          outcome_plot_3 <- data.frame(
               date = date_value,
               mean = as.matrix(mod_3$forecasts),
               lower_80 = NA,
               lower_95 = NA,
               upper_80 = NA,
               upper_95 = NA
          )
     }
     
     if (datafile_class_B$Method[index] == 'Hybrid'){
          mod7 <- hybridModel(ts_train_B, 
                              models = c('aesn'),
                              a.args = list(seasonal = T),
                              weights="equal", parallel=TRUE, num.cores = 10)
          outcome <- forecast(mod7, h = 12*3)
          
          outcome_plot_3 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          )
     }
     
     outcome_plot_2 <- outcome_plot_2 |> 
          mutate_at(vars(contains('er')), as.numeric)
     outcome_plot_3 <- outcome_plot_3 |> 
          mutate_at(vars(contains('er')), as.numeric)
     
     write.xlsx(cbind(outcome_plot_2, outcome_plot_3),
                paste0('./outcome/simulate data/B_', datafile_class_A$disease_name[index], '.xlsx'))
     
     outcome_plot_2[outcome_plot_2<0] <- 0
     outcome_plot_3[outcome_plot_3<0] <- 0
     
     max_value <- max(outcome_plot_2[,-1], outcome_plot_3[,-1], na.rm = T)
     min_value <- min(outcome_plot_2[,-1], outcome_plot_3[,-1], na.rm = T)
     diff_value_1 <- round(sum(head(outcome_plot_2$mean, 12*3), na.rm = T) - sum(head(outcome_plot_3$mean, 12*3), na.rm = T))
     diff_label_1 <- round(diff_value_1/sum(head(outcome_plot_3$mean, 12*3), na.rm = T), 3)*100
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Without COVID-19'),
                    size = 0.7, data = outcome_plot_2)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Within COVID-19'),
                    size = 0.7, data = outcome_plot_3)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = '#3C5488B2'),
                      data = outcome_plot_3, alpha = 0.3, show.legend = F)+
          # annotate('text', x = median(outcome_plot_2$date, 12), y = Inf, 
          #          label = paste0(diff_value_1, '\n(', sprintf('%.1f', diff_label_1), '%)'),
          #          color = ifelse(diff_value_1 > 0, 'red', '#019875FF'),
          #          vjust = 1.1,
          #          size = 6)+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_2$date), max(outcome_plot_3$date)+31, by="year"))+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(values = c('Within COVID-19' = "#DC0000B2",
                                        'Without COVID-19' = '#3C5488B2'))+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = ifelse(index > 20, "Date", ''),
               y = ifelse(index %in% c(5*0:4+1),'Cases', ''),
               color = '',
               title = paste0(LETTERS[index], ': ', datafile_class_A$disease_name[index]))
     remove(outcome_plot_2, outcome_plot_3, index)
     return(fig1)
}

# run model ---------------------------------------------------------------

i <- 16
# lapply(1:26, auto_select_function)
auto_analysis_function(index = 20)

cl <- makeCluster(12)
registerDoParallel(cl)
clusterEvalQ(cl, {
     library(tidyverse)
     library(openxlsx)
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
})

clusterExport(cl, c('datafile_analysis', 'datafile_class_A', 'datafile_class_B',
                    'forcast_length', 'split_date', 'train_length', 'test_length',
                    'fill_color', 'func_rmse', 'theme_set', 'date_value'), 
              envir = environment())
outcome <- parLapply(cl, 1:25, auto_analysis_function)

stopCluster(cl)

plot <- do.call(wrap_plots, outcome)

ggsave('./fig/20221024_COVID_simulate.pdf', 
       plot + plot_layout(ncol = 5, guides = 'collect')&
            theme(legend.position = 'bottom',
                  panel.background = element_blank(),
                  plot.background = element_blank()),
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 16, height = 16)



