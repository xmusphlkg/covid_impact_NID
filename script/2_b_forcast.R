
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

datafile_class <- read.xlsx('./data/disease_class.xlsx')

datafile_class <- read.xlsx('./data/model_select_A.xlsx') |> 
     filter(!is.na(best_Method)) |> 
     left_join(datafile_class, by = c(disease = 'diseasename'))
datafile_class$id <- 1:nrow(datafile_class)

split_date <- as.Date("2019/12/1")
# train_length <- 12*10
# test_length <- 12*2
# forcast_length <- test_length+12+12+5

train_length <- 12*12
test_length <- 0
forcast_length <- test_length+12+12+5

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

datafile_class <- data.frame(disease_list = disease_list,
                             disease_name = disease_name) |> 
     right_join(datafile_class, by = c('disease_name' = 'disease')) |> 
     arrange(class, disease_name)

# data clean --------------------------------------------------------------

i <- 7

auto_analysis_function <- function(i){
     datafile_single <- datafile_analysis %>% 
          filter(disease_1 == datafile_class$disease_list[i]) %>% 
          select(date, disease_1, value) %>% 
          complete(
               date = seq.Date(
                    from = min(date),
                    to = max(date),
                    by = 'month'
               ),
               fill = list(value = 0,
                           disease_1 = datafile_class$disease_list[i])
          )
     
     ## simulate date before 2020
     df_simu <- datafile_single  %>% 
          arrange(date) %>% 
          unique() %>% 
          filter(date <= split_date)%>% 
          select(value)
     
     ts_obse_1 <- df_simu %>% 
          ts(frequency = 12,
             start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                       as.numeric(format(min(datafile_single$date), "%m"))))
     
     ts_train_1 <- head(ts_obse_1, train_length)
     # ts_test_1 <- tail(ts_obse_1, test_length)
     
     
     outcome_plot_1 <- datafile_single |> 
          filter(date >= as.Date('2020-01-01'))
     max_case <- max(outcome_plot_1$value)
     
     # Select Method ------------------------------------------------------------
     
     if (datafile_class$Method[i] == 'SARIMA'){
          mod <- auto.arima(ts_train_1, seasonal = T)
          outcome <- forecast(mod, h = forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          ) |> 
               filter(date >= as.Date('2020-01-01'))
     }
     
     if (datafile_class$Method[i] == 'STL'){
          outcome <- tryCatch(stlf(ts_train_1, lambda=0, h=forcast_length), error = function(e) NULL)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          ) |> 
               filter(date >= as.Date('2020-01-01'))
     }
     
     if (datafile_class$Method[i] == 'ETS'){
          outcome <- forecast(ets(ts_train_1), h=forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome$mean)),
               mean = as.matrix(outcome$mean),
               lower_80 = as.matrix(outcome$lower[,1]),
               lower_95 = as.matrix(outcome$lower[,2]),
               upper_80 = as.matrix(outcome$upper[,1]),
               upper_95 = as.matrix(outcome$upper[,2])
          ) |> 
               filter(date >= as.Date('2020-01-01'))
     }
     
     if (datafile_class$Method[i] == 'Neural Network'){
          mod_5 <- nnetar(ts_train_1)
          
          outcome_2 <- forecast(mod_5, h = forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = zoo::as.Date(time(outcome_2$mean)),
               mean = as.matrix(outcome_2$mean),
               lower_80 = NA,
               lower_95 = NA,
               upper_80 = NA,
               upper_95 = NA
          ) |> 
               filter(date >= as.Date('2020-01-01'))
     }
     
     if (datafile_class$Method[i] == 'Grey Model'){
          mod_3 <- gm(ts_train_1, term = forcast_length)
          
          outcome_plot_2 <- data.frame(
               date = outcome_plot_1$date,
               mean = tail(as.matrix(mod_3$forecasts), forcast_length - test_length),
               lower_80 = NA,
               lower_95 = NA,
               upper_80 = NA,
               upper_95 = NA
          )
     }
     
     if (datafile_class$Method[i] == 'Hybrid'){
          mod7 <- hybridModel(ts_train_1, 
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
               filter(date >= as.Date('2020-01-01'))
     }
     
     max_value <- max(outcome_plot_2[,-1], max_case, na.rm = T)
     min_value <- min(outcome_plot_2[,-1], na.rm = T)
     diff_value_1 <- round(sum(head(outcome_plot_1$value, 12), na.rm = T) - sum(head(outcome_plot_2$mean, 12), na.rm = T))
     diff_value_2 <- round(sum(tail(outcome_plot_1$value, 17), na.rm = T) - sum(tail(outcome_plot_2$mean, 17), na.rm = T))
     diff_label_1 <- round(diff_value_1/sum(head(outcome_plot_2$mean, 12), na.rm = T), 3)*100
     diff_label_2 <- round(diff_value_2/sum(tail(outcome_plot_2$mean, 17), na.rm = T), 3)*100
     
     outcome_plot_2 <- outcome_plot_2 |> 
          mutate_at(vars(contains('er')), as.numeric)
     
     write.xlsx(full_join(outcome_plot_2, outcome_plot_1),
                paste0('./outcome/simulate data/A_', datafile_class$disease_name[i], '.xlsx'))
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    size = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    size = 0.7, data = outcome_plot_2)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                      data = outcome_plot_2, alpha = 0.3, show.legend = F)+
          # annotate('text', x = median(head(outcome_plot_1$date, 12)), y = Inf, 
          #          label = paste0(diff_value_1, '\n(', sprintf('%.1f', diff_label_1), '%)'),
          #          color = ifelse(diff_value_1 > 0, 'red', '#019875FF'),
          #          vjust = 1.1,
          #          size = 6)+
          # annotate('text', x = median(tail(outcome_plot_1$date, 17)), y = Inf, 
          #          label = paste0(diff_value_2, '\n(', sprintf('%.1f', diff_label_2), '%)'),
          #          color = ifelse(diff_value_2 > 0, 'red', '#019875FF'),
          #          vjust = 1.1,
          #          size = 6)+
          geom_vline(xintercept = as.Date('2020/12/15'), show.legend = F,
                     linetype = 'longdash')+
          coord_cartesian(ylim = c(0, NA))+
          scale_x_date(expand = expansion(add = c(0, 0)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="1 year"))+
          scale_y_continuous(expand = c(0, 0),
                             breaks = pretty(c(min_value, max_value, 0)),
                             limits = range(pretty(c(min_value, max_value, 0))))+
          scale_color_manual(values = c(Forecasted = "#DC0000B2", Observed = '#3C5488B2'))+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = ifelse(i>20, "Date", ''),
               y = ifelse(i %in% c(5*0:4+1),'Cases', ''),
               color = '',
               title = paste0(LETTERS[i], ': ', datafile_class$disease_name[i]))
     
     return(fig1)
}

# run model ---------------------------------------------------------------

i <- 3
# lapply(1:26, auto_select_function)
auto_analysis_function(23)

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

clusterExport(cl, c('datafile_analysis', 'datafile_class',
                    'forcast_length', 'split_date', 'train_length', 'test_length',
                    'fill_color', 'func_rmse', 'theme_set'), 
              envir = environment())
outcome <- parLapply(cl, 1:25, auto_analysis_function)

stopCluster(cl)

plot <- do.call(wrap_plots, outcome)

ggsave('./fig/20220919_COVID_impact.pdf', 
       plot + plot_layout(ncol = 5, guides = 'collect')&
            theme(legend.position = 'bottom'),
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 16, height = 16)



