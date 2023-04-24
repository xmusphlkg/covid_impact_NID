
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)

# data --------------------------------------------------------------------

# DataRaw <- read.xlsx("./data/df_load_20220902.xlsx")
# head(DataRaw)
# DataRaw$date <- convertToDate(DataRaw$date)
# DataNation <- DataRaw |> 
#      filter(religion == '全国' & type == 'inci' & disease_1 != 'remove') |> 
#      filter(date >= as.Date('2008/01/01'))
# table(DataNation$disease_1)
# 
# write.xlsx(DataNation,
#            file = "./Nation.xlsx")

DataNation <- read.xlsx('./data/Nation.xlsx', detectDates = T)
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

DataNation <- DataNation |> 
     filter(disease_1 %in% disease_list)
table(DataNation$disease_1)

DataTable <- DataNation |> 
     group_by(disease_1, date) |> 
     count() |> 
     pivot_wider(names_from = disease_1,
                 values_from = n,
                 values_fill = 0)
