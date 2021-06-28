# 类别变量比例函数
cat_per_fun <- function(df, var){
  qvar <- enquo(var)
  df %>% 
    count(!!qvar, sort = TRUE) %>% 
    mutate(proportion = n / sum(n))
}

# 抽样检查函数
cat_sample_fun <- function(df, size){
  df %>%
    select(-ID, -Date) %>%
    sample_frac(size = size) %>%
    lapply(n_distinct) %>%
    as_tibble() %>%
    bind_cols(tibble(size = size, n = floor(nrow(df) * size))) %>%
    select(size, n, everything()) -> result
  return(result)
}

# 抽样检查函数:按比例
cat_sample_prop_fun <- function(df, size){
  dis_sample <- df %>% select(-ID, -Date) %>% sample_frac(size = size) %>% lapply(n_distinct) %>% as_tibble()
  prop <- dis_sample / dis_full
  prop <- bind_cols(tibble(size = size, n = floor(nrow(df) * size)), prop)
  return(prop)
}

# recipe前预处理函数
production_prediction_fun <- function(file){
  
  # 读入数据
  df <- read_csv(file)
  
  # recipe前处理
  df %>% 
    filter(complete.cases(.)) %>% 
    rename(Primary_type = `Primary Type`, 
           Location_description = `Location Description`, 
           Community_area = `Community Area`,
           FBI_code = `FBI Code`) %>%
    separate(col = Date, into = c("Date", "Time", "AP"), sep = " ") %>% # 复杂处理放到recipe外处理
    mutate(Date = mdy(Date), Time = hms(Time)) %>% 
    mutate(Month = month(Date, label = FALSE),
           Wday = wday(Date, label = FALSE),
           Hour = if_else(AP == "PM", hour(Time) + 12, hour(Time))) %>% # recipe没有关于time的处理工具
    mutate(Block_type = str_split(Block, " ")) %>% 
    mutate(Block_type = sapply(Block_type, function(x) last(unlist(x)))) %>% 
    select(-c(V1, `Case Number`, IUCR, `X Coordinate`, `Y Coordinate`, `Updated On`, 
              Location, Block, Time, AP, Year)) %>% 
    mutate(across(c("Beat", "District", "Ward", "Community_area"), as.factor),
           across(where(is.character) | where(is.logical), as.factor),
           across(where(is.double) & !c("Latitude", "Longitude", "Date"), as.integer)) %>% 
    mutate(Arrest = fct_relevel(Arrest, "TRUE")) %>% 
    arrange(Date) -> df
  
  # 结果预测
  rf_wf_final_fit <- read_rds("rf_wf_final_fit_new.rds")
  prod_pred <- rf_wf_final_fit %>% predict(df) %>% bind_cols(df) %>% select(ID, .pred_class) %>% rename(prediction = .pred_class)
  
  # 返回结果
  return(prod_pred)

  }

# 因变量比例计算函数
y_prop_fun <- function(df, var){
  qvar <- enquo(var)
  df %>% 
    count(!!qvar, Arrest) %>% 
    group_by(!!qvar) %>% 
    mutate(proportion = n / sum(n)) %>% 
    filter(Arrest == TRUE) %>% 
    select(-c(n, Arrest)) -> df
  return(df)
}
