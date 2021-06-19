# 类别变量比例函数
cat_per_fun <- function(df, var){
  qvar <- enquo(var)
  df %>% 
    count(!!qvar, sort = TRUE) %>% 
    mutate(percent = n / sum(n))
}

# 抽样检查函数
cat_sample_fun <- function(df, size){
  df %>%
    select(-ID) %>%
    sample_frac(size = size) %>%
    lapply(n_distinct) %>%
    as_tibble() %>%
    bind_cols(tibble(size = size, n = floor(nrow(df) * size))) %>%
    select(size, n, everything())-> result
  return(result)
}

# recipe前预处理函数
prod_prep_before_recipe_fun <- function(file){
  df <- read_csv(file)
  set.seed(100)
  df %>% 
    filter(complete.cases(.)) %>% 
    rename(Primary_type = `Primary Type`, 
           Location_Description = `Location Description`, 
           Community_Area = `Community Area`) %>%
    separate(col = Date, into = c("Date", "Time", "AP"), sep = " ") %>% # 复杂处理放到recipe外处理
    mutate(Date = mdy(Date), Time = hms(Time)) %>% 
    mutate(Hour = if_else(AP == "PM", hour(Time) + 12, hour(Time))) %>% # recipe没有关于time的处理工具
    mutate(Block_type = str_split(Block, " ")) %>% 
    mutate(Block_type = sapply(Block_type, function(x) last(unlist(x)))) %>% 
    select(-c(V1, `Case Number`, IUCR, `FBI Code`, `X Coordinate`, `Y Coordinate`, `Updated On`, 
              Location, Block, Time, AP, Year)) %>% 
    mutate(across(where(is.double) & !c("Latitude", "Longitude", "Date"), as.integer),
           across(where(is.character) | where(is.logical), as.factor)) %>% 
    mutate(Arrest = fct_relevel(Arrest, "TRUE")) %>% 
    arrange(Date) -> df
  return(df)
}