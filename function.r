# 类别变量比例函数
cat_per_fun <- function(df, var){
  qvar <- enquo(var)
  df %>% 
    group_by(!!qvar) %>% 
    summarise(n = n()) %>% 
    mutate(percent = n / sum(n)) %>% 
    arrange(-percent)
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