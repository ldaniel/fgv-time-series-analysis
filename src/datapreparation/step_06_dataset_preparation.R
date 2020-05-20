# dataset preparation ---------------------------------------------------------

## filter Selic data -----
selic_actual        <- filter(selic_actual, Date >= make_date(2002, 1, 1))
selic_actual$Value  <- selic_actual$Value / 100

selic_target        <- filter(selic_target, Date >= make_date(2002, 1, 1))
selic_target$Value  <- selic_target$Value / 100

## filter IPCA data -----
ipca_12m        <- filter(ipca_12m, Date >= make_date(2002, 1, 1))
ipca_12m$Value  <- ipca_12m$Value / 100

# Create Time Series ----------------------------------------------------------

df <- filter(bond_data, titulo == "LTN 2021-01-01") %>% 
  select(dia, taxa_venda) %>% 
  arrange(dia)

dates <- seq(min(df$dia), max(df$dia), by = "day") %>% as_tibble
colnames(dates) <- c('dia')

df <- left_join(dates, df, by = 'dia')
df$isWeekend <- ifelse(wday(df$dia) %in% c(7, 1), 1, 0)
df$isHoliday <- ifelse(df$dia %in% holidaysANBIMA, 1, 0)
df$taxa_venda = na.locf(df$taxa_venda, na.rm = TRUE) * 100

df$date <- make_date(year(df$dia), month(df$dia), 1)
 
df <- group_by(df, date) %>%
   summarise(taxa_venda = mean(taxa_venda))
 
saveRDS(df, 'data/processed/target_dataset.rds')
