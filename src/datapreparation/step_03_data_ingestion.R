# performing data loading -----------------------------------------------------
dataDirectory <- paste(stringr::str_replace(getwd(),"/markdown",""), "/data/raw/", sep = "")

# get bond data -----
files = list.files(dataDirectory, full.names = TRUE)

temp_false = tibble()
temp_true = tibble()

for (file in files) {
  if (grepl('historico', file) == FALSE) {
    print(file)
    readxl::excel_sheets(file)
    for (sheet in excel_sheets(file)) {
      temp_false = read_xls(file, skip = 1, sheet = sheet) %>%
        mutate(classe = trimws(str_extract(trimws(sheet), '[A-Z|\\-\\s|[A-Za-z]]+')),
               vencimento = dmy(str_extract(trimws(sheet), '\\d+$')),
               titulo = paste(classe, vencimento),
               source = file) %>%
        bind_rows(temp_false)
    }
  } else {
    print(file)
    readxl::excel_sheets(file)
    for (sheet in excel_sheets(file)) {
      temp_true = read_xls(file, skip = 1, sheet = sheet, col_types = 'text') %>%
        mutate(classe = trimws(str_extract(trimws(sheet), '[A-Z|\\-\\s|[A-Za-z]]+')),
               vencimento = dmy(str_extract(trimws(sheet), '\\d+$')),
               titulo = paste(classe, vencimento),
               source = file) %>%
        bind_rows(temp_true)
    }
  }
}

temp_true <- filter(temp_true, !is.na(temp_true$Dia)) %>%
  select(-matches('\\.')) %>%
  mutate(Dia = as.Date(as.numeric(Dia), origin = "1899-12-30", optional = TRUE)) %>%
  type_convert() %>%
  mutate(pu_base = ifelse(is.na(`PU Extrato 9:00`), `PU Base 9:00`, `PU Extrato 9:00`)) %>%
  select(Dia, `Taxa Compra 9:00`, `Taxa Venda 9:00`, `PU Compra 9:00`, `PU Venda 9:00`,
         `pu_base`, classe, vencimento, titulo, source)

temp_false <- filter(temp_false, !is.na(temp_false$Dia)) %>%
  mutate(dia1 = parse_date(Dia, format = '%d/%m/%Y'),
         dia2 = as.Date(as.numeric(Dia), origin = "1899-12-30", optional = TRUE),
         Dia = as.Date(as.numeric(ifelse(is.na(dia1), dia2, dia1)),
                       origin = "1970-01-01", optional = TRUE)) %>%
  select(-dia1, -dia2)

colnames(temp_false) <- c('dia', 'taxa_compra', 'taxa_venda',
                          'pu_compra', 'pu_venda', 'pu_base',
                          'classe', 'vencimento', 'titulo', 'source')

colnames(temp_true) <- c('dia', 'taxa_compra', 'taxa_venda',
                         'pu_compra', 'pu_venda', 'pu_base',
                         'classe', 'vencimento', 'titulo', 'source')

df <- bind_rows(temp_false, temp_true)
df <- filter(df, !is.na(df$taxa_compra))

df$classe = plyr::mapvalues(df$classe,
                            c('NTNB', 'NTNBP', 'NTNC', 'NTNF', 'NTN-B Principal'),
                            c('NTN-B', 'NTN-B Princ', 'NTN-C', 'NTN-F', 'NTN-B Princ'))

df$titulo <- paste(df$classe, df$vencimento)

bond_data <- df

rm(temp_false, temp_true, file, files, sheet, df)
gc()

## get Selic data -----
selic_actual  <- Quandl("BCB/4189", api_key = "-xkrwaz77Fyu-Wd-fs2y")
selic_target  <- Quandl("BCB/432", api_key = "-xkrwaz77Fyu-Wd-fs2y")

## get IPCA data -----
ipca_12m  <- Quandl("BCB/13522", api_key = "-xkrwaz77Fyu-Wd-fs2y")
