# dataset preparation ---------------------------------------------------------

## filter Selic data -----
selic_actual        <- filter(selic_actual, Date >= make_date(2002, 1, 1))
selic_actual$Value  <- selic_actual$Value / 100

selic_target        <- filter(selic_target, Date >= make_date(2002, 1, 1))
selic_target$Value  <- selic_target$Value / 100

## filter IPCA data -----
ipca_12m        <- filter(ipca_12m, Date >= make_date(2002, 1, 1))
ipca_12m$Value  <- ipca_12m$Value / 100

## saving processed dataset -----

# remover este comentário e as 2 linhas abaixo depois que tiver o dataset final
# library(readxl)
# target_dataset <- read_excel("D:/OneDrive/FGV/04_Modulos/08 - Analise de Series Temporais/Trabalho Individual/DepartmentStoreSales_V2.xls")

if (!file.exists('./data/processed/target_dataset.rds')) {
  saveRDS(target_dataset, './data/processed/target_dataset.rds')
}
