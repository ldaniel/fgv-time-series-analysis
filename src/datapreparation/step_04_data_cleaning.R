
## remove outliers -----------------------------------------------------------
bond_data <- filter(bond_data, !pu_compra == 0)
bond_data <- filter(bond_data, !pu_venda == 0)
