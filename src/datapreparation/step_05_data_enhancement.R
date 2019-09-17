## create bond names dataframe ----
bond_names = tibble(classe = c('LFT', 'LTN', 'NTN-B', 'NTN-B Princ', 'NTN-C', 'NTN-F'),
                    old_name = c('Letra Financeira do Tesouro',
                                 'Letra do Tesouro Nacional',
                                 'Notas do Tesouro Nacional – Série B',
                                 'Notas do Tesouro Nacional – Série B Principal',
                                 'Notas do Tesouro Nacional – Série C',
                                 'Notas do Tesouro Nacional – Série F'),
                    new_name = c('Tesouro selic',
                                 'Tesouro pré-fixado',
                                 'Tesouro IPCA com juros semestrais',
                                 'Tesouro IPCA',
                                 'Tesouro IGPM com juros semestrais',
                                 'Tesouro pré-fixado com juros semestrais'),
                    type = c('pós-fixado',
                             'pré-fixado',
                             'índice de preço',
                             'índice de preço',
                             'índice de preço',
                             'pré-fixado'),
                    juros_semestrais = c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))

## calculate bond status dataframe ----
bond_status <- group_by(bond_data, titulo) %>%
  summarise(start = min(dia),
            vencimento = max(vencimento),
            status = ifelse(today() <= vencimento, 'active', 'expired'))
