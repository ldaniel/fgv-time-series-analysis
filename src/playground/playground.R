# source scripts -------------------------------------------------------------

source("./src/datapreparation/execute_data_preparation.R")

# analisys -------------------------------------------------------------------

## Histórico da Taxa de Venda - NTN-B ----------------------------------------
plot_NTNB_premium_trend <- left_join(bond_data, bond_names, by = 'classe') %>%
  left_join(bond_status, by = 'titulo') %>%
  filter(classe == 'NTN-B' | classe == 'NTN-B Princ',
         status == 'active') %>%
  ggplot(aes(x = dia)) +
  geom_line(aes(color = titulo, y = taxa_venda), size = 0.2) +
  geom_smooth(aes(y = taxa_compra), method = 'loess') +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0( x * 100, "%"),
                     limits = c(0, 0.1)) +
  labs(y = 'Taxa de Venda %',
       x = 'Data',
       title = 'Histórico da Taxa de Venda - NTN-B')

plot_NTNB_premium_trend <- ggplotly(plot_NTNB_premium_trend)

plot_NTNB_premium_trend

## Histórico da Taxa de Venda - NTN-B ----------------------------------------
plot_prefix_trend <- left_join(bond_data, bond_names, by = 'classe') %>%
  left_join(bond_status, by = 'titulo') %>%
  filter(classe == 'LTN' | classe == 'NTN-F',
         status == 'active') %>%
  ggplot(aes(x = dia)) +
  geom_line(aes(color = titulo, y = taxa_venda), size = 0.2) +
  geom_smooth(aes(y = taxa_compra), method = 'loess') +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"),
                     limits = c(0, 0.2)) +
  labs(y = 'Taxa de Venda %',
       x = 'Data',
       title = 'Histórico da Taxa de Venda - pré-fixado')

plot_prefix_trend <- ggplotly(plot_prefix_trend)

plot_prefix_trend

## Histórico da Taxa de Venda - LFT ----------------------------------------
plot_posfix_trend <- left_join(bond_data, bond_names, by = 'classe') %>%
  left_join(bond_status, by = 'titulo') %>%
  filter(classe == 'LFT',
         status == 'active') %>%
  ggplot(aes(x = dia)) +
  geom_line(aes(color = titulo, y = pu_venda), size = 0.2) +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0(x, " R$")) +
  labs(y = 'Taxa de Venda %',
       x = 'Data',
       title = 'Histórico da Preço de Venda - pós-fixado')

plot_posfix_trend <- ggplotly(plot_posfix_trend)

plot_posfix_trend

## Histórico da Selic + Inflação -------------------------------------------
plot_selic_inflation <- ggplot() +
  geom_line(data = selic_actual, aes(x = Date, y = Value, color = 'Selic Annual')) +
  geom_line(data = ipca_12m, aes(x = Date, y = Value, color = 'IPCA 12m')) +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_color_manual(values = c('Selic Annual' = 'darkblue', 'IPCA 12m' = 'darkred'),
                     name = NULL) +
  labs(y = '% Anualizado',
       x = 'Data',
       title = 'Inflação e Selic')

plot_selic_inflation <- ggplotly(plot_selic_inflation)

plot_selic_inflation
