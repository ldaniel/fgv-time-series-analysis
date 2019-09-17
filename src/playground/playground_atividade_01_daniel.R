# loading required libraries --------------------------------------------------

# libraries for matrix methods
#library(ggmcmc)
library(markovchain)

# libraries for plot
library(plotly)


# loading other scripts do be used here ---------------------------------------
source("./src/util/auxiliary_functions.R")

clearEnv()

# Atividade individual 1 ------------------------------------------------------
# criando a matriz de transição do e-commerce
matriz_transicao <- matrix(c(0.00, 0.15, 0.20, 0.00, 0.00,
                             0.10, 0.00, 0.10, 0.00, 0.00,
                             0.10, 0.05, 0.00, 0.00, 0.00,
                             0.20, 0.35, 0.13, 1.00, 0.00,
                             0.60, 0.45, 0.57, 0.00, 1.00), 
                           5, 5)

# este passo não é necessário quando se usa a library markovchain
rownames(matriz_transicao) <- c("Site", "Hotpage", "CallCenter", "Venda", "Saida")
colnames(matriz_transicao) <- c("Site", "Hotpage", "CallCenter", "Venda", "Saida")

matriz_transicao

# criando a DTMC (Discrete Time Markov Chain)
dtmc_ecommerce <- new("markovchain", 
                      transitionMatrix = matriz_transicao,
                      states = c("Site", "Hotpage", "CallCenter", "Venda", "Saida"),
                      name = "Markov Chain para o e-commerce") 
dtmc_ecommerce
plot(dtmc_ecommerce, edge.arrow.size = 0.5)

# Item A: calculando as probabilidades
# Pela matriz de transição, coletando um a um os valores
transitionProbability(dtmc_ecommerce, "Hotpage", "Site")
transitionProbability(dtmc_ecommerce, "Hotpage", "Hotpage")
transitionProbability(dtmc_ecommerce, "Hotpage", "CallCenter")
transitionProbability(dtmc_ecommerce, "Hotpage", "Venda")
transitionProbability(dtmc_ecommerce, "Hotpage", "Saida")

# Simulando um usuário com início na hotpage de passo 1 (para o item A.1)
initialState <- c(0, 1, 0, 0, 0)
steps <- 1
finalState <- initialState * (dtmc_ecommerce ^ steps)

cat(paste0("\nProbabilidade de iniciar pela hotpage e estar em:\n"))
print(finalState)

# Simulando um usuário com início na hotpage de passo 2 (para o item A.2)
initialState <- c(0, 1, 0, 0, 0)
steps <- 2
finalState <- initialState * (dtmc_ecommerce ^ steps)

cat(paste0("\nProbabilidade de iniciar pela hotpage e, após ", steps, " interações, estar em:\n"))
print(finalState)

# Simulando um usuário com início no site de passo 10000 (para o item B.1), objetivando a probabilidade de venda do canal
initialState <- c(1, 0, 0, 0, 0)
steps <- 10000
finalState <- initialState * (dtmc_ecommerce ^ steps)

cat(paste0("\nProbabilidade de iniciar pelo site e, após muitas interações (estabilidade), gerar uma venda: ", finalState[1,"Venda"]))

# Simulando um usuário com início na hotpage de passo 10000 (para o item B.2), objetivando a probabilidade de venda do canal
initialState <- c(0, 1, 0, 0, 0)
steps <- 10000
finalState <- initialState * (dtmc_ecommerce ^ steps)

cat(paste0("\nProbabilidade de iniciar pela hotpage e, após muitas interações (estabilidade), gerar uma venda: ", finalState[1,"Venda"]))

# Simulando um usuário com início no call center de passo 10000 (para o item B.3), objetivando a probabilidade de venda do canal
initialState <- c(0, 0, 1, 0, 0)
steps <- 10000
finalState <- initialState * (dtmc_ecommerce ^ steps)

cat(paste0("\nProbabilidade de iniciar pelo call center e, após muitas interações (estabilidade), gerar uma venda: ", finalState[1,"Venda"]))

# Simulando um pool de usuários distribuídos conforme vetor de entrada a seguir, por 10000 passos (para o item D), objetivando a condição final dos usuários
initialState <- c(0.50, 0.20, 0.30, 0.00, 0.00)
steps <- 10000
finalState <- initialState * (dtmc_ecommerce ^ steps)

cat(paste0("\n\nProbabilidade de conversão assintótica final:\n"))
print(finalState)
