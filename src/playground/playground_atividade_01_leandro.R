# loading required libraries --------------------------------------------------

# libraries for matrix methods
#library(ggmcmc)
library(markovchain)
library(diagram)

# libraries for plot
library(plotly)
library(circlize)

# loading other scripts do be used here ---------------------------------------
source("./src/util/auxiliary_functions.R")

clearEnv()

# atividade individual 1 ------------------------------------------------------
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

# calculando a probabilidade
transitionProbability(dtmc_ecommerce, "Site", "Venda")
transitionProbability(dtmc_ecommerce, "Hotpage", "Venda")
transitionProbability(dtmc_ecommerce, "CallCenter", "Venda")

# estimando os estados sequentes no segundo passo
initialState <- c(0, 1, 0, 0, 0)
steps <- 2
finalState <- initialState * (dtmc_ecommerce ^ steps)
finalState

# obtendo a estabilidade final da matriz
steadyStates(dtmc_ecommerce)

# estimando o estado assintótico dado um estado inicial
initialState <- c(0.50, 0.20, 0.30, 0.00, 0.00)
steps <- 1000
finalState <- initialState * (dtmc_ecommerce ^ steps)
finalState

# plotando a matriz usando a biblioteca diagram -------------------------------
# exemplo completo em https://rpubs.com/JanpuHou/326048

plotmat(matriz_transicao)

plotmat(matriz_transicao,
        lwd = 1, 
        box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length = .1,
        arr.width = .1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Markov Chain para o e-commerce")

# plotando a matriz usando a biblioteca circlize ------------------------------
# https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html

# example 1
chordDiagram(matriz_transicao)
circos.clear()

# example 2
chordDiagram(matriz_transicao, order = c("Site", "Hotpage", "CallCenter", "Venda", "Saida"))
circos.clear()

# example 3
circos.par(gap.after = c(rep(5, nrow(matriz_transicao)-1), 15, rep(5, ncol(matriz_transicao)-1), 15))
chordDiagram(matriz_transicao)
circos.clear()

# example 4
circos.par(start.degree = 90, clock.wise = FALSE)
chordDiagram(matriz_transicao, big.gap = 5)
circos.clear()

# example 5
grid.col = c(Site = "red", Hotpage = "green", CallCenter = "blue", Venda = "grey", Saida = "grey")
chordDiagram(matriz_transicao, grid.col = grid.col)
chordDiagram(t(matriz_transicao), grid.col = grid.col)
circos.clear()

# example 6
par(mfrow = c(1, 3))
chordDiagram(matriz_transicao, grid.col = grid.col, directional = 1)
chordDiagram(matriz_transicao, grid.col = grid.col, directional = 1, diffHeight = uh(5, "mm"))
chordDiagram(matriz_transicao, grid.col = grid.col, directional = -1)
circos.clear()