# ler base de dados
library(data.table)
dados_salarios <- fread(input = paste0('dados_salarios.csv'), header = T, na.strings = "NA", data.table = FALSE, dec = ",")

dados_salarios$trab_remoto = as.character(dados_salarios$trab_remoto)

# amostragem
library(samplingbook)

sample.size.prop(e = 0.01, P = 0.5, N = Inf, level = 0.95)
sample.size.mean(e = 10000, S = 71000, N = 607, level = 0.95)

asa147 <- dados_salarios[sample(nrow(dados_salarios), size = 147),]
mean(asa147$salario_USD)
mean(dados_salarios$salario_USD)

# subconjuntos
dados_2020 <- dados_salarios[dados_salarios$ano == 2020,]
dados_ft <- dados_salarios[dados_salarios$emprego == 'FT',]