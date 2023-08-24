# tipos de dados e operações
mult = 3 * 7
soma = 1 + 1
div <- mult / soma
m <- "masculino"
f <- "feminino"

# vetores
idade <- c(20, 26, 20, 25, 23)
sexo <- c(m, m, f, f, m)

# média
mean(idade)
mean(sexo)

# tabela
dados <- data.frame(idade, sexo)

# remover variável
rm(idade, sexo)

# coluna da tabela
mean(dados$idade)

dados1 <- dados[,2]
dados2 <- dados[-2,]
dados3 <- dados[c(1, nrow()),]
dados4 <- dados[dados$sexo==m,]
