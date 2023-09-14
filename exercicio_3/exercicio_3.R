library(data.table)
library(dplyr)

base <- fread(input = paste0('mobile.csv'), header = T, na.strings = "NA", data.table = FALSE, dec = '.')
base$touch_screen <- recode(base$touch_screen, `0`='0:NAO', `1`='1:SIM')
base$blue <- recode(base$blue, `0`='0:NAO', `1`='1:SIM')

amostra <- base[sample(nrow(base), size = 600),]

calc.valores <- function(variavel) {
  media <- mean(variavel)
  desvio.padrao <- sd(variavel)
  coef.var <- (desvio.padrao * 100) / media
  resultados <- c(
    paste('Média:', media),
    paste('Desvio Padrão:', desvio.padrao),
    paste('Coeficiente de Variação:', coef.var)
  )
  resultados
}

# Cálculo da média,  desvio padrão e coeficiente de variação para 'battery_power'
by(amostra$battery_power, amostra$touch_screen, calc.valores)

# Cálculo da média,  desvio padrão e coeficiente de variação para 'm_dep'
by(amostra$m_dep, amostra$touch_screen, calc.valores)

# Cálculo da média,  desvio padrão e coeficiente de variação para 'int_memory'
by(amostra$int_memory, amostra$blue, calc.valores)

# Boxplot para 'battery_power'
boxplot(
  battery_power~touch_screen,
  data = amostra,
  main = 'Comparação da energia da bateria com a presença de touch screen',
  xlab = 'Com touch screen',
  ylab = 'Energia da bateria',
  col = 'darkgreen'
)

# Boxplot para 'm_dep'
boxplot(
  m_dep~touch_screen,
  data = amostra,
  main = 'Comparação da profundidade da tela com a presença de touch screen',
  xlab = 'Com touch screen',
  ylab = 'Profundidade da tela',
  col = 'lightblue'
)

# Boxplot para 'int_memory'
boxplot(
  int_memory~blue,
  data = amostra,
  main = 'Comparação da memória interna com a presença de bluetooth',
  xlab = 'Com bluetooth',
  ylab = 'Memória interna',
  col = 'purple'
)
