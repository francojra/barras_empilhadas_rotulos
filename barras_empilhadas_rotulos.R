
# Gráfico de barras empilhadas com rótulos -------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 03/11/23 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(scales)

# Criar dados ------------------------------------------------------------------------------------------------------------------------------

# Criar um vetor de classes com repetição
classes <- rep(1:3, each = 15)

# Criar um vetor de sobreviventes com valores aleatórios 'sim' e 'não'
sobreviventes <- sample(c('sim', 'não'), length(classes), replace = TRUE)

# Criar um data frame com as duas colunas
data <- data.frame(classe = classes, sobreviventes = sobreviventes)

# Visualizar as primeiras linhas da tabela
view(data)

# Gráfico ----------------------------------------------------------------------------------------------------------------------------------

data %>% 
  group_by(classe, sobreviventes) %>% 
  count() %>% 
  
ggplot(., aes(x = classe, y = n/sum(n)*100, fill = sobreviventes)) + 
  geom_bar(stat = "identity", position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(breaks = c("não", "sim"),
                       labels = c("Não", "Sim")) + 
  geom_text(aes(label = percent(n/sum(n)*3)),
            position = position_fill(vjust = 0.5),
            size = 4, fontface = "bold") +
  labs(y = "Frequência (%)", x = "Classe da passagem", 
       fill = "Sobreviveu \nao desastre?") +
  theme_bw() +
  theme(text = element_text(size = 15, color = "black"),
        axis.text = element_text(color = "black", size = 13))
