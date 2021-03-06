#Instalar os pacotes
#install.packages("PNADcIBGE")
#install.packages("plotly")
#install.packages("tidyverse")


library(readr)

library(readr)
pnad_2020_3tri <- read_rds("PNADCONTINUA_20203TRI.rds")


# == Arrumando o nome das colunas ===
colnames(pnad_2020_3tri) <- c("Ano","Trimestre", "UF", "UPA", "Estrato", "num.domicilio","peso.cpos",
                              "projecao.pop", "posest", "num.ordem", "sexo", "idade", "cor", "motivo.n.procura",
                              "nivel.instrucao", "forca.trabalho","Ocupacao","forca.potencial",
                              "subocupacao","desalento",
                              "habitual", "efetivo")

# == retirar pessoas com menos de 14 anos ===
library(dplyr)

pnadc_2020tri_filtro <- pnad_2020_3tri %>% 
  filter(idade >= 14)


# == Categorizar os dados em Jovem, Adulto e Idoso e faixas----
pnadc_categorias <- pnadc_2020tri_filtro %>% 
  mutate(  faixa.idade = case_when(idade < 18 ~ "14 a 17 anos",
                                 idade < 30 ~ "18 a 29 anos",
                                 idade < 40 ~ "30 a 39 anos",
                                 idade < 60 ~ "40 a 59 anos",
                                 idade >= 60 ~ "60 anos ou +"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#===== Visualização ========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Graficos -----
library(ggplot2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#===== Gráfico 1 ========

pnadc_categorias %>% 
  filter(Ocupacao != "NA") %>% 
  ggplot(aes(x = faixa.idade, fill = Ocupacao)) +
  geom_bar(position = "fill")


# MELHORANDO A VISUALIZAÇÃO - Camadas do ggplot
pnadc_categorias %>% 
  filter(Ocupacao != "NA") %>% 
  ggplot(aes(x = faixa.idade, fill = Ocupacao)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = "Situação de ocupação por faixa etária", fill = "Situação",
       x = "Faixa etária", y = "Porcentagem por grupo")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ===== Gráfico 2 ========
pnadc_categorias %>% 
  filter(desalento != "NA") %>% 
  ggplot(aes(x = faixa.idade, y = ..count../sum(..count..), fill = cor)) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = "Distribuição percentual do desalento por faixa etária", fill = "Cor",
       x = "Faixa etária", y = "Porcentagem")


# Melhorando a visualização
library(forcats)

pnadc_categorias %>% 
  filter(desalento != "NA") %>% 
  count(faixa.idade, cor, sort = TRUE) %>%
  mutate(faixa.idade = fct_reorder(faixa.idade, n)) %>% 
  ggplot(aes(x = faixa.idade, y = n/sum(n) , fill = cor)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = "Distribuição percentual do desalento por faixa etária", fill = "Cor",
       x = "Faixa etária", y = "Porcentagem")

#Deixando interativo!
p <- pnadc_categorias %>% 
  filter(desalento != "NA") %>% 
  count(faixa.idade, cor, sort = TRUE) %>%
  mutate(faixa.idade = fct_reorder(faixa.idade, n)) %>% 
  ggplot(aes(x = faixa.idade, y = n/sum(n) , fill = cor)) +
  geom_bar(aes(text = paste0(round(n/sum(n), digits = 2), "%",
                              "<br>Cor: ", cor)), stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = "Distribuição percentual do desalento por faixa etária", fill = "Cor",
       x = "Faixa etária", y = "Porcentagem")


library(plotly)
ggplotly(p, tooltip = "text") 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ======== Gráfico 3 ========
pnadc_categorias %>% 
  filter(idade <= 29,
         motivo.n.procura != "NA") %>% 
  count(motivo.n.procura, cor, sort = TRUE) %>%
  ggplot(aes(x = motivo.n.procura, y = n/sum(n), fill = cor)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = "Motivo da não busca por emprego",subtitle = "Entre os jovens",  fill = "Cor",
       x = "Faixa etária", y = "Porcentagem")
