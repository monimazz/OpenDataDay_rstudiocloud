#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PEGANDO DADOS DA PNAD CONTÍNUA - Microdados por pesquisa anual ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(PNADcIBGE)

variaveis <- c("UF",
               "V1027",
               "V1028",
               "V2007",
               "V2009",
               "V2010",
               "V4001",
               "V4005",
               "V4071",
               "V4073",
               "V4074A")

# == Necessário indicar o trimestre ==
dadosPNADc_2020_3tri <- get_pnadc(year = 2020, quarter = 3, vars = variaveis, labels = TRUE) 

# == Colocando as variáveis em uma tabela ==
pnad_2020_3tri <- dadosPNADc_2020_3tri$variables

# == Arrumando o nome das colunas ===
colnames(pnad_2020_3tri) <- c("Ano","Trimestre", "UF", "UPA", "Estrato", "num.domicilio", "peso.spos", "peso.cpos",
                              "projecao.pop", "posest", "num.ordem", "sexo", "idade", "cor", "trabalhou", "afastado",
                              "procurou.trabalho", "queria.trabalhar", "pq.n.procurou", "habitual", "efetivo")

# == retirar pessoas com menos de 14 anos ===
pnadc_2020tri_filtro <- pnad_2020_3tri %>% 
  filter(idade >= 14)


# == Categorizar os dados em Jovem, Adulto e Idoso e faixas----
pnadc_categorias <- pnadc_2020tri_filtro %>% 
  mutate(idade.categoria = case_when(idade < 30 ~ "Jovem",
                                     idade < 60 ~ "Adulto",
                                     idade > 59 ~ "Idoso",
                                     TRUE ~ "n/a"),
         faixa.idade = case_when(idade < 18 ~ "14 a 17 anos",
                                 idade < 25 ~ "18 a 24 anos",
                                 idade < 30 ~ "25 a 30 anos",
                                 idade < 40 ~ "30 a 39 anos",
                                 idade < 60 ~ "40 a 59 anos",
                                 idade >= 60 ~ "60 anos ou +"
         ))



# == Categorizarpor termos tecnicos ----
pnadc_categorias <- pnadc_categorias %>% 
  mutate(forcadetrabalho = case_when(trabalhou == "Sim" | afastado == "Sim" ~ "Ocupada",
                                     trabalhou == "Não" & procurou.trabalho == "Sim" ~ "Desocupada",
                                     trabalhou == "Não" & queria.trabalhar == "Sim" ~"Desalentada",
                                     trabalhou == "Não" & queria.trabalhar == "Não" ~"Não quer trabalhar",
                                     TRUE ~ "Não trabalhou"))

# Graficos -----
library(ggplot2)

#* ocupada,desocupada,desalento - faixa etaria ou jovem
pnadc_categorias %>% 
  ggplot(aes(x = forcadetrabalho, fill = idade.categoria)) +
  geom_bar() +
  coord_flip()


#* Zoom situacao de força de trabalho dos jovens
pnadc_categorias %>% 
  filter(idade.categoria == "Jovem") %>% 
  ggplot(aes(x = forcadetrabalho, fill = faixa.idade)) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(x = "", y = "contagem", title = "")


#* perfil dos desalentados - cor e genero
pnadc_categorias %>% 
  filter(idade.categoria == "Jovem",
         forcadetrabalho == "Desalentada") %>% 
  ggplot(aes(x = sexo, fill = cor)) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(x = "Sexo", y = "contagem", title = "Perfil do jovem desalentado")

#* motivo da não procura de trabalho pelos jovens.
pnadc_categorias %>% 
  filter(idade.categoria == "Jovem",
         pq.n.procurou != "n/a") %>% 
  ggplot(aes(x = pq.n.procurou, fill = cor)) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(x = "Sexo", y = "contagem", title = "Perfil do jovem desalentado")

library(forcats)
pnadc_categorias %>% 
  filter(idade.categoria == "Jovem",
         pq.n.procurou != "n/a") %>% 
  count(pq.n.procurou, cor, sort = TRUE) %>% 
  mutate(pq.n.procurou = fct_reorder(pq.n.procurou, n)) %>% 
  ggplot(aes(x = pq.n.procurou, y = n, fill = cor)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(x = "Motivo", y = "contagem", title = "Porque não procurou emprego? - Jovem")


# === fazendo mapas ===
library(geobr)

mapadado <- pnadc_categorias %>% 
  filter(forcadetrabalho == "Desalentada", 
         idade.categoria == "Jovem") %>% 
  group_by(UF) %>% 
  summarize(n = n(),
            n100mil = n()/100000)

states <- read_state(year=2019)

mapa_desalento <- left_join(states, mapadado, by = c("name_state"="UF"))

mapa_desalento %>% 
  ggplot() +
  geom_sf(aes(fill = n100mil)) +
  theme_void()


