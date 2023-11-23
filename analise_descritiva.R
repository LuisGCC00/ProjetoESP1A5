#Instalação/Carregamento das libs necessárias para a execução do script.
source("install_load_packages.R")

#Leitura do csv contendo os dados da amostra.
dados <- read.csv("amostra_van5_2022_tratado.csv")

#Criação de um dataframe com o conjunto de dados da amostra.
df <- data.frame(dados)

#Numero de registros da amostra.
numero_registros <- nrow(df)
print(numero_registros)

#Exibir nome da coluna e valor para a primeira linha.
for (coluna in colnames(df)) {
  cat(coluna, ": ", df[1, coluna], "\n")
}

# Exibir o tipo de cada variável.
for (coluna in colnames(df)) {
  cat("Tipo da variável", coluna, ":", class(df[[coluna]]), "\n")
}

#Criar gráfico de barras para distribuição da faixa etária.
barplot(table(df$FaixaEtaria), main="Distribuição da Faixa Etária", xlab="Faixa Etária", ylab="Frequência na amostra", col="grey", border="black")

# Criar um gráfico de barras para distribuição do imc por idade.
barplot(table(df$IMCIdade), main="Distribuição do IMC por Idade", xlab="IMC por Idade", ylab="Frequência na amostra", col="lightyellow", border="black")

#Filtrar os dados da coluna Faixa Etária.
df_faixa_etaria <- df[df$FaixaEtaria == "ENTRE 2 ANOS A 5 ANOS", ]

# Criar o gráfico de barras para distribuição de imc por idade para a faixa etária predominante na amostra.
ggplot(df_faixa_etaria, aes(x = IMCIdade)) +
  geom_bar(stat = "count", fill = "lightyellow", color = "black") +
  labs(title = paste("Distribuição do IMC por Idade - Faixa Etária:", df$FaixaEtaria),
       x = "IMC por Idade",
       y = "Frequência na amostra")

#Criar o grafico de barras para distribuição do tipo de aleitamento para a faixa etária predominante na amostra.
ggplot(df_faixa_etaria, aes(x = TipoAleitamento)) +
  geom_bar(stat = "count", fill = "brown", color = "black") +
  labs(title = paste("Distribuição do Tipo de Aleitamento - Faixa Etária:", df$FaixaEtaria),
       x = "Tipo de Aleitamento",
       y = "Frequência na amostra")

#criar visualização da relação entre faixa etária e tipo de aleitamento.
ggplot(df, aes(x = FaixaEtaria, fill = TipoAleitamento)) +
  geom_bar(position = "stack", stat = "count") +
  labs(title = "Relação entre Faixa Etária e Tipo de Aleitamento", x = "Faixa Etária", y = "Contagem")


# Filtrando o dataframe para a faixa etária de 2 a 5 anos
dados_filtrados <- df[df$FaixaEtaria == "ENTRE 2 ANOS A 5 ANOS", ]


dados_filtrados$IMCIdade<- dados_filtrados$Peso / (dados_filtrados$Altura/100)^2

# Criando o gráfico de dispersão com jitter
ggplot(dados_filtrados, aes(x = TipoAleitamento, y = IMCIdade, color = Idade)) +
  geom_jitter(width = 0.2) + # O valor dentro de width controla a quantidade de jitter
  theme_minimal() +
  labs(title = "Relação entre Tipo de Aleitamento, IMC e Idade (2 a 5 anos)",
       x = "Tipo de Aleitamento",
       y = "Índice de Massa Corporal (IMC)",
       color = "Idade (Anos)") +
  scale_color_gradient(low = "blue", high = "red")

# Calculando e imprimindo as frequências dos tipos de aleitamento
frequencias <- table(dados_filtrados$TipoAleitamento)
print(frequencias)

ggplot(dados_filtrados, aes(x = TipoAleitamento, y = IMCIdade)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relação entre IMC e Tipo de Aleitamento na Faixa Etária de 2 a 5 Anos",
       x = "Tipo de Aleitamento",
       y = "Índice de Massa Corporal (IMC)")


