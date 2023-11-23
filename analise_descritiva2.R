#Script com o estudo sobre a faixa etária e IMC
source("install_load_packages.R")
#Bibliotecas

file <- "amostra.csv"
data <- read.csv(file, stringsAsFactors = FALSE)
main_df <- data.frame(data)


#Verificando o tipo de dados de cada coluna da amostra
tipos_de_dados <- sapply(amostra, function(coluna) class(coluna))

print(tipos_de_dados)


# Função para encontrar dados nulos e acumular informações
encontrar_nulos <- function(coluna) {
  nulos <- sum(is.na(coluna))
  if (nulos > 0) {
    return(data.frame(Coluna = deparse(substitute(coluna)), Nulos = nulos))
  } else {
    return(NULL)
  }
}

# Aplicar a função a todas as colunas da amostra
resultados <- lapply(amostra, encontrar_nulos)

# Combinar os resultados em um data frame
resultados_df <- do.call(rbind, resultados)

print(resultados_df)


#Visão dos dados da coluna DS_IMC_IDADE
visao_ds_imc_idade <- unique(main_df$DS_IMC_IDADE)
cat(visao_ds_imc_idade, "\n")

### Contagem de valores na coluna DS_IMC_IDADE
qtd_total <- sum(!is.na(main_df$DS_IMC_IDADE))
qtd_eutrofia <- sum(main_df$DS_IMC_IDADE == "EUTROFIA")
qtd_risco_sobrepeso <- sum(main_df$DS_IMC_IDADE == "RISCO DE SOBREPESO")
qtd_sobrepeso <- sum(main_df$DS_IMC_IDADE == "SOBREPESO")
qtd_magreza <- sum(main_df$DS_IMC_IDADE == "MAGREZA")
qtd_obesidade <- sum(main_df$DS_IMC_IDADE == "OBESIDADE")
qtd_magreza_acentuada <- sum(main_df$DS_IMC_IDADE == "MAGREZA ACENTUADA")

# Exibir a quantidade de cada categoria
cat("Valor Total:", qtd_total, "\nQtd. Eutrofia:", qtd_eutrofia, "\nQtd. Risco Sobrepeso:", qtd_risco_sobrepeso,
    "\nQtd. Sobrepeso:", qtd_sobrepeso, "\nQtd. Magreza:", qtd_magreza,
    "\nQtd. Obesidade:", qtd_obesidade, "\nQtd. Magreza Acentuada:", qtd_magreza_acentuada, "\n")


#Visão dos Dados FAIXA_ETARIA
df_visao_colunas <- unique(main_df$FAIXA_ETARIA)
print(df_visao_colunas)

# Visão das Faixas Etárias
qtd_total_faixa_etaria <- sum(!is.na(main_df$FAIXA_ETARIA))

qtd_faixa_2_a_5_anos <- sum(main_df$FAIXA_ETARIA == "ENTRE 2 ANOS A 5 ANOS")
qtd_faixa_menor_6_meses <- sum(main_df$FAIXA_ETARIA == "MENOR DE 6 MESES")
qtd_entre_6_a_2_meses <- sum(main_df$FAIXA_ETARIA == "ENTRE 6 MESES A 2 ANOS")

cat("Faixa Etaria Total:", qtd_total_faixa_etaria, "\n")
cat("Quantidade Faixa 2 a 5 anos:", qtd_faixa_2_a_5_anos, "\n")
cat("Quantidade Faixa menor de 6 meses:", qtd_faixa_menor_6_meses, "\n")
cat("Quantidade Entre 6 meses a 2 anos:", qtd_entre_6_a_2_meses, "\n")

### Filtragens e Somas
# Sobrepeso
df_sobrepeso <- main_df[main_df$DS_IMC_IDADE == "SOBREPESO", ]
qtd_faixa_2_a_5_anos_sobrepeso <- sum(df_sobrepeso$FAIXA_ETARIA == "ENTRE 2 ANOS A 5 ANOS")

# Magreza
df_magreza <- main_df[main_df$DS_IMC_IDADE == "MAGREZA", ]
qtd_faixa_2_a_5_anos_magreza <- sum(df_magreza$FAIXA_ETARIA == "ENTRE 2 ANOS A 5 ANOS")

# Obesidade
df_obesidade <- main_df[main_df$DS_IMC_IDADE == "OBESIDADE", ]
qtd_faixa_2_a_5_anos_obesidade <- sum(df_obesidade$FAIXA_ETARIA == "ENTRE 2 ANOS A 5 ANOS")

# Magreza Acentuada
df_magreza_acentuada <- main_df[main_df$DS_IMC_IDADE == "MAGREZA ACENTUADA", ]
qtd_faixa_2_a_5_anos_magreza_acentuada <- sum(df_magreza_acentuada$FAIXA_ETARIA == "ENTRE 2 ANOS A 5 ANOS")

# Risco de Sobrepeso
df_risco_sobrepeso <- main_df[main_df$DS_IMC_IDADE == "RISCO DE SOBREPESO", ]
qtd_faixa_2_a_5_anos_risco_sobrepeso <- sum(df_risco_sobrepeso$FAIXA_ETARIA == "ENTRE 2 ANOS A 5 ANOS")

# Eutrofia
df_eutrofia <- main_df[main_df$DS_IMC_IDADE == "EUTROFIA", ]
qtd_faixa_2_a_5_anos_eutrofia <- sum(df_eutrofia$FAIXA_ETARIA == "ENTRE 2 ANOS A 5 ANOS")

cat("Valor Eutrofia:", qtd_faixa_2_a_5_anos_eutrofia,
    "\nValor de Risco de Sobrepeso:", qtd_faixa_2_a_5_anos_risco_sobrepeso,
    "\nValor de Sobrepeso:", qtd_faixa_2_a_5_anos_sobrepeso,
    "\nValor de Magreza:", qtd_faixa_2_a_5_anos_magreza,
    "\nValor de Obesidade:", qtd_faixa_2_a_5_anos_obesidade,
    "\nValor de Magreza Acentuada:", qtd_faixa_2_a_5_anos_magreza_acentuada
)

# Média de casos
# Média baseada em indivíduos de faixa etaria: 2 a 5 anos

# Eutrofia
media_eutrofia_2_5_anos <- qtd_faixa_2_a_5_anos_eutrofia / qtd_total

# Risco de sobrepeso
media_faixa_2_5_anos_risco_sobrepeso <- qtd_faixa_2_a_5_anos_risco_sobrepeso / qtd_total

# Sobrepeso
media_faixa_2_5_anos_sobrepeso <- qtd_faixa_2_a_5_anos_sobrepeso / qtd_total

# Magreza
media_faixa_2_5_anos_magreza <- qtd_faixa_2_a_5_anos_magreza / qtd_total

# Obesidade
media_faixa_2_5_anos_obesidade <- qtd_faixa_2_a_5_anos_obesidade / qtd_total

# Magreza Acentuada
media_faixa_2_5_anos_magreza_acentuada <- qtd_faixa_2_a_5_anos_magreza_acentuada / qtd_total

# Valor total
total_2_5_anos <- media_eutrofia_2_5_anos + media_faixa_2_5_anos_risco_sobrepeso + media_faixa_2_5_anos_sobrepeso + media_faixa_2_5_anos_magreza + media_faixa_2_5_anos_obesidade + media_faixa_2_5_anos_magreza_acentuada

cat("Saídas Esperadas:",
    "\n\nPC Eutrofia:", media_eutrofia_2_5_anos,
    "\nPC Risco de Sobrepeso:", media_faixa_2_5_anos_risco_sobrepeso,
    "\nPC Sobrepeso:", media_faixa_2_5_anos_sobrepeso,
    "\nPC Magreza:", media_faixa_2_5_anos_magreza,
    "\nPC Obesidade:", media_faixa_2_5_anos_obesidade,
    "\nPC Magreza Acentuada:", media_faixa_2_5_anos_magreza_acentuada,
    "\n\nValor Total:", total_2_5_anos, "\n")


### Teste de Qui-Quadro

# Dados 
observacoes <- c(3666, 1132, 358, 100, 156, 68)
total_com_risco_sobrepeso <- 9181

# Porcentagens calculadas com base nos valores fornecidos
proporcoes_esperadas <- c(0.3993, 0.1232, 0.0389, 0.0108, 0.0169, 0.0074)

# Calcula as expectativas sob a hipótese nula
expectativas <- proporcoes_esperadas * total_com_risco_sobrepeso

# Reorganiza os dados em uma tabela de contingência
tabela_contingencia <- matrix(observacoes, nrow = 2, byrow = TRUE)

# Realiza o teste qui-quadrado de independência
resultado_teste <- chisq.test(tabela_contingencia)

# Exibe o resultado
cat("Teste de Qui-Quadrado:", resultado_teste$statistic, "\n")
cat("Valor p:", resultado_teste$p.value, "\n")


### Porcentagens de casos
##### Porcentagens baseada em indivíduos de faixa etaria: 2 a 5 anos

# Eutrofia
media_eutrofia_2_5_anos <- (qtd_faixa_2_a_5_anos_eutrofia / qtd_total) * 100

# Risco de sobrepeso
media_faixa_2_5_anos_risco_sobrepeso <- (qtd_faixa_2_a_5_anos_risco_sobrepeso / qtd_total) * 100 

# Sobrepeso
media_faixa_2_5_anos_sobrepeso <- (qtd_faixa_2_a_5_anos_sobrepeso / qtd_total) * 100

# Magreza
media_faixa_2_5_anos_magreza <- (qtd_faixa_2_a_5_anos_magreza / qtd_total) * 100

# Obesidade
media_faixa_2_5_anos_obesidade <- (qtd_faixa_2_a_5_anos_obesidade / qtd_total) * 100

# Magreza Acentuada 
media_faixa_2_5_anos_magreza_acentuada <- (qtd_faixa_2_a_5_anos_magreza_acentuada / qtd_total) * 100

# Valor total
total_2_5_anos <- media_eutrofia_2_5_anos + media_faixa_2_5_anos_risco_sobrepeso + media_faixa_2_5_anos_sobrepeso + media_faixa_2_5_anos_magreza + media_faixa_2_5_anos_obesidade + media_faixa_2_5_anos_magreza_acentuada

cat("Média de IMC na faixa etária de 2 a 5 anos",
    "\n\nPC Eutrofia: ", sprintf("%.2f%%", media_eutrofia_2_5_anos),
    "\nPC Risco de Sobrepeso: ", sprintf("%.2f%%", media_faixa_2_5_anos_risco_sobrepeso),
    "\nPC Sobrepeso: ", sprintf("%.2f%%", media_faixa_2_5_anos_sobrepeso),
    "\nPC Magreza: ", sprintf("%.2f%%", media_faixa_2_5_anos_magreza),
    "\nPC Obesidade: ", sprintf("%.2f%%", media_faixa_2_5_anos_obesidade),
    "\nPC Magreza Acentuada: ", sprintf("%.2f%%", media_faixa_2_5_anos_magreza_acentuada),
    "\n\nValor Total: ", sprintf("%.2f%%", total_2_5_anos)
)


### Histograma com as médias

dados_histograma <- data.frame(
  Categoria = c("Eutrofia", "Risco de Sobrepeso", "Sobrepeso", "Magreza", "Obesidade", "Magreza Acentuada"),
  Percentual = c(
    media_eutrofia_2_5_anos,
    media_faixa_2_5_anos_risco_sobrepeso,
    media_faixa_2_5_anos_sobrepeso,
    media_faixa_2_5_anos_magreza,
    media_faixa_2_5_anos_obesidade,
    media_faixa_2_5_anos_magreza_acentuada
  )
)

#histograma
histograma <- ggplot(dados_histograma, aes(x = Categoria, y = Percentual, fill = Categoria)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Média de IMC na Faixa Etária de 2 a 5 anos", x = "Categoria", y = "Percentual") +
  theme_minimal()

print(histograma)

# Visão de dados da coluna: DS_PESO_IDADE
## Filtros utilizados na visão: 
## ['FAIXA_ETARIA'] == "ENTRE 2 ANOS A 5 ANOS" 
## ['DS_PESO_IDADE'] == 'PESO ADEQUADO PARA A IDADE'

# Filtrar o data frame para obter as observações desejadas
df_visao_estatura <- subset(main_df, FAIXA_ETARIA == "ENTRE 2 ANOS A 5 ANOS" & DS_PESO_IDADE == 'PESO ADEQUADO PARA A IDADE')

# Obter valores únicos da coluna 'DS_PESO_IDADE'
unique_values <- unique(df_visao_estatura$DS_PESO_IDADE)

# Imprimir os valores únicos
cat(unique_values, "\n")


### Porcentagem de indivíduos com: 
# Faixa etária entre 2 anos a 5 anos 
# Peso adequado para idade </p>
qtd_est_adequada <- ((sum(df_visao_estatura$DS_ESTATURA_IDADE == 'ESTATURA ADEQUADA PARA A IDADE') / qtd_total) * 100)
qtd_baixa_est <- ((sum(df_visao_estatura$DS_ESTATURA_IDADE == 'BAIXA ESTATURA PARA A IDADE') / qtd_total) * 100)
qtd_muito_baixa_est <- ((sum(df_visao_estatura$DS_ESTATURA_IDADE == 'MUITO BAIXA ESTATURA PARA A IDADE') / qtd_total) * 100)

cat("Total:", qtd_total, "\n")
cat("Estatura Adequada para a idade: ", round(qtd_est_adequada, 2), "%\n")
cat("Baixa Estatura para a idade: ", round(qtd_baixa_est, 2), "%\n")
cat("Muito Baixa Estatura para a idade: ", round(qtd_muito_baixa_est, 2), "%\n")

#### Visão de valores da coluna: NU_ALTURA
## Filtros utilizados na visão: 
# ['FAIXA_ETARIA'] == "ENTRE 2 ANOS A 5 ANOS"<br>
# ['DS_PESO_IDADE'] == 'PESO ADEQUADO PARA A IDADE'
df_ordenado <- df_visao_estatura[order(df_visao_estatura$NU_ALTURA),]
lista_altura <- unique(df_ordenado$NU_ALTURA)
cat(lista_altura)

### Visão de intervalo entre quartis
mediana_altura <- median(df_ordenado$NU_ALTURA)
cat("Mediana:", mediana_altura, "\n")

q1 <- quantile(df_ordenado$NU_ALTURA, 0.25)
q3 <- quantile(df_ordenado$NU_ALTURA, 0.75)
cat("Q1:", q1, "\nQ2:", q3, "\n")

iqr <- q3 - q1
cat("IQR:", iqr, "\n")

limite_inferior <- q1 - 1.5 * iqr
cat("Limite Inferior:", limite_inferior, "\n")
limite_superior <- q3 + 1.5 * iqr
cat("Limite Superior:", limite_superior, "\n")

#Outliers
outliers <- df_ordenado[(df_ordenado$NU_ALTURA < limite_inferior) | (df_ordenado$NU_ALTURA > limite_superior), c('NO_MUNICIPIO', 'TP_SEXO', 'NU_ALTURA')]
cat("\n\nOutliers:\n")
print(outliers)


#Boxplot
# Ordena o dataframe pela coluna NU_ALTURA em ordem crescente
df_ordenado <- df_visao_estatura[order(df_visao_estatura$NU_ALTURA),]

# Cria uma lista de alturas únicas
lista_altura <- unique(df_ordenado$NU_ALTURA)

# Cria o boxplot
ggplot(df_ordenado, aes(x = "", y = NU_ALTURA)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Boxplot da Altura", x = "", y = "Altura") +
  theme_bw()



