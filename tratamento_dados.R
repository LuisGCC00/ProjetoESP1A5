#***Tratamento de dados ***#

#Leitura do csv contendo os dados da amostra.
dados <- read.csv("amostra.csv")

#Dataframe dos dados da amostra.
df <- data.frame(dados)


#Definindo novos nomes às colunas da amostra.
novos_nomes <- c(
  "DSEI", "PoloBase", "MunicipioIBGE", "Municipio", "UF", 
  "CodIndio", "Sexo", "DtAtendimento", "Peso", "Altura", 
  "Idade", "FaixaEtaria", "TipoAleitamento", "PesoIdade", 
  "EstaturaIdade", "IMCIdade"
)

#Atribuir os novos nomes às colunas
colnames(df) <- novos_nomes


#Conversão da variável Idade de character para numeric
df$Idade <- gsub(",", ".", df$Idade)
df$Idade <- as.numeric(df$Idade)

#Substituindo valores NA por "Sem Informação" na coluna TipoAleitamento.
df$TipoAleitamento <- ifelse(is.na(df$TipoAleitamento), "Sem Informação", df$TipoAleitamento)

#gravando os dados tratados em um arquivo csv.
write.csv(df, "amostra_van5_2022_tratado.csv", row.names = FALSE)
