# inst/scripts/update_clones.R

# 1. Configurar autenticação (o token vem do ambiente do Actions)
# O pacote gh usa GITHUB_PAT por padrão.
# No Actions, definiremos essa variável com o token automático.

# 2. Buscar dados dos últimos 14 dias
library(gh)
repo <- "hydroversebr/hydrobr" # Substitua pelo seu repositório

owner <- strsplit(repo, "/")[[1]][1]
nome  <- strsplit(repo, "/")[[1]][2]

# 1. Buscar dados dos últimos 14 dias
dados <- tryCatch(
  gh("GET /repos/{owner}/{repo}/traffic/clones",
     owner = owner, repo = nome)$clones,
  error = function(e) { message("Erro: ", e$message); NULL }
)

if (is.null(dados) || length(dados) == 0) quit(status = 0)

# 2. Converter para data.frame e agregar por dia (soma count e uniques)
df_novos <- do.call(rbind, lapply(dados, function(x) {
  data.frame(timestamp = as.Date(x$timestamp),
             count     = as.integer(x$count),
             uniques   = as.integer(x$uniques))
}))

df_novos <- aggregate(cbind(count, uniques) ~ timestamp, data = df_novos, FUN = sum)

# 3. Ler histórico existente
caminho <- "inst/extdata/clones_history.csv"
if (file.exists(caminho)) {
  df_hist <- read.csv(caminho)
  df_hist$timestamp <- as.Date(df_hist$timestamp)
} else {
  df_hist <- data.frame(timestamp = as.Date(character()),
                        count = integer(), uniques = integer())
}

# 4. Adicionar APENAS datas que ainda não existem no histórico
datas_novas <- setdiff(df_novos$timestamp, df_hist$timestamp)
df_append   <- df_novos[df_novos$timestamp %in% datas_novas, ]

df_final <- rbind(df_hist, df_append)
df_final <- df_final[order(df_final$timestamp), ]

# 5. Salvar
dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
write.csv(df_final, caminho, row.names = FALSE)

message("Adicionados ", nrow(df_append), " dia(s). Total: ", nrow(df_final))

# Adicione no final do seu script update_clones.R

# ... (código anterior para ler/escrever o CSV) ...

# Calcular a soma total de unique clones
total_uniques <- sum(df_final$uniques, na.rm = TRUE)

# Gerar dados no formato Shields.io
# A estrutura é: { "schemaVersion": 1, "label": "unique clones", "message": "123", "color": "blue" }
badge_data <- sprintf(
  '{"schemaVersion": 1, "label": "unique clones", "message": "%s", "color": "blue"}',
  format(total_uniques, big.mark = ",", scientific = FALSE)
)

# Gravar o arquivo (recomenda-se colocá-lo em um local acessível publicamente, como o diretório raiz ou inst/extdata)
writeLines(badge_data, "inst/extdata/clones_badge.json")
