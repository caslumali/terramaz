#!/usr/bin/env Rscript

# Script para renderizar o relatório com o idioma e territórios especificados
# Uso:
#   Rscript render.R fr
#   Rscript render.R pt
#   Rscript render.R es
#   Rscript render.R en
# Ou simplesmente: source("scripts/quarto/render.R") no R console

# Captura argumentos da linha de comando
args <- commandArgs(trailingOnly = TRUE)

# Define idioma padrão como francês se não especificado
lang <- if (length(args) > 0) args[1] else "fr"
lang <- "pt"

# Valida o idioma
valid_langs <- c("fr", "pt", "es", "en")
if (!lang %in% valid_langs) {
  stop(paste("Idioma inválido:", lang, "\nUse um dos seguintes:", paste(valid_langs, collapse = ", ")))
}

# Territórios padrão
territories <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")

# Determina o diretório do script e o diretório raiz do projeto
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
if (script_dir == "") {
  # Se não conseguir pegar do RStudio, usa commandArgs
  script_dir <- getSrcDirectory(function(x) {x})
}
if (script_dir == "") {
  script_dir <- getwd()
}

# Procura o report.qmd
possible_paths <- c(
  file.path(script_dir, "report.qmd"),
  file.path(dirname(script_dir), "report.qmd"),
  file.path(dirname(dirname(script_dir)), "report.qmd"),
  "report.qmd"
)

report_path <- NULL
for (path in possible_paths) {
  if (file.exists(path)) {
    report_path <- normalizePath(path, winslash = "/")
    break
  }
}

if (is.null(report_path)) {
  stop("Não foi possível encontrar report.qmd. Por favor, execute o script da raiz do projeto ou ajuste os caminhos.")
}

# Mensagem informativa
message("=", paste(rep("=", 50), collapse = ""))
message(paste("Renderizando relatório em", toupper(lang)))
message(paste("Arquivo:", report_path))
message(paste("Territórios:", paste(territories, collapse = ", ")))
message("=", paste(rep("=", 50), collapse = ""))

# Renderiza o documento usando o profile correspondente
tryCatch({
  quarto::quarto_render(
    input = report_path,
    execute_params = list(
      lang = lang,
      territories = territories
    ),
    profile = lang
  )
  
  message("\n✓ Renderização concluída com sucesso!")
  message(paste("Arquivo gerado em: docx/report.docx"))
  
}, error = function(e) {
  message("\n✗ Erro durante a renderização:")
  message(e$message)
  quit(status = 1)
})