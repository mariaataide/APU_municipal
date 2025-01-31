# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(fs)
library(vroom)
library(stringr)
library(stringi)
library(writexl)

# Banco de dados ----------------------------------------------------------

desp_mun <- 
  fs::dir_ls(
    path = "Despesa_municipal/",
    glob = "*.csv"
  ) |> 
  vroom::vroom(
    locale = vroom::locale(
      encoding = "latin1", 
      decimal_mark = ","
    ),
    delim = ";"
  ) |> 
  dplyr::mutate(Codigo_6_dig = stringr::str_sub(CodigoCompleto, 1, 6))

# Tradutores --------------------------------------------------------------

tradutor_VA <- 
  readxl::read_excel(
    "Tradutor_Finbra_CR.xlsx",
    sheet = "3ed_VA",
    range = "A3:B33",
    col_names = c("CR", "Codigo_Finbra")  
  ) |> 
  dplyr::relocate(Codigo_Finbra, .before = everything()) |> 
  dplyr::mutate(
    Codigo_Finbra = str_replace_all(Codigo_Finbra, "\\.", ""),
    Codigo_Finbra = as.double(Codigo_Finbra),
    Codigo_6_dig = stringr::str_sub(Codigo_Finbra, 1, 6)
  )


tradutor_CI <- 
  readxl::read_excel(
    "Tradutor_Finbra_CR.xlsx",
    sheet = "3ed_VA",
    range = "E3:F14",
    col_names = c("Codigo_Finbra", "CR"),
  ) |> 
  dplyr::mutate(
    Codigo_Finbra = str_replace_all(Codigo_Finbra, "\\.", ""),
    Codigo_Finbra = as.double(Codigo_Finbra),
    Codigo_6_dig = stringr::str_sub(Codigo_Finbra, 1, 6)
  )


# Calculo VA / CI ---------------------------------------------------------

gerar_base <- function(func_desp,
  estagio,
  trad,
  cod_desp = NULL,
  cod_trad = NULL,
  by = NULL) {
# Validar o argumento `by`
if (is.null(by)) {
if (is.null(cod_desp) || is.null(cod_trad)) {
stop("Você deve fornecer os argumentos `by` ou ambos `cod_desp` e `cod_trad`.")
}
by <- setNames(cod_trad, cod_desp) # Mapear colunas padrão se `by` não for fornecido
}

# Seleção e normalização da base de despesas
APU <- desp_mun |>
dplyr::select(
Ano,
CodigoCompleto,
Codigo_6_dig,
DescricaoElemento,
DescricaoFuncao,
Empenhada,
Liquidada
) |>
dplyr::mutate(
across(
where(is.character),
~ stringi::stri_trans_general(., id = "Latin-ASCII")
),
DescricaoElemento = stringr::str_replace_all(DescricaoElemento, "[^\\x00-\\x7F]", "")
) 

# Carregar códigos corretamente (pegando a primeira coluna dos arquivos)
codigos_VA <- readxl::read_excel("Verificação códigos APU.xlsx", sheet = "Códigos VA") |> 
dplyr::pull(1) # Extraindo a primeira coluna corretamente

codigos_CI <- readxl::read_excel("Verificação códigos APU.xlsx", sheet = "Códigos CI") |> 
dplyr::pull(1) 

# Filtro baseado no tipo de tradutor
APU <- if (identical(trad, tradutor_VA)) {
APU |> dplyr::filter(Codigo_6_dig %in% codigos_VA)
} else {
APU |> dplyr::filter(Codigo_6_dig %in% codigos_CI)
}

# Verificação dos valores válidos para `func_desp`
valores_validos <- c("APU", "EDUCACAO", "SAUDE")

if (!func_desp %in% valores_validos) {
stop("Valor inválido para func_desp. Escolha entre: 'APU', 'EDUCACAO', 'SAUDE'.")
}

# Converter `estagio` para símbolo
estagio <- dplyr::sym(estagio)

# Aplicar filtro condicional baseado na função de despesa
base_filtrada <- if (func_desp == "APU") {
APU
} else {
APU |> dplyr::filter(DescricaoFuncao == func_desp)
}

# Se a base estiver vazia após filtragem, retornar aviso
if (nrow(base_filtrada) == 0) {
warning("A base filtrada está vazia. Verifique os filtros aplicados.")
return(tibble::tibble(Ano = numeric(0), APU = numeric(0)))
}

# Criar a base final agregada por Ano
base <- base_filtrada |>
dplyr::group_by(Ano) |>  # Agrupar corretamente
dplyr::summarise(APU = sum(!!estagio, na.rm = TRUE), .groups = "drop") # Somar corretamente

return(base)
}


# Empenhada

VA_APU  <-  gerar_base(
  func_desp = "APU",
  estagio = "Empenhada",
  trad = tradutor_VA,
  by = c("Codigo_6_dig" = "Codigo_6_dig")
)

VA_EDUC  <-  gerar_base(
  func_desp = "EDUCACAO",
  estagio = "Empenhada",
  trad = tradutor_VA,
  by = c("Codigo_6_dig" = "Codigo_6_dig")
)

VA_SAUDE  <-  gerar_base(
  func_desp = "SAUDE",
  estagio = "Empenhada",
  trad = tradutor_VA,
  by = c("Codigo_6_dig" = "Codigo_6_dig")
)


CI_APU  <-  gerar_base(
  func_desp = "APU",
  estagio = "Empenhada",
  trad = tradutor_CI,
  by = c("Codigo_6_dig" = "Codigo_6_dig")
)

CI_EDUC  <-  gerar_base(
  func_desp = "EDUCACAO",
  estagio = "Empenhada",
  trad = tradutor_CI,
  by = c("Codigo_6_dig" = "Codigo_6_dig")
)

CI_SAUDE  <-  gerar_base(
  func_desp = "SAUDE",
  estagio = "Empenhada",
  trad = tradutor_CI,
  by = c("Codigo_6_dig" = "Codigo_6_dig")
)

  
VA <- plyr::join_all(list(VA_APU, VA_EDUC, VA_SAUDE), by = "Ano") |> 
  setNames(c("ANO", "APU", "EDUC", "SAUDE"))

CI <- plyr::join_all(list(CI_APU, CI_EDUC, CI_SAUDE), by = "Ano") |> 
  setNames(c("ANO", "APU", "EDUC", "SAUDE"))


# Liquidada

VA_APU_LIQ  <-  gerar_base(
  func_desp = "APU",
  estagio = "Liquidada",
  trad = tradutor_VA,
  by = c("CodigoCompleto" = "Codigo_Finbra")
)

VA_EDUC_LIQ  <-  gerar_base(
  func_desp = "EDUCACAO",
  estagio = "Liquidada",
  trad = tradutor_VA,
  by = c("CodigoCompleto" = "Codigo_Finbra")
)

VA_SAUDE_LIQ  <-  gerar_base(
  func_desp = "SAUDE",
  estagio = "Liquidada",
  trad = tradutor_VA,
  by = c("CodigoCompleto" = "Codigo_Finbra")
)

CI_APU_LIQ  <-  gerar_base(
  func_desp = "APU",
  estagio = "Liquidada",
  trad = tradutor_CI,
  by = c("Codigo_6_dig" = "Codigo_6_dig")
)

CI_EDUC_LIQ  <-  gerar_base(
  func_desp = "EDUCACAO",
  estagio = "Liquidada",
  trad = tradutor_CI,
  by = c("Codigo_6_dig" = "Codigo_6_dig")
)

CI_SAUDE_LIQ  <-  gerar_base(
  func_desp = "SAUDE",
  estagio = "Liquidada",
  trad = tradutor_CI,
  by = c("Codigo_6_dig" = "Codigo_6_dig")
)


VA_LIQ <- plyr::join_all(list(VA_APU, VA_EDUC, VA_SAUDE), by = "Ano") |> 
  setNames(c("ANO", "APU", "EDUC", "SAUDE")) 

CI_LIQ <- plyr::join_all(list(CI_APU, CI_EDUC, CI_SAUDE), by = "Ano") |> 
  setNames(c("ANO", "APU", "EDUC", "SAUDE"))

# Salva .xlsx -------------------------------------------------------------

writexl::write_xlsx(
  x = list(VA = VA, CI = CI), 
  path = "APU_municipal_empenhada.xlsx"
)

writexl::write_xlsx(
  x = list(VA = VA_LIQ, CI = CI_LIQ), 
  path = "APU_municipal_liquidada.xlsx"
)