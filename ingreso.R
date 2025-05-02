# Para instalar los paquetes en caso de no tenerlos
# install.packages(c("haven", "dplyr", "tidyr", "purrr", "rio"))

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(rio)

data = read_dta("ruta al archivo.dta")

agregado_ingreso = function(data) {
data <- data %>% 
  mutate(
    p76 = if_else(!is.na(p75) & p75 == 1, p76, NA_real_),
    p63 = ifelse((p63==999 | p63==9999 | p63==99999 | p63==999999), 999999, p63),
    p64b = ifelse((p64b==999 | p64b==9999 | p64b==99999 | p64b==999999), 999999, p64b),
    p65 = ifelse((p65==999 | p65==9999 | p65==99999 | p65==999999), 999999, p65),
    p66 = ifelse((p66==999 | p66==9999 | p66==99999 | p66==999999), 999999, p66),
    p67 = ifelse((p67==999 | p67==9999 | p67==99999 | p67==999999), 999999, p67),
    p68b = ifelse((p68b==999 | p68b==9999 | p68b==99999 | p68b==999999), 999999, p68b),
    p69 = ifelse((p69==999 | p69==9999 | p69==99999 | p69==999999), 999999, p69),
    p70b = ifelse((p70b==999 | p70b==9999 | p70b==99999 | p70b==999999), 999999, p70b),
    p71b = ifelse((p71b==999 | p71b==9999 | p71b==99999 | p71b==999999), 999999, p71b),
    p72b = ifelse((p72b==999 | p72b==9999 | p72b==99999 | p72b==999999), 999999, p72b),
    p73b = ifelse((p73b==999 | p73b==9999 | p73b==99999 | p73b==999999), 999999, p73b),
    p74b = ifelse((p74b==999 | p74b==9999 | p74b==99999 | p74b==999999), 999999, p74b),
    p76 = ifelse((p76==999 | p76==9999 | p76==99999 | p76==999999), 999999, p76),
    # Ingreso laboral primario
    ingr = 0,
    ingr = if_else(!is.na(p63) & p63 < 999999, (ingr + p63), ingr),
    ingr = if_else(!is.na(p64b) & p64b < 999999, (ingr + p64b), ingr),
    ingr = if_else(!is.na(p65) & p65 < 999999, (ingr - p65), ingr),
    ingr = if_else(!is.na(p66) & p66 < 999999, ingr + p66, ingr),
    ingr = if_else(!is.na(p67) & p67 < 999999, ingr + p67, ingr),
    ingr = if_else(!is.na(p68b) & p68b < 999999, ingr + p68b, ingr),
    ingr = if_else(is.na(p63) & is.na(p64b) & is.na(p65) & is.na(p66) & is.na(p67) & is.na(p68b), NA_real_, ingr),
    ingr = case_when(
      p63 == 999999 ~ 999999,
      p66 == 999999 ~ 999999, TRUE ~ ingr
    ),
    # Ingreso laboral secundario
    ingrls = 0,
    ingrls = if_else(!is.na(p69) & p69 < 999999, ingrls + p69, ingrls),
    ingrls = if_else(!is.na(p70b) & p70b < 999999, ingrls + p70b, ingrls),
    ingrls = if_else(is.na(p69) & is.na(p70b), NA_real_, ingrls),
    ingrls = case_when(p69 == 999999 & p70b == 999999 ~ 999999, T ~ ingrls),
    ingrls = if_else((is.na(p69) & p70b == 999999), 999999, ingrls),
    ingrls = if_else((p69 == 999999 & is.na(p70b)), NA_real_, ingrls),
    # ingreso Laboral
    ingrl = 0,
    ingrl = if_else((!is.na(ingr) & ingr < 0 & !is.na(ingrls) & ingrls < 999999), (ingrl + ingrls), ingrl),
    ingrl = if_else((((!is.na(ingr) & ingr < 999999 & ingr > 0) | (!is.na(ingr) & ingr == 0)) &
      (!is.na(ingrls) & ingrls < 999999)), (ingrl + ingr + ingrls), ingrl),
    ingrl = if_else((is.na(ingr) & !is.na(ingrls) & ingrls < 999999), (ingrl + ingrls), ingrl),
    ingrl = if_else((is.na(ingrls) & (!is.na(ingr) & ingr < 999999 & ingr > 0)), (ingrl + ingr), ingrl),
    ingrl = if_else((!is.na(ingr) & ingr < 0 & is.na(ingrls)), -1, ingrl),
    ingrl = if_else((!is.na(ingr) & ingr == 999999 & is.na(ingrls)), 999999, ingrl),
    ingrl = if_else((!is.na(ingr) & ingr == 999999 & (!is.na(ingrls) & ingrls < 999999 & ingrls >= 0)), 999999, ingrl),
    ingrl = if_else((!is.na(ingr) & ingr == 999999 & !is.na(ingrls) & ingrls == 999999), 999999, ingrl), # no se encuentra especificado el caso se incrementa en dic19
    ingrl = if_else((is.na(ingr) & !is.na(ingrls) & ingrls == 999999), 999999, ingrl), # no se encuentra especificado el caso se incrementa en nov19
    ingrl = if_else((!is.na(ingrls) & ingrls == 999999 & (!is.na(ingr) & ingr < 999999 & ingr >= 0)), (ingrl + ingr), ingrl),
    ingrl = if_else((is.na(ingr) & is.na(ingrls)), NA_real_, ingrl),
    # No laboral
    # Ingreso Total
    ingrltot = 0,
    ingrltot = if_else(p71a == 1 & !is.na(p71b) & p71b < 999999, (ingrltot + p71b), ingrltot),
    ingrltot = if_else(p72a == 1 & !is.na(p72b) & p72b < 999999, (ingrltot + p72b), ingrltot),
    ingrltot = if_else(p73a == 1 & !is.na(p73b) & p73b < 999999, (ingrltot + p73b), ingrltot),
    ingrltot = if_else(p74a == 1 & !is.na(p74b) & p74b < 999999, (ingrltot + p74b), ingrltot),
    ingrltot = if_else(p75 == 1 & !is.na(p76) & p76 < 999999, (ingrltot + p76), ingrltot),
    ingrltot = if_else(!is.na(ingrl) & ingrl == 999999, 999999, ingrltot),
    ingrltot = if_else(!is.na(ingrl) & ingrl > -1 & ingrl < 999999, (ingrltot + ingrl), ingrltot),
    # personas que no tienen inversiones, transferencias, bdh, y no tienen ingresos del trabajo.
    ingrltot = if_else(is.na(p71b) & is.na(p72b) & is.na(p73b) & is.na(p74b) & is.na(p76) & (is.na(ingrl) | ingrl == -1), NA_real_, ingrltot),
    ingrltot = if_else(is.na(ingrl) & (!is.na(ingrltot) & ingrltot == 0), NA_real_, ingrltot),
    #Se incrementa en abr-20, siete nueves (9s)
    ingrltot = if_else(!is.na(p63) & (p63 == 999 | p63 == 9999 | p63 ==99999  | p63 == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p64b) & (p64b == 999 | p64b == 9999 | p64b ==99999  | p64b == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p65) & (p65 == 999 | p65 == 9999 | p65 ==99999  | p65 == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p66) & (p66 == 999 | p66 == 9999 | p66 ==99999  | p66 == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p67) & (p67 == 999 | p67 == 9999 | p67 ==99999  | p67 == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p68b) & (p68b == 999 | p68b == 9999 | p68b ==99999  | p68b == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p69) & (p69 == 999 | p69 == 9999 | p69 ==99999  | p69 == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p70b) & (p70b == 999 | p70b == 9999 | p70b ==99999  | p70b == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p71b) & (p71b == 999 | p71b == 9999 | p71b ==99999  | p71b == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p72b) & (p72b == 999 | p72b == 9999 | p72b ==99999  | p72b == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p73b) & (p73b == 999 | p73b == 9999 | p73b ==99999  | p73b == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p74b) & (p74b == 999 | p74b == 9999 | p74b ==99999  | p74b == 9999999), 999999, ingrltot),
    ingrltot = if_else(!is.na(p76) & (p76 == 999 | p76 == 9999 | p76 ==99999  | p76 == 9999999), 999999, ingrltot),
    # Se incrementa en abr-20, hace ingreso laboral 999999 cuando no tiene ingresos laborales y no laborales.
    ingrl = if_else((!is.na(p66) & p66 == 999999) | (!is.na(p63) & p63 == 999999) |
      ((!is.na(p70b) & p70b == 999999 & !is.na(p69) & p69 == 999999 &
        ((p71b == 999999 | is.na(p71b)) &
          (p72b == 999999 | is.na(p72b)) &
          (p73b == 999999 | is.na(p73b)) &
          (p74b == 999999 | is.na(p74b)) &
          (p76 == 999999 | is.na(p76))))), 999999, ingrl),
    # Se incrementa en abr-20, hace ingreso laboral 999999 cuando no tiene ingresos laborales y no laborales.
    ingrltot = if_else((!is.na(p66) & p66 == 999999) | (!is.na(p63) & p63 == 999999) |
      ((!is.na(p70b) & p70b == 999999 & !is.na(p69) & p69 == 999999 &
        ((p71b == 999999 | is.na(p71b)) &
          (p72b == 999999 | is.na(p72b)) &
          (p73b == 999999 | is.na(p73b)) &
          (p74b == 999999 | is.na(p74b)) &
          (p76 == 999999 | is.na(p76))))), 999999, ingrltot),
    ingrltot = case_when(
      ingrltot == 0 ~ NA_real_,
      ingrltot == 999999 ~ NA_real_, T ~ ingrltot
    )
  ) %>% 
  arrange(id_hogar) %>% 
  group_by(id_hogar)  %>% 
  mutate(
    ingrlt = sum(ingrltot, na.rm = T),
    ingrlt = case_when(ingrlt == 0 ~ NA_real_, T ~ ingrlt),
    N_BREAK = n()
  ) %>% 
  ungroup() %>% 
  mutate(ingpc = (ingrlt / N_BREAK)) %>% 
  ungroup()
}

# #
# #apply(data[, c("ingr", "ingrls", "ingrl", "ingrltot", "ingrlt", "N_BREAK", "ingpc")], 2, mean, na.rm = T)
# #apply(data[, c("ingr", "ingrls", "ingrl", "ingrltot", "ingrlt", "N_BREAK", "ingpc")], 2, sum, na.rm = T)
# 
# # pruebas No ejecutar
# # data[1,"ingrl_inec"] = NA
# 
# # Se verifica con con l ingreso laboral y el percapita que coincida
# validaciones_ingresos <- data %>% 
#   tidyr::replace_na(list(ingrl_inec = -100, ingrl = -100)) %>% 
#   tidyr::replace_na(list(ingpc_inec = -100, ingpc = -100)) %>% 
#   mutate(
#     dif_ingrl = ingrl_inec - ingrl,
#     dif_ingpc = ingpc_inec - ingpc,
#     ingrl_inec = as.numeric(ingrl_inec)
#   ) |>
#   filter(dif_ingrl != 0 | dif_ingpc != 0) |>
#   select("id_persona", "ingrl", "ingrl_inec", "dif_ingrl", "ingpc", "ingpc_inec", "dif_ingpc")
# 
# 
# # Tabulados ---------------------------------------------------------------
# data = data %>% mutate(t=1)
# Desagregaciones <- c("t", "area", "p02")
# 
# ingresos_validaciones <- Desagregaciones %>%
#   map_dfr(
#     ~ bind_rows((data %>%
#         group_by(!!sym(.x)) %>%
#         summarize(
#           ingr = mean(ingr, na.rm = T),
#           ingrls = mean(ingrls, na.rm = T),
#           ingrl = mean(ingrl, na.rm = T),
#           ingrltot = mean(ingrltot, na.rm = T),
#           ingrlt = mean(ingrlt, na.rm = T),
#           N_BREAK = mean(N_BREAK, na.rm = T),
#           ingpc = mean(ingpc, na.rm = T)
#         ))) %>%
#       mutate(Indicador = .x, Categorías = !!sym(.x)) %>%
#       select(-.x)
#   )
# 
# rio::export(list(aa = validaciones_ingresos,
#   bb = ingresos_validaciones),"varios/validaciones_ingresos.xlsx",rowNames = TRUE, overwrite = TRUE)
# 
# 
# 
# 