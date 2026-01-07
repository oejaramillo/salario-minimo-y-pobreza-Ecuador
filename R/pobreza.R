pobreza_por_ingreso <- function(data) {
  data <- data %>%
    mutate(
      lpobreza = 56.64 * (ipc_ / 70.262819184092),
      pobreza = ifelse(!is.na(ingpc), 0, NA),
      pobreza = replace(pobreza, (ingpc < lpobreza), 1)
    ) %>%
    mutate(
      pobreza = labelled(pobreza,
        labels = c(
          "no pobreza" = 0,
          "pobreza" = 1
        )
      )
    ) %>%  
    mutate(
      lepobreza = 31.92 * (ipc_ / 70.262819184092),
      epobreza = ifelse(!is.na(ingpc), 0, NA),
      epobreza = replace(epobreza, (ingpc < lepobreza), 1)
    ) %>%
    mutate(
      epobreza = labelled(epobreza,
        labels = c(
          "no indigente" = 0,
          "indigente" = 1
        )
      )
    ) %>% 
    mutate(
      brecha = ((lpobreza - ingpc) / lpobreza),
      severidad = ((lpobreza - ingpc) / lpobreza)^2
    )
}

