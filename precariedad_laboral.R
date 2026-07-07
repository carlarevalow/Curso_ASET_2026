install.packages("tidyverse")
install.packages("openxlsx")
install.packages("eph")
install.packages("ggthemes")
install.packages("readxl")

library(tidyverse)
library(openxlsx)
library(eph)
library(ggthemes) # diseños preconfigurados para los gráficos ggplot
library(readxl) # Cargamos readxl para traer levantar excels

## Datos del 4to trimestre de 2025
eph_2025_t4 <- eph::get_microdata(year = 2025, period = 4)

# Me quedo con los ocupados asalariados de 18 a 60 años
asalariados_2025_t4 <- eph_2025_t4 %>%
  
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  filter(CH06 >= 18 & CH06 <= 60) # de 18 a 60 años

summary(asalariados_2025_t4$CH06)


## PRECARIEDAD LABORAL: aportes jubilatorios, trabajo a tiempo parcial involuntario y contrato a tiempo determinado
asalariados_precarios <- asalariados_2025_t4 %>%
  mutate(
    descuento_jub = case_when(
      PP07H == 1 ~ "Si",
      PP07H == 2 ~ "No"
    ),
    part_time_inv = case_when(
      PP3E_TOT < 35 & PP03G == 1 ~ "Si",
      TRUE ~ "No"
    ),
    tiempo_determinado = case_when(
      PP07C == 1 ~ "Si",
      TRUE ~ "No"
    ),
    precariedad = case_when(
      descuento_jub == "No" |
      part_time_inv == "Si" |
      tiempo_determinado == "Si" ~ 1,
      TRUE ~ 0
    ),
    sexo = case_when(
      CH04 == 1 ~ "Hombre",
      CH04 == 2 ~ "Mujer"
    ),
    gedad = case_when(
      CH06 >= 18 & CH06 <= 25 ~ "18-25",
      CH06 >= 26 & CH06 <= 40 ~ "26-40",
      CH06 >= 41 & CH06 <= 60 ~ "41-60"
    )
  )


names(asalariados_precarios) #Reviso las variables q tengo en la base

## Cuadros descriptivos de precariedad laboral por sexo y edad

vector_precariedad <- c(
  "descuento_jub",
  "part_time_inv",
  "tiempo_determinado",
  "precariedad"
)

wb <- createWorkbook() # Creo un workbook para guardar los resultados de los cuadros descriptivos

for (i in vector_precariedad) {

cat("Procesando:", i, "\n")

# TABLAS CANTIDADES Y PORCENTAJES POR SEXO Y EDAD
tabla_sexo <- calculate_tabulates(
  base = asalariados_precarios,
  x = i,
  y = "sexo",
  weights = "PONDERA"
)

print(tabla_sexo)

addWorksheet(wb, paste0(i, "_sexo"))
writeData(wb, sheet = paste0(i, "_sexo"), x = tabla_sexo)

tabla_edad <- calculate_tabulates(
  base = asalariados_precarios,
  x = i,
  y = "gedad",
  weights = "PONDERA"
)

print(tabla_edad)

addWorksheet(wb, paste0(i, "_edad"))
writeData(wb, sheet = paste0(i, "_edad"), x = tabla_edad)

tabla_sexo_pct <- calculate_tabulates(
  base = asalariados_precarios,
  x = i,
  y = "sexo",
  add.percentage = "col",
  weights = "PONDERA"
)
print(tabla_sexo_pct)

addWorksheet(wb, paste0(i, "_sexo_pct"))
writeData(wb, sheet = paste0(i, "_sexo_pct"), x = tabla_sexo_pct)

tabla_edad_pct <- calculate_tabulates(
  base = asalariados_precarios,
  x = i,
  y = "gedad",
  add.percentage = "col",
  weights = "PONDERA"
)

print(tabla_edad_pct)

addWorksheet(wb, paste0(i, "_edad_pct"))
writeData(wb, sheet = paste0(i, "_edad_pct"), x = tabla_edad_pct)


}

saveWorkbook(
  wb,
  file = "resultados/Resultados_precariedad.xlsx",
  overwrite = TRUE
)

names(asalariados_precarios)

# Graficar porcentajes por grupo de edad

tabla_sexo <- asalariados_precarios %>%
  group_by(sexo) %>%
  summarise(
    precariedad = weighted.mean(precariedad, PONDERA, na.rm = TRUE),
    .groups = "drop"
  )


grafico_sexo <-ggplot(tabla_sexo,
       aes(x = sexo,
           y = precariedad)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = scales::percent(precariedad, accuracy = 0.1)),
    vjust = -0.5
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Sexo",
    y = "Proporción de trabajadores precarios",
    title = "Precariedad laboral según sexo"
  ) +
  theme_minimal(base_size = 20)

ggsave(
  filename = "resultados/grafico_precariedad_sexo.png",
  plot = grafico_sexo,
  width = 8,
  height = 6,
  dpi = 300
)
tabla_edad <- asalariados_precarios %>%
  group_by(gedad) %>%
  summarise(
    precariedad = weighted.mean(precariedad, PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    gedad = factor(gedad,
                   levels = c("18-25", "26-40", "41-60"))
  )

grafico_edad <- ggplot(tabla_edad,
       aes(x = gedad,
           y = precariedad)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::percent(precariedad, accuracy = 0.1)),
            vjust = -0.5,
            size = 5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Grupo de edad",
    y = "Proporción de trabajadores precarios",
    title = "Precariedad laboral según grupo de edad"
  ) +
  theme_minimal(base_size = 20)

ggsave(
  filename = "resultados/grafico_precariedad_edad.png",
  plot = grafico_edad,
  width = 8,
  height = 6,
  dpi = 300
)

# LOG DE EJECUCIÓN
cat(
  paste(Sys.time(), "- Script ejecutado correctamente\n"),
  file = "resultados/log.txt",
  append = TRUE
)

archivo_log <- "resultados/log.csv"
# LOG DE EJECUCIÓN
cat(
  paste(Sys.time(), "- Script ejecutado correctamente\n"),
  file = "resultados/log.txt",
  append = TRUE
)