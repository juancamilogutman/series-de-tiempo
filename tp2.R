library(tidyverse) #Para manejar bases de datos
library(tsibble) #Para manejar bases para series de tiempo
library(fable) #Para correr modelos de series de tiempo
library(ggplot2) #Para graficar
library(knitr) #Para tablas
library(fpp3) #No recuerdo para que era
library(vars)

# library(readxl)
# base_excel <- read_excel("TP 2 - Datos.xlsx")
# saveRDS(excel_data, "base_tp2.rds")

df <- readRDS("base_tp2.RDS")

df <- df %>%
  rename(
    t = ...1,
    PBI_ARG = PIBARG,
    PBI_SOCIOS = PIBSOCIOS
  )

# Pequeña exploración
kable(head(df, 15))
kable(tail(df, 15))
names(df)

# Formato fechas:
df <- df %>%
  mutate(t = yearquarter(t))

df <- df %>% as_tsibble(index = t)

# Plot PBI ARG
autoplot(df, PBI_ARG) +
  labs(title = "PBI Argentino",
       subtitle = "1996-2019",
       y = "PBI")

# Plot EXPO ARG
autoplot(df, EXPO) +
  labs(title = "PBI Argentino",
       subtitle = "1996-2019",
       y = "PBI")
 