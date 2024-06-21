library(tidyverse) #Para manejar bases de datos
library(tsibble) #Para manejar bases para series de tiempo
library(fable) #Para correr modelos de series de tiempo
library(ggplot2) #Para graficar
library(knitr) #Para tablas
library(fpp3) #No recuerdo para que era
library(vars)
library(urca)
library(ggplot2)
library(forecast) # Assuming autoplot comes from the forecast package

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

df <- df %>% mutate(log_EXPO = log(df$EXPO))
df <- df %>% mutate(log_IMPO = log(df$IMPO)) 

# Plot PBI ARG
autoplot(df, PBI_ARG) +
  labs(title = "PBI Argentino",
       subtitle = "1996-2019",
       y = "PBI")

# Plot EXPO ARG
autoplot(df, log_EXPO) +
  labs(title = "Exportaciones Argentinas",
       subtitle = "1996-2019",
       y = "Exportaciones")

# Plot IMPO ARG
autoplot(df, log_IMPO) +
  labs(title = "Importaciones Argentinas",
       subtitle = "1996-2019",
       y = "Importaciones")



# Your data frame 'df' should have the columns 'log_EXPO' and 'log_IMPO'

# Create the base plot with the export data
combined_plot <- autoplot(df, log_EXPO) +
  labs(title = "Exportaciones e Importaciones Argentinas",
       subtitle = "1996-2019",
       y = "Logaritmo de Exportaciones") +
  theme_minimal()

# Add the import data as a layer
combined_plot <- combined_plot + 
  autolayer(df, log_IMPO, series = "Importaciones") +
  labs(y = "Logaritmo de Exportaciones e Importaciones") +
  theme_minimal()

# Print the combined plot
print(combined_plot)

#La hipótesis nula del test es que hay raíz unitaria.
resultado_adf_impo <- ur.df(df$IMPO)
resultado_adf_log_impo <- ur.df(df$log_IMPO)

print(resultado_adf_impo)
print(resultado_adf_log_impo)