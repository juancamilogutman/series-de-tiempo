rm(list = ls())

library(tidyverse) #Para manejar bases de datos
library(tsibble) #Para manejar bases para series de tiempo
library(ggplot2) #Para graficar
library(knitr) #Para tablas
# library(fpp3) #No recuerdo para que era
# library(vars)
# library(tseries)
# library(urca) #para el adf.test
# library(forecast)#para el auto.arima
# library(aTSA)
# library(fable) #Para correr modelos de series de tiempo

df <- readRDS("bases/base_tp2.RDS")

# Pequeña exploración
names(df)
str(df)
head(df, 15)
tail(df, 15)

# Gráfico Impo vs Expo
plot_impo_expo <- autoplot(df, log_X, color="Blue") +
  labs(title = "Exportaciones e Importaciones Argentinas",
       subtitle = "1996-2019",
       y = "Logaritmo de Exportaciones", x= "Año y trimestre") +
  theme_minimal()

plot_impo_expo <- plot_impo_expo +
  autolayer(df, log_M, series = "Importaciones", colour="Red") +
  labs(y = "Logaritmo de Exportaciones e Importaciones") +
  theme_minimal()

print(plot_impo_expo)


df_long <- df %>%
  pivot_longer(cols = c(log_X, log_PBI_Socios, log_TCRM),
               names_to = "series",
               values_to = "value")

ggplot(df_long, aes(x = Q, y = value, color = series)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series Plot", x = "Quarter", y = "Log Value")

para_plot <- df_long[, c("Q", "series", "value"), drop = FALSE]

para_plot <- para_plot %>%
  filter(para_plot$series == "log_M")

autoplot(para_plot) +
  facet_wrap(~ para_plot$series) + # Replace key_variable with your actual key column
  labs(x = "Año", y = "Variables",
       title = "Argentina: PIB, importaciones y tipo de cambio real") +
  guides(colour=guide_legend(title="Variables"))
# 
# ggplot(df_post_2001_long, aes(x = Q, y = value, color = series)) +
#   geom_line() +
#   scale_x_yearquarter(date_breaks = "1 quarter") + 
#   theme_minimal() +
#   labs(title = "Time Series Plot", x = "Quarter", y = "Log Value")