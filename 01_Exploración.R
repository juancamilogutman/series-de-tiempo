rm(list = ls())

library(tidyverse) #Para manejar bases de datos
library(tsibble) #Para manejar bases para series de tiempo
library(ggplot2) #Para graficar
library(knitr) #Para tablas

df <- readRDS("bases/base_tp2.RDS")

# Pequeña exploración
names(df)
str(df)
head(df, 15)
tail(df, 15)


# combined_plot <- autoplot(df, log_X, color="Blue") +
#   labs(title = "Exportaciones e Importaciones Argentinas",
#        subtitle = "1996-2019",
#        y = "Logaritmo de Exportaciones", x= "Año y trimestre") + 
#   theme_minimal()
# 
# combined_plot <- combined_plot + 
#   autolayer(df, log_M, series = "Importaciones", colour="Red") +
#   labs(y = "Logaritmo de Exportaciones e Importaciones") +
#   theme_minimal()
# 
# print(combined_plot)
# 
# otro_autoplot <- autoplot(df, log_PBI_Socios, color="Blue") +
#   labs(title = "Exportaciones e Importaciones Argentinas",
#        subtitle = "1996-2019",
#        y = "Logaritmo de Exportaciones",
#        x= "Año y trimestre") + 
#   theme_minimal() 
# 
# otro_autoplot + autolayer(df, log_PBI_Arg, series = "Importaciones", colour="Red") +
#   labs(y = "Logaritmo de Exportaciones e Importaciones") +
#   theme_minimal()
# 
# df_long <- df %>%
#   pivot_longer(cols = c(log_X, log_PBI_Socios, log_TCRM), 
#                names_to = "series", 
#                values_to = "value")
# 
# ggplot(df_long, aes(x = Q, y = value, color = series)) +
#   geom_line() +
#   theme_minimal() +
#   labs(title = "Time Series Plot", x = "Quarter", y = "Log Value")
# 
# names(df)
# 
# 
# para_plot <- df[, c("Q", "log_M", "log_PBI_Arg", "log_TCRM"), drop = FALSE]
# 
# 
# autoplot(para_plot) +
#   facet_wrap(~ para_plot$Q) + # Replace key_variable with your actual key column
#   labs(x = "Año", y = "Variables", 
#        title = "Argentina: PIB, importaciones y tipo de cambio real") +
#   guides(colour=guide_legend(title="Variables"))
# 
# ggplot(df_post_2001_long, aes(x = Q, y = value, color = series)) +
#   geom_line() +
#   scale_x_yearquarter(date_breaks = "1 quarter") + 
#   theme_minimal() +
#   labs(title = "Time Series Plot", x = "Quarter", y = "Log Value")