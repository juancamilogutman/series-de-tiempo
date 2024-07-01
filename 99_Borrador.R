# 
# #La hipótesis nula del test es que hay raíz unitaria.
# resultado_adf_impo <- ur.df(df$IMPO)
# resultado_adf_log_impo <- ur.df(df$log_IMPO)
# 
# summary(resultado_adf_impo)
# # En este caso el estadístico de prueba es negativo y no rechazamos
# # ni con 90, 95 ni 99 de confianza
# 
# summary(resultado_adf_log_impo)

# forecast::auto.arima(df$log_X, max.p = 5, max.q = 5, start.p = 1,
#            start.q = 1, ic = "aic")
# forecast::auto.arima(df$log_M, max.p = 5, max.q = 5, start.p = 1,
#            start.q = 1, ic = "aic")

#alt + guión
#control shift M es el pipe
#ctrl + alt + arriba crea cursores
#ctrl shift C comenta
#ctrl shift 1 y 2 

# #Slicing 2001
# start_2001_Q1 <- which(df$Q == yearquarter("2001 Q1"))
# df_pre_2001 <- df %>% slice(1:(start_2001_Q1 - 1))
# df_post_2001 <- df %>% slice(start_2001_Q1:n())
# 
# 
# df_post_2001_long <- df_post_2001 %>%
#   pivot_longer(cols = c(log_X, log_PBI_Socios, log_TCRM), 
#                names_to = "series", 
#                values_to = "value")

# ggplot(df_X_long, aes(x = Q, y = value, color = series)) +
#   geom_line() +
#   theme_minimal() +
#   labs(title = "Time Series Plot", x = "Quarter", y = "Log Value")

# X_impo <- cbind(df$log_PBI_Arg, df$log_TCRM)
# coint.test(df$log_M, X_impo, d = 1, nlag = NULL, output = TRUE)
# coint.test(df$log_M, X_impo, d = 0, nlag = NULL, output = TRUE)

# # ECM IMPORTACIONES (bivariado, uniecuacional)
# reg_coint_impo <- lm(log_M ~ log_PBI_Arg,data=df) #Relación de largo plazo
# residuos_impo <- reg_coint_impo$residuals # Capturamos los residuos
# tseries::adf.test(residuos_impo) # Vemos si el residuo es estacionario (OK)
# 
# residuos_impo_lag <- -lag(residuos_impo)[-1]
# ecm_impo_2 <- lm(diff(log_M) ~ residuos_impo_lag + diff(log_PBI_Arg), data = df)
# summary(ecm_impo_2)

# vec2var()
