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

#Slicing 2001
start_2001_Q1 <- which(df$Q == yearquarter("2001 Q1"))
df_pre_2001 <- df %>% slice(1:(start_2001_Q1 - 1))
df_post_2001 <- df %>% slice(start_2001_Q1:n())


df_post_2001_long <- df_post_2001 %>%
  pivot_longer(cols = c(log_X, log_PBI_Socios, log_TCRM), 
               names_to = "series", 
               values_to = "value")