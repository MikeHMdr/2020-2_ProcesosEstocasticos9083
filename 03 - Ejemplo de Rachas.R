####################################################################################################
# 03 - EJEMPLO DE RACHAS
####################################################################################################

# DESCRIPCION ######################################################################################
# Función que simula una trayectoria para cadena de rachas
#
# ARGUMENTOS #######################################################################################
# n := Número de pasos
# u := Posición inicial
# p := probabilidad de incrementar en uno en cada paso.
####################################################################################################
cadena_rachas <- function(num_pasos, probabilidad = 1/2){
  resultados   <- sample(x = c(1,0), size = num_pasos, replace = TRUE, prob = c(probabilidad, 1-probabilidad) )
  racha_actual <- 0
  historico_rachas <- racha_actual
  for( i in seq_len(num_pasos) ){
    racha_actual <- ifelse( resultados[i] == 1, racha_actual + 1, 0 )
    historico_rachas <- c(historico_rachas, racha_actual )
  }
  racha_max <- max(historico_rachas)
  plot(historico_rachas,type='n',main="Cadena de Rachas",
       xlab="Espacio Parametral [n]", ylab="Espacio de Estados [X_n]" ); grid(lwd = 3)
  abline(h=racha_max, col="forestgreen", lwd=3)
  lines(historico_rachas,col="firebrick1",pch=19,lwd=3)
  points(historico_rachas,col="royalblue",pch=19,cex=2)
}

# EJEMPLO  #########################################################################################
cadena_rachas(num_pasos = 100, probabilidad = 1/2)
####################################################################################################

