####################################################################################################
# 02 - EJEMPLO DE LA RUINA DEL JUGADOR
####################################################################################################

# DESCRIPCION ######################################################################################
# Función que simula una trayectoria para la ruina del jugador
#
# ARGUMENTOS #######################################################################################
# dinero_propio       := dinero de uno mismo
# dinero_contrincante := dinero del contrincante
# probabilidad        := Probabilidad de ganar en cada paso
####################################################################################################
ruina_jugador <- function(dinero_propio, dinero_contrincante, probabilidad = 1/2){
  dinero_total  <- dinero_propio + dinero_contrincante
  dinero_actual <- dinero_propio
  historico_dinero <- c(dinero_propio)
  fin_del_juego    <- FALSE
  while( fin_del_juego == FALSE ){
    dinero_actual <- dinero_actual + sample(x = c(1,-1), size = 1, prob = c(probabilidad, 1-probabilidad) )
    historico_dinero <- c(historico_dinero, dinero_actual)
    if( dinero_actual == 0 || dinero_actual == dinero_total ){
      fin_del_juego <- TRUE
    }
  }
  flag_ganador <- ifelse( dinero_actual == dinero_total, "¡GANASTE!", "!PERDISTE!" )
  plot(historico_dinero,type='n',main=paste0("Ruina del Jugador\n",flag_ganador),
       xlab="Espacio Parametral [n]", ylab="Espacio de Estados [X_n]" ); grid(lwd = 3)
  lines(historico_dinero,col="firebrick1",pch=19,lwd=3)
  points(historico_dinero,col="royalblue",pch=19,cex=2)
}

# EJEMPLO  #########################################################################################
ruina_jugador(dinero_propio = 10, dinero_contrincante = 15, probabilidad = 1/2)
####################################################################################################

