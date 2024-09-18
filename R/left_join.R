
# Cargar el paquete dplyr
library(dplyr)

# Crear la primera tabla (izquierda)
tabla_izq <- data.frame(
  id = c(1, 2, 3, 4),
  nombre = c("Juan", "María", "Pedro", "Ana")
)

# Crear la segunda tabla (derecha)
tabla_der <- data.frame(
  id = c(1, 3, 4, 5),
  ciudad = c("Madrid", "Barcelona", "Valencia", "Sevilla")
)

# Realizar el left join
resultado <- left_join(tabla_izq, tabla_der, by = "id")

# Mostrar el resultado
print(resultado)
