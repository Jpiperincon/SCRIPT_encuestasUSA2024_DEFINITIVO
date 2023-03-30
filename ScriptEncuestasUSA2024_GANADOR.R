# instalar paquete para poder lear excel
install.packages("readxl")

#cargar la libreria que permitirá leer el excel 
library(readxl)

# buscar la ruta de acceso del archivo 
file.choose()

#cargar el docuemnto
encuesta2024 <- "E:\\Proyectos R\\datos taller elecciones USA\\Encuestas_USA_formato_numeric.xlsx"
encuesta2024 <- read_excel("E:\\Proyectos R\\datos taller elecciones USA\\Encuestas_USA_formato_numeric.xlsx")

#visualizar el dataset
View(encuesta2024)

# revisar la estructura del dataset
str(encuesta2024)

class(encuesta2024$Fecha)
encuesta2024$Fecha = as.Date(encuesta2024$Fecha)
class(encuesta2024$Fecha)

class(encuesta2024$`%Winfrey`)
class(encuesta2024$`%Johnson`)
class(encuesta2024$`%Dif`)

#cargar e instalar libreria dplyr

install.packages("dplyr")
library(dplyr)

#agrupar datos por fechas y hallar promedio por día

encuesta_dia <- encuesta2024 %>% 
  group_by(Fecha)

promedio_dia <- encuesta_dia %>% 
  summarise(promedio_Winfrey = mean(`%Winfrey`),
            promedio_Johnson = mean(`%Johnson`),
            promedio_Dif = mean(`%Dif`))

View(encuesta_dia)
View(promedio_dia)

#Código para hallar el promedio movil

install.packages("zoo")

library("zoo")

# Crear un objeto zoo a partir del dataset promediado por día
encuestas_zoo <- zoo(promedio_dia[,2:4], order.by = as.Date(promedio_dia$Fecha))

# Calcular promedio móvil de los últimos 7 días
promedio_movil <- rollapply(encuestas_zoo, width = 8, FUN = mean, align = "right", fill = NA)

# Convertir objeto zoo a data.frame
promedio_movil_df <- data.frame(Fecha = as.Date(index(promedio_movil)), coredata(promedio_movil))

View(promedio_movil_df)

# Crear la gráfica de línea 2D

ggplot(data = promedio_dia, aes(x = Fecha)) +
  geom_line(aes(y = promedio_Winfrey, color = "%Winfrey"), size = 0.5) +
  geom_line(aes(y = promedio_Johnson, color = "%Johnson"), size = 0.5) +
  
  scale_color_manual(values = c("%Winfrey" = "blue", "%Johnson" = "red")) +
  xlab("Fecha") +
  ylab("Promedio día") +
  ggtitle("Promedio encuestas candidatos USA 2024")

#crear la gráfica de línea 2D con promedio_movil_df 

ggplot(data = promedio_movil_df, aes(x = Fecha)) +
  geom_line(aes(y = promedio_Winfrey, color = "%Winfrey"), size = 0.5) +
  geom_line(aes(y = promedio_Johnson, color = "%Johnson"), size = 0.5) +
  
  scale_color_manual(values = c("%Winfrey" = "blue", "%Johnson" = "red")) +
  xlab("Fecha") +
  ylab("Promedio día") +
  ggtitle("Promedio movil encuestas candidatos USA 2024")