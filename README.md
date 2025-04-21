# Cyclistic-Bike-Sharing
Bike sharing usage analysis by user type for 2024.
## Overview  
Dashboard comparing **members vs. casual users** in a bike-sharing system.
Detailed report of all the steps of the project. Including description and tools used throughout the process.
Most relevant recomendations for stakeholders given the initial business hypotesis to solve.
## Key Insights  
-Members take 63% of the total trips, but casual users used 53% of the total minutes , with an average ride 52% longer than members.

-Summer months peak for both user types.

-Members have similar usage during the whole week while weekends spike for casual users.

-E-bikes are most popular, marginally ahead of classic bikes for both types of users.

## Data
Public dataset ([https://divvy-tripdata.s3.amazonaws.com/index.html])
  
## Dashboard Preview
![Dashboard 1](https://github.com/user-attachments/assets/6e6a5d60-95ba-4c18-aa47-c85d4bdbc505)

  **Dashboard Online**: [Ver en Tableau Public]([https://public.tableau.com/views/CyclisticAnalysis2024_17451901427050/Dashboard1?:language=en-GB&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link]

## Final Report 
Download detailed report:
[Reporte Final.pdf](https://github.com/user-attachments/files/19836051/Reporte.Final.pdf)

## Analysis Code RStudio
Includes Data Cleaning, Transformation and Validation to be used in Tableau.
### Requirements  
- R 4.0+.  
- Packages: `tidyverse`, `ggplot2`,  `dplyr`, `luridate`, `tidyr`, `stringr`.


[Uploading Script Cyclistic # Establecer el directorio donde está el archivo
setwd("C:/Users/leobl/OneDrive/Desktop/Cyclistic")
getwd()

#Verificar los archivos csv del directorio

list.files(pattern = "\\.csv$")

#Leer los archivo CSV y unirlos

library(dplyr)
archivos_csv <- list.files(pattern = "\\.csv$")
monthly_data <- archivos_csv %>%
  lapply(read.csv, stringsAsFactors = FALSE) %>%
  bind_rows()

#Guardar el nuevo archivo

write.csv(monthly_data, "monthly_data.csv", row.names = FALSE)

#Ver las primeras filas

head(monthly_data)
names(monthly_data)

#Renombrar filas

names(monthly_data) <- c("id", "bike_type", "started_at", "ended_at",         
                         "start_station_name", "start_station_id", "end_station_name","end_station_id",    
                         "start_lat", "start_lng", "end_lat", "end_lng", "user_type")

#Verificar tipos de datos

str(monthly_data)

#Cambiar inicio y fin a POSIXct

head(monthly_data$started_at)
library(lubridate)
monthly_data$started_at <- as.POSIXct(monthly_data$started_at, 
                                        format = "%Y-%m-%d %H:%M:%S",
                                        tz = "UTC")
head(monthly_data$started_at)
monthly_data$ended_at <- as.POSIXct(monthly_data$ended_at, 
                                      format = "%Y-%m-%d %H:%M:%S",
                                      tz = "UTC")
head(monthly_data$ended_at)

#Creamos nueva columna con la duracion del viaje en minutos

monthly_data$ride_lenght_mins <- as.numeric(
  difftime(monthly_data$ended_at, monthly_data$started_at, units = "mins")
)
head(monthly_data$ride_lenght_mins)

#Buscamos valores nulos o inconsistentes

summary(monthly_data$ride_lenght_mins)

#Filtramos los valores inconsistentes obtenidos. Viajes negativos o muy largos

monthly_data_cleaned <- monthly_data[monthly_data$ride_lenght_mins >= 0, ]
monthly_data_cleaned <- monthly_data_cleaned[monthly_data_cleaned$ride_lenght_mins <= 1440, ]
summary(monthly_data_cleaned$ride_lenght_mins)

#Verificamos valores atípicos

outliers <- monthly_data[monthly_data$ride_lenght_mins < 0 | monthly_data$ride_lenght_mins > 1440, ]
View(outliers)

#Hay valores negativos, se determina que el error es que en estos campos los valores de las columnas
#inicio y fin están intercambiados. Lo solucionamos

negativos <- which(monthly_data$ride_lenght_mins < 0)
monthly_data[negativos, c("started_at", "ended_at")] <- monthly_data[negativos, c("ended_at", "started_at")]
monthly_data$ride_lenght_mins <- as.numeric(
  difftime(monthly_data$ended_at, 
           monthly_data$started_at, 
           units = "mins"))
summary(monthly_data$ride_lenght_mins)

#No viajes negativos. Viaje más largo 26 horas. Considero realista y son solo 7596 0,13% 
#con lo cual decido dejarlos para el analisis

sum(monthly_data$ride_lenght_mins > 1440, na.rm = TRUE)

#Crear columna día de la semana

monthly_data$day_of_week <- wday(monthly_data$started_at, label = TRUE, abbr = FALSE)
head(monthly_data[c("started_at", "day_of_week")])

#Contar viajes por d

library(dplyr)

#Verificar que todos los viajes tienen al menos alguno de los parametros de identificacion 
#tanto de estacion de inicio como de fin para asegurarnos que no hay viajes inválidos respecto a estos

invalid_rows <- monthly_data %>%
  filter(
    is.na(start_station_name) & 
      is.na(start_station_id) & 
      !(is.na(start_lat) | is.na(start_lng)) 

invalid_rows_end <- monthly_data %>%
  filter(
    is.na(end_station_name) & 
      is.na(end_station_id) & 
      !(is.na(end_lat) | is.na(end_lng))
  )
print(invalid_rows) 
#todos los viajes se pueden identificar inicio y fin con 
#al menos uno de los datos

#calcular viajes por mes

monthly_data <- monthly_data %>%
  mutate(
    mes = month(started_at, label = TRUE, abbr = FALSE)
    )

viajes_por_mes <- monthly_data %>%
  group_by(mes, user_type) %>%
  summarise(
    total_viajes = n(),
    .groups = 'drop'
  )

tabla_resumen <- viajes_por_mes %>%
  tidyr::pivot_wider(
    names_from = user_type,
    values_from = total_viajes,
    values_fill = 0
  )

print(tabla_resumen)

library(ggplot2)

ggplot(viajes_por_mes, aes(x = mes, y = total_viajes, fill = user_type)) +
  geom_col(position = "stack") +
  labs(
    title = "Viajes por mes y tipo de usuario",
    x = "Mes",
    y = "Total de viajes",
    fill = "Tipo de usuario"
  ) +
  facet_wrap(~mes, scales = "free_x") +
  theme_minimal()

#version porcentajes

viajes_por_mes %>%
  group_by(mes) %>%
  mutate(
    porcentaje = round(total_viajes / sum(total_viajes) * 100, 1)
  ) %>%
  tidyr::pivot_wider(
    names_from = user_type,
    values_from = c(total_viajes, porcentaje)
  )

#calculos finales para el análisis y gráficos. Luego a Tableau

library(tidyr)

#totales generales

totals <- list(
  total_rides = nrow(monthly_data),
  total_minutes = sum(monthly_data$ride_lenght_mins, na.rm = TRUE),
  avg_ride_lenght = mean(monthly_data$ride_lenght_mins, na.rm = TRUE)
)

#por tipo de ususario

by_user_type <- monthly_data %>% 
  group_by(user_type) %>% 
  summarise(
    Rides = n(),
    total_minutes = sum(ride_lenght_mins),
    avg_ride_lenght = mean(ride_lenght_mins)
  )

#por día de la semana y usuario

print(Rides_per_day_of_week)
#se calculó antes

#por mes y tipo de usuario

rides_month_user_type <- monthly_data %>% 
  mutate(mes = month(started_at, label = TRUE, abbr = FALSE)) %>% 
  count(mes, user_type, name = "rides_month_user_type")

#preferencia bicicleta

popular_bikes <- monthly_data %>% 
  count(bike_type, user_type, name = "total_rides_bike") %>% 
  group_by(user_type) %>% 
  mutate(porcentaje = round(total_rides_bike / sum(total_rides_bike) * 100, 1))

#estaciones mas populares
#Top 5 por tipo de usuario

popular_stations <- monthly_data %>% 
  count(start_station_name, user_type, name = "total_inicios") %>% 
  group_by(user_type) %>% 
  slice_max(total_inicios, n = 5)  

#visualizaciones clave
#viajes por tipo de usuario, barras

ggplot(by_user_type, aes(x = user_type, y = Rides, fill = user_type)) +
  geom_col() +
  labs(title = "Total Rides by User Type")

#promedio duracion por usuario tipo

ggplot(by_user_type, aes(x = user_type, y = AVG_Lenght, fill = user_type)) +
  geom_col() +
  labs(title = "Average Ride Lenght by User Type")

#guardo csv para exportar a tableau public

write.csv(by_user_type, "by_user_type.csv", row.names = FALSE)
write.csv(Rides_per_day_of_week, "rides_per_day_of_week.csv", row.names = FALSE)
write.csv(rides_month_user_type, "rides_month_user_type.csv", row.names = FALSE)
write.csv(popular_bikes, "popular_bikes.csv", row.names = FALSE)
write.csv(popular_stations, "popular_stations.csv", row.names = FALSE)

#calculos que faltan y csv para dashboard en tableau

user_type_metrics <- monthly_data %>%
  group_by(user_type) %>% 
  summarise(
    total_minutes = sum(ride_lenght_mins, na.rm = TRUE),  
    avg_ride_length = mean(ride_lenght_mins, na.rm = TRUE) 
  )
print(user_type_metrics)
write.csv(popular_stations_clean, "popular_stations_clean.csv", row.names = FALSE, na = "")
write.csv(
  user_type_metrics %>% select(user_type, total_minutes),
  "total_minutes_per_user_type.csv",
  row.names = FALSE
)

write.csv(
  user_type_metrics %>% select(user_type, avg_ride_length),
  "avg_ride_length_per_user_type.csv",
  row.names = FALSE
)

popular_stations_clean <- popular_stations %>%
  filter(!is.na(start_station_name))

head(popular_stations_clean)
sum(is.na(popular_stations_clean$start_station_name))

library(stringr)
popular_stations_clean <- popular_stations %>%
  filter(
    !is.na(start_station_name) &  
      str_trim(start_station_name) != ""
  )
head(popular_stations_clean)
RStudio.R…]()
