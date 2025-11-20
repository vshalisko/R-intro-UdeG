### Codificador binario para variable categorica

encoder_b2 <- function(df, c_fuente, c_res1, c_res2) {
  ## df - tabla
  ## c_fuente - nombre de la columna a recodificar
  ## c_res1 - nombre de la primera columna binaria
  ## c_res2 - nombre de la segunda columna binaria
  ## recodificación:
  ## 0 -> 0, 0
  ## 1 -> 1, 0
  ## 2 -> 0, 1
  ## 3 -> 1, 1
  df[,c_res1] <- ifelse(df[,c_fuente] == 1 | df[,c_fuente] == 3, 1, 0)
  df[,c_res2] <- ifelse(df[,c_fuente] == 2 | df[,c_fuente] == 3, 1, 0)
  return(df)
}


encoder_b3 <- function(df, c_fuente, c_res1, c_res2, c_res3) {
  ## df - tabla
  ## c_fuente - nombre de la columna a recodificar
  ## c_res1 - nombre de la primera columna binaria
  ## c_res2 - nombre de la segunda columna binaria
  ## c_res2 - nombre de la tercera columna binaria
  ## recodificación:
  ## 0 -> 0, 0, 0
  ## 1 -> 1, 0, 0
  ## 2 -> 0, 1, 0
  ## 3 -> 0, 0, 1
  ## 4 -> 1, 1, 0
  ## 5 -> 1, 0, 1
  ## 6 -> 0, 1, 1
  ## 7 -> 1, 1, 1
  df[,c_res1] <- ifelse(df[,c_fuente] == 1 | 
                        df[,c_fuente] == 4 |
                        df[,c_fuente] == 5 |
                        df[,c_fuente] == 7, 1, 0)
  df[,c_res2] <- ifelse(df[,c_fuente] == 2 | 
                        df[,c_fuente] == 4 |
                        df[,c_fuente] == 6 |
                        df[,c_fuente] == 7, 1, 0)
  df[,c_res3] <- ifelse(df[,c_fuente] == 3 | 
                        df[,c_fuente] == 5 |
                        df[,c_fuente] == 6 |
                        df[,c_fuente] == 7, 1, 0)
  return(df)
}

## ejemplo de uso con la tabla mediciones que contiene la columan TA

mediciones <- data.frame(TA = c(3, 2, 1, 2, 0))

mediciones1 <- encoder_b2(mediciones, "TA", "TA_B1", "TA_B2")
summary(mediciones1)

mediciones2 <- encoder_b3(mediciones, "TA", "TA_B1", "TA_B2", "TA_B3")
summary(mediciones2)
