library('saber')
library('caret')
library('parallel')
library('nnet')

rmse_fold <- function(pliegue, form, datos, nn_size) {
  pliegue_logic <- seq_len(nrow(datos)) %in% pliegue
  prueba <- subset(datos, pliegue_logic)
  entrena <- subset(datos, !pliegue_logic)
  modelo <- nnet(form, data = entrena, size = nn_size, linout = TRUE, trace = FALSE)
  response_name <- setdiff(names(datos), modelo$coefnames)
  Y_pronosticado <- predict(modelo, newdata = prueba)
  rmse <- RMSE(Y_pronosticado, prueba[[response_name]])
  rmse
}

tamano_muestral_max <- 10000
iteraciones <- 20
tamano_muestral <- floor(seq(500, tamano_muestral_max, length.out = iteraciones))
neuronas <- 20
n_pliegues <- 4

data("SB11_20111")

variables <- c(
  'ECON_PERSONAS_HOGAR',
  'ECON_CUARTOS',
  'ECON_SN_LAVADORA',
  'ECON_SN_NEVERA',
  'ECON_SN_HORNO',
  'ECON_SN_DVD',
  'ECON_SN_MICROHONDAS',
  'ECON_SN_AUTOMOVIL',
  'MATEMATICAS_PUNT'
)

calcula_rmse_tam <- function(tamano_muestral) {
  
  indices_muestra <- seq_len(nrow(SB11_20111)) %in% sample(seq_len(nrow(SB11_20111)), tamano_muestral)
  muestra <- subset(SB11_20111, subset = indices_muestra, select = variables)
  muestra <- na.omit(muestra)
  
  createFolds(muestra$MATEMATICAS_PUNT, k = n_pliegues) -> pliegues
  
  lapply(
    pliegues,
    rmse_fold,
    MATEMATICAS_PUNT ~ .,
    datos = muestra,
    nn_size = neuronas
  ) -> rmse_pliegues
  
  rmse_pliegues <- unlist(rmse_pliegues)
  mean(rmse_pliegues)
}

## Red neuronal
mclapply(
  tamano_muestral,
  calcula_rmse_tam,
  mc.cores = 1
) -> rmse_por_tam
rmse_por_tam <- unlist(rmse_por_tam)
plot(rmse_por_tam, ylim=c(10, 16))
abline(h = mean(rmse_por_tam), col = 2, lwd = 2)


tamano_muestral <- 5000


indices_muestra <- seq_len(nrow(SB11_20111)) %in% sample(seq_len(nrow(SB11_20111)), tamano_muestral)
muestra <- subset(SB11_20111, subset = indices_muestra, select = variables)
muestra <- na.omit(muestra)

red_neuronal <- nnet(MATEMATICAS_PUNT ~ ., data = muestra, size = neuronas, linout = TRUE)
predict(red_neuronal, newdata = SB11_20111) -> puntaje_pronostico
nuevo_puntaje_mat <- SB11_20111$MATEMATICAS_PUNT - puntaje_pronostico
nuevo_puntaje_mat <- na.omit(nuevo_puntaje_mat)
plot(density(nuevo_puntaje_mat))