# https://github.com/nebulae-co/saber -------------------------------------

# Paquetes ----------------------------------------------------------------

# install.packages("devtools")
# devtools::install_github("nebulae-co/saber")

library("saber")

# carga de datos ----------------------------------------------------------

data("SB11_20111") # 31707
#data("SB11_20112")

# Intervalos de confianza de la media -------------------------------------
table(SB11_20111$ECON_SN_INTERNET)

tamano_muestral <- 30
iteraciones <- 100

poblac_A <- SB11_20111$FISICA_PUNT[SB11_20111$ECON_SN_INTERNET == 0]
media_Pobla_A <- mean(poblac_A, na.rm=TRUE)

poblac_B <- SB11_20111$FISICA_PUNT[SB11_20111$ECON_SN_INTERNET == 1]
media_Pobla_B <- mean(poblac_B, na.rm=TRUE)

plot(media_Pobla_A, media_Pobla_B, col=4, pch=20)
abline(0, 1)

for(i in seq_len(iteraciones)) {
  muestra <- sample(seq_len(nrow(SB11_20111)), tamano_muestral)
  cuales_A <- muestra %in% seq_len(nrow(SB11_20111)) & SB11_20111$ECON_SN_INTERNET[muestra] == 0
  muestra_A <- SB11_20111$FISICA_PUNT[cuales_A]
  media_muestral_A <- mean(muestra_A, na.rm=TRUE)
  t_test_A <- t.test(muestra_A)
  intervalo_A <- t_test_A$conf.int
  LI_A <- min(intervalo_A)
  LS_A <- max(intervalo_A)
  
  cuales_B <- muestra %in% seq_len(nrow(SB11_20111)) & SB11_20111$ECON_SN_INTERNET[muestra] == 1
  muestra_B <- SB11_20111$FISICA_PUNT[cuales_B]
  media_muestral_B <- mean(muestra_B, na.rm=TRUE)
  t_test_B <- t.test(muestra_B)
  intervalo_B <- t_test_B$conf.int
  LI_B <- min(intervalo_B)
  LS_B <- max(intervalo_B)
  
  rect(LI_A, LI_B, LS_A, LS_B)
}

plot(media_Pobla_A, media_Pobla_B, col=4, pch=20,cex)
