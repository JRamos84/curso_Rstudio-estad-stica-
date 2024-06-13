

# https://github.com/nebulae-co/saber -------------------------------------




# Paquetes ----------------------------------------------------------------


# install.packages("devtools")
# devtools::install_github("nebulae-co/saber")

library("saber")


# carga de datos ----------------------------------------------------------

data("SB11_20111") # 31707
#data("SB11_20112")



# SB11_20112 %>% names()


iteraciones <- 38
tamano_muestral <- 27

plot(
mean(SB11_20111$MATEMATICAS_PUNT),
sd(SB11_20111$MATEMATICAS_PUNT),
pch = 20,
cex = 4,
col = "white"
)


for(i in seq_len(iteraciones)){
points(
  mean(sample(SB11_20111$MATEMATICAS_PUNT, tamano_muestral)),
  sd(sample(SB11_20111$MATEMATICAS_PUNT, tamano_muestral)),
  pch = 20
  
)
}

points(
  mean(SB11_20111$MATEMATICAS_PUNT),
  sd(SB11_20111$MATEMATICAS_PUNT),
  pch = 20,
  cex = 4,
  col = 2
)



