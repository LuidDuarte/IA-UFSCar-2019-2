debugSource("Canibais.R")
debugSource("buscaInformada.R")

inicial <- Canibais(desc = c(M = 3, C = 3, B = 1))

objetivo <- Canibais()
objetivo$desc <- c(M = 0, C = 0, B = 0)

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))