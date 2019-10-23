debugSource("Estado.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

desc_matriz = matrix(c('*','A','*','0'), 2,2) # inicializa matriz 2x2

inicial <- Estado(desc = desc_matriz)
inicializa.Estado(inicial)
objetivo <- Estado(desc = matrix(c(0),2,2))
inicializa.Estado(final)

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))