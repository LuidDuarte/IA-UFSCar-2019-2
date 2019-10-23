debugSource("Aspirador.R")
debugSource("buscaInformada.R")
debugSource("buscaDesinformada.R")


vetor <- c('*','*','*','*')
pos_aspirador <- sample(1:4, 1)
while((pos_limpa <- sample(1:4,1)) == pos_aspirador)
    pos_limpa <- sample(1:4,1)
vetor[pos_aspirador] <- 'A'
vetor[pos_limpa] <- '0'

c('*','0','A','*')
desc_matriz = matrix(vetor, 2,2) # inicializa matriz 2x2
desc_matriz
inicial <- Estado(desc = desc_matriz)
inicializa.Estado(inicial)

objetivo <- Estado(desc = matrix(c(0),2,2))
inicializa.Estado(objetivo)

cat("====\tBusca Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))