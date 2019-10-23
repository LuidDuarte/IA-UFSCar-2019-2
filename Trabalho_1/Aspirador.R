Estado <- function(desc=NULL,num_sujeiras=0, aspirador_pos_x=0, aspirador_pos_y=0){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", NULL, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  assign("num_sujeiras", 0, envir = e)
  assign("aspirador_pos_x", 0, envir = e)
  assign("aspirador_pos_y", 0, envir = e)
  
  class(e) <- c("Estado")
  
  return(e)
}

## Define quantidade de sujeira presente
def_sujeiras.Estado <- function(obj){
  for(celula in obj$desc){
    if(celula == '*'){
      obj$num_sujeiras <- obj$num_sujeiras+1
    }
  }
}

## Retorna posição do aspirador
def_aspirador_pos.Estado <- function(obj){
  for(i in 1:2){
    for(j in 1:2){
      if(obj$desc[i,j] == "A"){
        obj$aspirador_pos_x <- j
        obj$aspirador_pos_y <- i
      }
    }
  }
}

inicializa.Estado <- function(obj){
  def_aspirador_pos.Estado(obj)
  def_sujeiras.Estado(obj)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Estado = function(obj1,obj2){
  if(.Generic == "=="){
    return(obj1$num_sujeiras == obj2$num_sujeiras)
  }
}

## Sobrecarga da função genérica print
print.Estado <- function(obj){
  print(obj$desc)
  cat("Quantidade de sujeiras: ", obj$num_sujeiras, "\n")
  cat("Posicao do Aspirador: [", obj$aspirador_pos_y, ",", obj$aspirador_pos_x, "]\n")
  cat("g: ", obj$g, "\n")
  cat("h: ", obj$h, "\n")
  cat("f: ", obj$f, "\n")
}

## Criação do método genérico "heuristica"
heuristica <- function(atual, ...) {
  UseMethod("heuristica")
}

## Função padrão para o método genérico "heuristica"
## Deve ser implementada para o problema específico
heuristica.default <- function(atual, ...) {
  print("Funcao Generica. Defina a heuristica para o seu problema!\n")
  return(NULL)
}

## Criação do método genérico "geraFilhos"
geraFilhos <- function(obj) {
  UseMethod("geraFilhos")
}

copiaPai <- function(obj){
  copia = Estado(desc=obj$desc)
  copia$num_sujeiras <- obj$num_sujeiras
  copia$aspirador_pos_y <- obj$aspirador_pos_y
  copia$aspirador_pos_x <- obj$aspirador_pos_x

  
  return(copia)
}

geraFilhos.default <- function(obj) {
  filhos <- c()
  if(obj$aspirador_pos_y == 1){ # Descer
    if(obj$desc[1,obj$aspirador_pos_x] == '*'){ # Tem sujeira
      filho <- copiaPai(obj)
      filho$pai <- obj
      filho$desc[2,obj$aspirador_pos_x] <- 'A/*'
      if(obj$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] == 'A/*'){
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '*'
      }
      else{
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '0'
      }
      filho$aspirador_pos_y <- 2
      
      filho$g <- 3
      filho$h <- 2 * obj$num_sujeiras
      filho$f <- filho$g + filho$h

      filhos <- c(filhos, filho)
    }
    else{ # Nao tem sujeira
      filho <- copiaPai(obj)
      filho$pai <- obj
      filho$desc[2,obj$aspirador_pos_x] <- 'A'
      if(obj$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] == 'A/*'){
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '*'
      }
      else{
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '0'
      }
      filho$aspirador_pos_y <- 2
      filho$g <- 3
      filho$h <- (2 * obj$num_sujeiras) + 1
      filho$f <- filho$g + filho$h

      filhos <- c(filhos, filho)
    }
  }
  else{ # Subir
    if(obj$desc[1,obj$aspirador_pos_x] == '*'){ # Tem sujeira
      filho <- copiaPai(obj)
      filho$pai <- obj
      filho$desc[1,obj$aspirador_pos_x] <- 'A/*'
      if(obj$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] == 'A/*'){
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '*'
      }
      else{
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '0'
      }
      filho$aspirador_pos_y <- 1
      filho$g <- 3
      filho$h <- 2 * obj$num_sujeiras
      filho$f <- filho$g + filho$h

      filhos <- c(filhos, filho)
    }
    else{ # Nao tem sujeira
      filho <- copiaPai(obj)
      filho$pai <- obj
      filho$desc[1,obj$aspirador_pos_x] <- 'A'
      if(obj$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] == 'A/*'){
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '*'
      }
      else{
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '0'
      }      
      filho$aspirador_pos_y <- 1
      filho$g <- 3
      filho$h <- (2 * obj$num_sujeiras) + 1
      filho$f <- filho$g + filho$h

      filhos <- c(filhos, filho)
    }
  }
  if(obj$aspirador_pos_x == 1){ # Direita
    if(obj$desc[obj$aspirador_pos_y,2] == '*'){ # Tem sujeira
      filho <- copiaPai(obj)
      filho$pai <- obj
      filho$desc[obj$aspirador_pos_y,2] <- 'A/*'
      if(obj$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] == 'A/*'){
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '*'
      }
      else{
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '0'
      }      
      filho$aspirador_pos_x <- 2
      filho$g <- 1
      filho$h <- 2 * obj$num_sujeiras
      filho$f <- filho$g + filho$h

      filhos <- c(filhos, filho)
    }
    else{ # Nao tem sujeira
      filho <- copiaPai(obj)
      filho$pai <- obj
      filho$desc[obj$aspirador_pos_y,2] <- 'A'
      if(obj$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] == 'A/*'){
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '*'
      }
      else{
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '0'
      }      
      filho$aspirador_pos_x <- 2
      filho$g <- 1
      filho$h <- (2 * obj$num_sujeiras) + 1
      filho$f <- filho$g + filho$h

      filhos <- c(filhos, filho)
    }    
  }
  else{ # Esquerda
    if(obj$desc[obj$aspirador_pos_y,1] == '*'){ # Tem sujeira
      filho <- copiaPai(obj)
      filho$pai <- obj
      filho$desc[obj$aspirador_pos_y,1] <- 'A/*'
      if(obj$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] == 'A/*'){
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '*'
      }
      else{
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '0'
      }     
      filho$aspirador_pos_x <- 1
      filho$g <- 1
      filho$h <- 2 * obj$num_sujeiras
      filho$f <- filho$g + filho$h

      filhos <- c(filhos, filho)
    }
    else{ # Nao tem sujeira
      filho <- copiaPai(obj)
      filho$pai <- obj
      filho$desc[obj$aspirador_pos_y,1] <- 'A'
      if(obj$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] == 'A/*'){
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '*'
      }
      else{
        filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- '0'
      }     
      filho$aspirador_pos_x <- 1
      filho$g <- 1
      filho$h <- (2 * obj$num_sujeiras) + 1
      filho$f <- filho$g + filho$h

      filhos <- c(filhos, filho)
    }    
  }
  if(obj$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] == 'A/*'){
    filho <- copiaPai(obj)
    filho$pai <- obj
      filho$desc[obj$aspirador_pos_y,obj$aspirador_pos_x] <- 'A'
      filho$g <- 2
      filho$f <- filho$g + filho$h
      filho$num_sujeiras <- filho$num_sujeiras -1
      filho$h <- 2 * obj$num_sujeiras
      filhos <- c(filhos, filho)
  }

  return(filhos)
}