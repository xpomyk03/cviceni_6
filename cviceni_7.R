NajdiSetridene <- function(permutacni_vektor){
  k = 0;
  for (i in 2 : length(permutacni_vektor)){
    index = i;
    k = k + i-1;
    if (sum(permutacni_vektor[1:i]) == k){
      next
    }else{
      break
    }
  }
  return(index)
}


Vzestupne <- function(permutacni_vektor){
  
  vektor_indikaci <- numeric(length(permutacni_vektor))
  vektor_indikaci[c(1,length(permutacni_vektor))] <- 1
  
  for (i in 2:length(permutacni_vektor)){
    if (permutacni_vektor[i] == (permutacni_vektor[i-1]+1)){
      vektor_indikaci[c(i-1,i)] <- 1
    }else{
      next
    }
  }
  return(vektor_indikaci)
}



BreakPointSort <- function(vektor){
  
  permutacni_vektor <- c(0,vektor,length(vektor)+1)
  index <- NajdiSetridene(permutacni_vektor)
  
  while (index < length(permutacni_vektor)){
    vektor_indikaci <- Vzestupne(permutacni_vektor)
    #doplneni kolize
    if (sum(vektor_indikaci) == length(permutacni_vektor)){
      permutacni_vektor <- c(permutacni_vektor[1:(index-1)],rev(permutacni_vektor[index:(length(permutacni_vektor)-1)]),permutacni_vektor[length(permutacni_vektor)])
    }else{
    nejmensi_sestupny <- min(permutacni_vektor[vektor_indikaci == 0])
    index_nejmensi_sestupny <- which(permutacni_vektor == nejmensi_sestupny)
  
    permutacni_vektor <- c(permutacni_vektor[1:index-1],rev(permutacni_vektor[index:index_nejmensi_sestupny]),permutacni_vektor[(index_nejmensi_sestupny+1):length(permutacni_vektor)])
    
    #prepis stopujiciho indexu
    index <- NajdiSetridene(permutacni_vektor)
    }
  }
  vektor <- permutacni_vektor[2:(length(permutacni_vektor)-1)]
  return(vektor)
}
