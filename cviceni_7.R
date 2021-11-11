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



BreakPointSort <- function(){
  
}
