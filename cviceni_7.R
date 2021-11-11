indexstart <- function(permutacni_vecktor){
  k = 0;
  for (i in 2 : length(permutacni_vecktor)){
    index = i;
    k = k + i-1;
    if (sum(permutacni_vecktor[1:i]) == k){
      next
    }else{
      break
    }
  }
  return(index)
}
