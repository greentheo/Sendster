#common functions used often (and override package defaults)

getLItem = function(list,item){
  return(unlist(lapply(list, function(x)return(x[[item]]))))
  
}

tryErr = function(expr){
  res = try(expr)
  return(ifelse(class(res)=="try-error",NA,res))
}