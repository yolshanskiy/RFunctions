# get quantiles data.table

get_quantiles_var <- function(dt, var, np = 5, grp = NA){
  qvar = paste0("q",var)
  if(!is.na(grp[1])){
    qs = dt[, {qs =  quantile(get(var), probs = 0:np/np, na.rm = T);  ret = list();
    for(i in 1:(np+1))  ret[[paste0("q",i)]] = qs[i];
    ret}, by = grp]
  }else{
    qs = dt[, {qs =  quantile(get(var), probs = 0:np/np, na.rm = T);  ret = list();
    for(i in 1:(np+1))  ret[[paste0("q",i)]] = qs[i];
    ret}]
  }
  return(qs)
}
