# get groups by quantiles data.table

## set as integers
set_quantiles_var <- function(dt, var, np = 5, grp = NA){
  qvar = paste0("q",var)
  if(!is.na(grp[1])){
    setDT(dt)[, (qvar) := cut(get(var), quantile(get(var), probs = 0:np/np, na.rm = T),
                              labels = FALSE, include.lowest = TRUE), by = grp]
  }else{
    setDT(dt)[, (qvar) := cut(get(var), quantile(get(var), probs = 0:np/np, na.rm = T),
                              labels = FALSE, include.lowest = TRUE)]
  }
}

## set as factors
set_quantiles_varF <- function(dt, var, np = 5, grp = NA){
  qvar = paste0("q",var)
  if(!is.na(grp[1])){
    setDT(dt)[, (qvar) := factor(cut(get(var), quantile(get(var), probs = 0:np/np, na.rm = T),
                                     labels = FALSE, include.lowest = TRUE), levels = 1:np) , by = grp]
  }else{
    setDT(dt)[, (qvar) := factor(cut(get(var), quantile(get(var), probs = 0:np/np, na.rm = T),
                                     labels = FALSE, include.lowest = TRUE), levels = 1:np)]
  }
}

## set quantiles with normalization
set_quantiles_var_norm <- function(dt, var, nor, np = 5, div = TRUE, grp = NA){
  qvar = paste0("q",var,"_n")
  if(!is.na(grp[1])){
    if(div){
      setDT(dt)[, (qvar) := cut(get(var)/get(nor), quantile(get(var)/get(nor), probs = 0:np/np, na.rm = T),
                                         labels = FALSE, include.lowest = TRUE), by = grp]
    }else{
      setDT(dt)[, (qvar) := cut(get(var)*get(nor), quantile(get(var)*get(nor), probs = 0:np/np, na.rm = T),
                                         labels = FALSE, include.lowest = TRUE), by = grp]
    }
  }else{
    if(div){
      setDT(dt)[, (qvar) := cut(get(var)/get(nor), quantile(get(var)/get(nor), probs = 0:np/np, na.rm = T),
                                         labels = FALSE, include.lowest = TRUE)]
    }else{
      setDT(dt)[, (qvar) := cut(get(var)*get(nor), quantile(get(var)*get(nor), probs = 0:np/np, na.rm = T),
                                         labels = FALSE, include.lowest = TRUE)]
    }
  }
}
