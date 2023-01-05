## Winsorize data.table

winsorize_dt <- function(dt, var, lev, na.rm = T){
  qs = quantile(dt[[var]], c(lev, 1 - lev), na.rm = na.rm )
  dt[,(paste0(var,"w")) := get(var)]
  dt[get(var) < qs[1], (paste0(var,"w")) := qs[1]]
  dt[get(var) > qs[2], (paste0(var,"w")) := qs[2]]
}

winsorize_dt_one_sided <- function(dt, var, lev, na.rm = T, lower = TRUE){
  if(lower == FALSE){
    qs = quantile(dt[[var]], c(1 - lev), na.rm = na.rm )
    dt[,(paste0(var,"w")) := get(var)]
    dt[get(var) > qs, (paste0(var,"w")) := qs]
  }
  if(lower == TRUE){
    qs = quantile(dt[[var]], c(lev), na.rm = na.rm )
    dt[,(paste0(var,"w")) := get(var)]
    dt[get(var) < qs, (paste0(var,"w")) := qs]
  }
}

## Winsorize data.table by group

winsorize_dt_grp <- function(dt, var, lev, grp, na.rm = T){
  dt[, by = grp, q1 := quantile(get(var), c(lev), na.rm = na.rm)]
  dt[, by = grp, q2 := quantile(get(var), c(1-lev), na.rm = na.rm)]
  dt[,(paste0(var,"w")) := get(var)]
  dt[get(var) < q1, (paste0(var,"w")) := q1]
  dt[get(var) > q2, (paste0(var,"w")) := q2]
  dt[,`:=`(q1 = NULL, q2 = NULL)]
}

winsorize_dt_one_sided_grp <- function(dt, var, lev, grp, na.rm = T, lower = TRUE){
  if(lower == FALSE){
    dt[, by = grp, q2 := quantile(get(var), c(1-lev), na.rm = na.rm)]
    dt[,(paste0(var,"w")) := get(var)]
    dt[get(var) > q2, (paste0(var,"w")) := q2]
    dt[,`:=`(q2 = NULL)]
  }
  if(lower == TRUE){
    dt[, by = grp, q1 := quantile(get(var), c(lev), na.rm = na.rm)]
    dt[,(paste0(var,"w")) := get(var)]
    dt[get(var) < q1, (paste0(var,"w")) := q1]
    dt[,`:=`(q1 = NULL)]
  }
}

