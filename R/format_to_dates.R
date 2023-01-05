# /* Get event data */
format_to_dates1 <- function(dt, cols, in_place = TRUE){
  dt[, (cols) := lapply(.SD, as.Date ), .SDcols = cols]
  if(in_place == FALSE) return(dt[])
}

format_to_datesWRDS <- function(dt, cols, in_place = TRUE){
  dt[, (cols) := lapply(.SD, function(x){as.Date(as.character(x), format = "%Y%m%d")}), .SDcols = cols]
  if(in_place == FALSE) return(dt[])
}
