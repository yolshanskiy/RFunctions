#### library(fixest)
## fixest creates an object that occupies significant space in memory.
## The provided argument "lean" set to TRUE reduces the size of the object but still the size is substantial.
## To deal with it apply to the returned outut save_memory_fixest.

save_memory_fixest <- function(dt, logit = FALSE){
  dt[["linear.predictors"]] = NULL
  dt[["working_residuals"]] = NULL
  dt[["fitted.values"]] = NULL
  dt[["scores"]] = NULL
  dt[["residuals"]] = NULL
  dt[["fixef_id"]] = NULL
  dt[["sumFE"]] = NULL
  dt[["irls_weights"]] = NULL
  dt["y"] = NULL
  dt[["obs_selection"]] = NULL
  dt[["fixef_removed"]] = NULL
  if(logit == T) dt[["family"]]$simulate = NULL
  nms = names(dt$family) 
  if(logit == F){
    for(nm in nms[2:14]){
      dt$family[[nm]] = NULL
    }
  }else{
    for(nm in nms[6:14]){
      dt$family[[nm]] = NULL
    }    
  }
  #dt$family$sum_dev.resids = NULL
  #dt$family$aic = NULL
  dt
}
