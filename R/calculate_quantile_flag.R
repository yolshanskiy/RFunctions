calculate_quantile_flag <- function(dt, grp, var, Q, flag_name = "flag",  direction = "greater") {
  # Check if the input is a data.table
  if (!is.data.table(dt)) {
    stop("Input must be a data.table.")
  }
  # Check if the input is a data.table
  if (!is.data.table(dt)) {
    stop("Input must be a data.table.")
  }
  
  # Check if the specified columns exist
  if (!(grp %in% names(dt) && var %in% names(dt))) {
    stop("Specified columns do not exist in the data.table.")
  }
  
  # Calculate quantile by group
  dt[, quantile_val := quantile(get(var), probs = Q), by = grp]
  
  # Assign 1 to observations exceeding quantile, 0 otherwise
  # Assign 1 to observations based on direction
  if (direction == "greater") {
    dt[, (flag_name) := as.integer(get(var) > quantile_val)]
  } else if (direction == "less") {
    dt[, (flag_name) := as.integer(get(var) < quantile_val)]
  } else {
    stop("Invalid direction. Use 'greater' or 'less'.")
  }
  
  # Remove the intermediate quantile_val column
  dt[, quantile_val := NULL]
  
  # Return the modified data.table
  return(dt)
}