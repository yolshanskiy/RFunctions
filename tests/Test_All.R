library(dplyr)
library(data.table)
library(RFunctions)

dt = rnorm(10000) %>%
  matrix(.,ncol = 10) %>%
  as.data.table()


dt[,V7:= fifelse(V7 > 1,"A","B")]
dt[,V8:= fifelse(V8 > 1, "1983-01-02", "2000-01-09")]
dt[,V9:= fifelse(V9 > 1, 20040201, 19651112)]


RFunctions::create_x_lagged(dt$V1,3) %>% as.data.table()
RFunctions::format_to_dates1(dt = dt, cols = "V8")
RFunctions::format_to_datesWRDS(dt = dt, cols = "V9")

## quantiles ##
RFunctions::get_quantiles_var(dt, var = "V1", np = 5)
RFunctions::set_quantiles_var(dt, var = "V1", np = 5)
dt[,by = c("qV1", "V7"),.N][order(V7,qV1)]
RFunctions::set_quantiles_varF(dt, var = "V1", np = 5)
dt[,by = c("qV1", "V7"),.N][order(V7,qV1)]
RFunctions::set_quantiles_var(dt, var = "V1", np = 5, grp = "V7")
dt[,by = c("qV1", "V7"),.N][order(V7,qV1)]

## winsorize ##
winsorize_dt(dt, "V2", 0.1, na.rm = T)
dt[V2!=V2w]

## check that c++ function works in the same way and does not corrupt column
dt[,V9 := copy(V2)]
dt[, V2ww := RFunctions::winsorize_quantiles(x = V2, 10, 90, print_quantiles = T)]
dt[V2w != V2ww]
dt[V9 != V2]
dt[,V2ww := NULL][,V9:=NULL]

## winsorize one side ##
winsorize_dt_one_sided(dt, "V2", 0.1, na.rm = T, lower = FALSE)
dt[V2!=V2w]

## check that c++ function works in the same way and does not corrupt column
dt[,V9 := copy(V2)]
dt[, V2ww := RFunctions::winsorize_one_sided_quantile(x = V2, 10, lower = FALSE)]
dt[, V2ww := winsorize_one_sided_quantile(x = V2, 10, lower = FALSE)]
dt[V2w != V2ww]
dt[V9 != V2]
dt[,V2ww := NULL][,V9:=NULL]

winsorize_dt_grp(dt, "V2", 0.1, na.rm = T, grp = "V8")
dt[V2!=V2w][, by = c("V8", "V2w"), .N]

## select your directory to get answers

subfolders = size_of_subfolders("~/Downloads/")
subfolders[csize > fs_bytes(0.5 * 1e+9)]
subfolders[csize > fs_bytes(0.5 * 1e+9)]

subfolders_withfiles = size_of_subfolders_with_largest_files("~/Downloads/", min_size_file = 1e+8)
subfolders_withfiles = size_of_subfolders_with_largest_files("~/Documents/", min_size_file = 1e+8)
subfolders_withfiles = size_of_subfolders_with_largest_files("~/Desktop/", min_size_file = 1e+8)

subfolders_withfiles[type == "file"]
subfolders_withfiles[depth <= 1]


