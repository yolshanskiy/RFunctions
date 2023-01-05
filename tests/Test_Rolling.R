library(dplyr)
library(data.table)
library(RFunctions)

##### Compare results for different simulations #####

compare_funs <- function(fun_tmp, seed_tmp, no_const = FALSE){
  set.seed(seed_tmp)
  ## simulate data
  x = matrix(rnorm(1000*6), nrow = 1000)
  y = rnorm(1000) + x %*% (1:6)

  ## results from the new function:
  Reg1 = fun_tmp(y = y, x = x, window_size = 50) %>%
    .$coefs %>%
    data.table()
  ## results from the rollRegress function:

  if(no_const){
    Reg2 = data.table(y = y, x = x) %>%
      rollRegres::roll_regres(data = ., y ~ 0 + x.V1 + x.V2 +  x.V3 + x.V4 + x.V5 +  x.V6, width = 50)
  }else{
    Reg2 = data.table(y = y, x = x) %>%
      rollRegres::roll_regres(data = ., y ~ x.V1 + x.V2 +  x.V3 + x.V4 + x.V5 +  x.V6, width = 50)
  }

  Reg2 = data.table(Reg2$coefs) %>%
    .[!is.na(x.V1)] %>% setnames(paste0("V",1:ncol(Reg2$coefs)))

  return(all.equal(Reg1, Reg2))
}

for(seed_tmp in 1:100){
  cat(compare_funs(fun_tmp = RFun2::roll_reg_with_constant, seed_tmp), " ")
}

for(seed_tmp in 1:100){
  cat(compare_funs(fun_tmp = RFunctions::roll_reg_with_constant, seed_tmp), " ")
}

for(seed_tmp in 1:100){
  cat(compare_funs(fun_tmp = RFunctions::roll_reg_with_constant_faster, seed_tmp), " ")
}

for(seed_tmp in 1:100){
  cat(compare_funs(fun_tmp = RFunctions::roll_reg_with_constant_faster_checkCollinear, seed_tmp), " ")
}

for(seed_tmp in 1:100){
  cat(compare_funs(fun_tmp = RFunctions::roll_reg_withNo_constant, seed_tmp, no_const = T), " ")
}


##### Speed Comparison ########
library(microbenchmark)

seed_tmp = 123
set.seed(seed_tmp)
## simulate data

window_size = 250L
x = matrix(rnorm(10000*6), nrow = 10000)
y = rnorm(1000) + x %*% (1:6)
data = data.table()[,y:=y][, ( paste0("x", 1:6)) := split(t(x), 1:6)]

microbenchmark(
  roll_reg_with_constant_faster(y = y, x = x, window_size = window_size) %>%
    .$coefs %>%
    data.table(),
  roll_reg_with_constant_faster_checkCollinear(y = y, x = x, window_size = window_size) %>%
    .$coefs %>%
    data.table(),
  roll_reg_with_constant(y = y, x = x, window_size = window_size) %>%
    .$coefs %>%
    data.table(),
  rollRegres::roll_regres(data = data, y ~ x1 + x2 +  x3 + x4 + x5 +  x6, width = window_size) %>%
    .$coefs,
  times = 100
)

##### Speed Comparison with groups ########
library(microbenchmark)

seed_tmp = 123
set.seed(seed_tmp)
## simulate data

window_size = 250L
TT = 100000
x = matrix(rnorm(TT*6), nrow = TT)
y = rnorm(1000) + x %*% (1:6)
data = data.table()[,y:=y][, ( paste0("x", 1:6)) := split(t(x), 1:6)]
data[,grp := ceiling((1:.N)/.N * 12)]

data.table::setDTthreads(1)

test1 <- function(data){
    data[,by = grp, .SDcols = c(paste0("x", 1:6)),
     {coefs = roll_reg_with_constant_faster(y = y, x = as.matrix(.SD), window_size = window_size)$coefs
     ret =  list()
     for(i in 1:ncol(coefs)) ret[[paste0("x",i)]] = coefs[,i]
     ret
     }]
}
test2 <- function(data){
  data[,by = grp, .SDcols = c(paste0("x", 1:6)),
       {coefs = roll_reg_with_constant_faster_checkCollinear(y = y, x = as.matrix(.SD), window_size = window_size)$coefs
       ret =  list()
       for(i in 1:ncol(coefs)) ret[[paste0("x",i)]] = coefs[,i]
       ret
       }]
}
test_rollRegres <- function(data){
  data[,by = grp, #.SDcols = c(paste0("x", 1:6)),
     {coefs = rollRegres::roll_regres(data = as.data.table(.SD), y ~ x1 + x2 +  x3 + x4 + x5 + x6, width = window_size)$coefs
     ret =  list()
     for(i in 1:ncol(coefs)) ret[[paste0("x",i)]] = coefs[,i]
     ret
     }]  %>% .[!is.na(x1)]
}

dt1 <- test1(data)
dt2 <- test2(data)
dt_RR <- test_rollRegres(data)
all.equal(dt1, dt2)
all.equal(dt1, dt_RR)

Start = Sys.time()
for(i in 1:10){
  test1(data)
}
Sys.time() - Start

library(doParallel)

# register a parallel backend using the number of cores
cl <- makeCluster(1)
registerDoParallel(cl)

# compute the mean of the value column by group
Start = Sys.time()
for(i in 1:10){
  test1(data)
}
Sys.time() - Start

# stop the cluster
stopCluster(cl)

microbenchmark(
  test1(data),
  test2(data),
  test_rollRegres(data),
  times = 40
)

##### Test rolling AR regression: #####

seed_tmp = 123
set.seed(seed_tmp)
## simulate data

nx = 2
x = matrix(rnorm(1000*2), nrow = 1000)
y = rnorm(1000) + x %*% (1:nx)

## test lag creator:
data.table(create_x_lagged(x = y, p = 3, flag_keep_x = TRUE))
data.table(create_x_lagged(x = y, p = 3, flag_keep_x = FALSE))


data = data.table()[,y:=y][, ( paste0("x", 1:nx)) := split(t(x), 1:nx)]
data[, ( paste0("z", 1:3)) := split(t(create_x_lagged(x1, 3, flag_keep_x = FALSE)), 1:3)]

## compare results for RR and our package
dt1 = data[!is.na(z3)][,{roll_reg_with_constant_faster(y = y, x = cbind(z1,z2,z3), window_size = 50)}][,res_var:=NULL]  %>% set_names(c("Intercept", paste0("z", 1:3)))
dt_RR = rollRegres::roll_regres(data = data[!is.na(z3)], y ~ z1 + z2 + z3, width = 50)$coefs %>% as.data.table() %>% .[!is.na(z1)] %>% set_names(c("Intercept", paste0("z", 1:3)))
dt_AR = data[,{roll_AR_with_constant_faster_checkCollinear(y = y, x = x1,  p = 3L, window_size = 50)}][,res_var:=NULL][][!is.na(coefs.V1)]  %>% set_names(c("Intercept", paste0("z", 1:3)))

all.equal(dt1, dt_RR)
all.equal(dt1, dt_AR)


