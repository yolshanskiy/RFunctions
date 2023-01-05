#
# roll_AR_with_constant_faster_checkCollinear <- function(y, x, p, window_size) {
#   # Call the C++ function
#   result <- roll_AR_with_constant_faster_checkCollinear_RCPP(y, x, p, window_size)  #.Call("roll_AR_with_constant_faster_checkCollinear_RCPP", y, x, p, window_size,
#             #   PACKAGE = "RFunctions")
#   return(result)
# }
#
# roll_reg_with_constant_faster_checkCollinear <- function(y, x, window_size) {
#   # Call the C++ function
#   result <- roll_reg_with_constant_faster_checkCollinear_RCPP(y, x, window_size) #.Call("roll_reg_with_constant_faster_checkCollinear_RCPP", y, x, window_size,
#              #  PACKAGE = "RFunctions")
#   return(result)
# }
#
# roll_reg_with_constant_faster <- function(y, x, window_size) {
#   # Call the C++ function
#   result <- roll_reg_with_constant_faster_RCPP( y, x, window_size) #.Call("roll_reg_with_constant_faster_RCPP", y, x, window_size,
#             #   PACKAGE = "RFunctions")
#   return(result)
# }
#
# roll_reg_with_constant <- function(y, x, window_size) {
#   # Call the C++ function
#   result <- roll_reg_with_constant_RCPP(y, x, window_size) #.Call("roll_reg_with_constant_RCPP", y, x, window_size,
#                #PACKAGE = "RFunctions")
#   return(result)
# }
#
# roll_reg_withNo_constant <- function(y, x, window_size) {
#   # Call the C++ function
#   result <- roll_reg_withNo_constant_RCPP(y, x, window_size)  #.Call("roll_reg_withNo_constant_RCPP", y, x, window_size,
#             #   PACKAGE = "RFunctions")
#   return(result)
# }
#
#
#
# # # Load the Rcpp package
# # library(Rcpp)
# #
# # # Read the C++ file as a character string
# # cpp_code <- readLines("src/roll_reg.cpp")
# #
# # # Extract the lines with the Rcpp::export attribute
# # export_lines <- grep("// \\[\\[Rcpp::export\\]\\]", cpp_code)
# #
# # # Extract the function definitions from the lines
# # functions <- cpp_code[export_lines + 1]
# #
# # # Iterate over the functions and create C++ files
# # for (cpp_function in functions) {
# #   # Extract the name of the function
# #   function_name <- grep("\\w+ \\w+\\(", cpp_function, value = TRUE, perl = TRUE)
# #   function_name <- gsub("\\(.*", "", function_name)
# #   function_name <- gsub("\\w+\\s+", "", function_name)
# #   function_name <- gsub("Rcpp::", "", function_name)
# #
# #   # Extract the headers from the C++ file
# #   headers <- grep("#include", cpp_code, value = TRUE)
# #
# #   # Find the start and end line of the function definition
# #   start_line <- which(cpp_code == cpp_function)
# #   end_line <- start_line + 1
# #   while (end_line <= length(cpp_code) && cpp_code[end_line] != "}") {
# #     end_line <- end_line + 1
# #   }
# #
# #   # Extract the lines of the function definition
# #   function_lines <- cpp_code[start_line:end_line]
# #
# #   # Create a character string containing the function and headers
# #   function_string <- c(headers, function_lines)
# #
# #   # Write the function and headers to a C++ file
# #   # writeLines(function_string, paste0("src/", function_name, ".cpp"))
# #
# #   sourceCpp(paste0("src/",function_name,".cpp"))
# #   function_args <- formals(fun = function_name)
# #
# #   # assign(function_name, Rcpp::cppFunction(paste0(cpp_code, '{
# #   #   // C++ code for ', function_name, ' goes here
# #   # }'), function_args))
# #
# # }
