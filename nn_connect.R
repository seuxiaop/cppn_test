nn_connect <- function(input, output_size = 10, std= 1.0 ,with_bias=T,weight = NULL){
 #input <-as.matrix(p_z, ncol = z_dim)
 # output_size = 10

 if(is.null(weight)){
   weight <- rnorm( dim(input)[2] * output_size , sd = std)
   weight <- matrix(weight, nrow = dim(input)[2])
 }
 
 output <- input %*% weight
 
 if(with_bias){
   bias <- rnorm(output_size, sd = std)
   bias <- matrix(rep(bias,dim(input)[1]), nrow = dim(input)[1], byrow = T)
   output <- output + bias
 }
 
 
 return(list(output= output, weight = weight))
 
}

# 
# nn_connect(matrix(c(1,2,3,4,5,6), nrow = 6)) 
