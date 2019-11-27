library(e1071)

x_size <- 1000
y_size <- 1000
z_dim <- 8
scale <- 4

x <- ((1:x_size)- 1 - (x_size - 1 )/2)/((x_size -1 )/2) * scale
y <- ((1:y_size)- 1 - (y_size - 1 )/2)/((y_size -1 )/2) * scale
p_x <- rep(x, each = y_size )
p_y <- rep(y,  x_size)
r <- sqrt(p_x^2 + p_y^2)
p_z <- runif(z_dim, min = -1, max=1) * scale
p_z <- rep(p_z, length(r))
## construct the network

H <- nn_connect(matrix(p_z,ncol = z_dim, byrow=T), output_size = 32, std = 1, with_bias = T)$output + 
     nn_connect(matrix(p_x,ncol = 1), output_size = 32, std = 1, with_bias = F)$output + 
     nn_connect(matrix(p_y,ncol = 1), output_size = 32, std = 1, with_bias = F)$output + 
     nn_connect(matrix(r,ncol = 1), output_size = 32, std = 1, with_bias = F)$output 
U <- tanh(H)


for(i in 1:3){
  
  U <- nn_connect(U, output_size = 32, std = 1, with_bias = F)$output
  U <- tanh(U)
}

U <- nn_connect(U, output_size = 3, std = 1, with_bias = F)$output
output <- sigmoid(U)
rm(H)

## plot the image
#output <- rgb(red =  1-output[,1], green = 1-output[,2] , blue = 1-output[,3])
output <- rgb(red =  output[,1], green = output[,2] , blue = output[,3])
my_image <- matrix(output, nrow= y_size,ncol = x_size)
plot(1:max(x_size, y_size), type='n')
rasterImage(my_image , 1, 1,y_size, x_size
            #,interpolate = FALSE
)

