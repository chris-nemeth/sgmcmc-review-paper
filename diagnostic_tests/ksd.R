#Function to calculate the kernel Stein discrepancy

# Uses the Inverse MultiQuadratic Kernel Stein Discrepancy (IMQ KSD)
# IMQ(x,y) = (c^2 + (x-y)^T(x-y))^-beta (for c in \R, beta in (0,1))
# x (matrix): num_points by d
# gradlogp (matrix): num_points by d
# c (double): parameter of IMQ
# beta (double): parameter of IMQ

# Note that the code is written for clarity and not optimised for speed

imqKSD = function(x,gradlogp,c=1,beta=0.5){
  c2 = c^2
  num_points = nrow(x)
  dim_x = ncol(x)
  imq_ksd_sum = 0
  
  #Calculate KSD
  for(i in 1:num_points){
    for(j in i:num_points){
      x1 = x[i,]
      x2 = x[j,]
      gradlogp1 = gradlogp[i,]
      gradlogp2 = gradlogp[j,]
      
      diff = x1-x2
      diff2 = sum(diff^2)
      
      base = diff2 + c2
      base_beta = base^(-beta)
      base_beta1 = base_beta/base
      
      kterm_sum = sum(gradlogp1*gradlogp2)*base_beta
      coeffgrad = -2.0 * beta * base_beta1
      gradx1term_sum = sum(gradlogp1*(-diff))*coeffgrad
      gradx2term_sum = sum(gradlogp2*diff)*coeffgrad
      gradx1x2term_sum = (-dim_x + 2*(beta+1)*diff2/base)*coeffgrad
      m <- 1 + 1*(i!=j) 
      imq_ksd_sum = imq_ksd_sum + m*(kterm_sum + gradx1term_sum + gradx2term_sum + gradx1x2term_sum)
    }
  }
  imq_ksd = sqrt(imq_ksd_sum)/num_points
  return(imq_ksd)
}
