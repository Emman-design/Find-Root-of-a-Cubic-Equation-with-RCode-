How to use is included as comment at the end of the function, all you need to do is copy and paste. 
cbroots<-function() 
{ 
  message(paste('Welcome! we are about to enter the coefficient of your equation of the form ax^3 + bx^2 + cx + d')) 
  a <- readline(prompt='Please Enter a nonzero value of a: ') 
  if(a==0){ message(paste('Warning! HIGHEST DEGREE POLY COEFFICIENT CANNOT BE ZERO')) 
  } 
  else{
    b <- readline(prompt='Please Enter the value of b: ') 
    c <- readline(prompt='Please Enter the value of c: ') 
    d <- readline(prompt='Please Enter the value of d: ')
  }
  
  a <- as.numeric(unlist(strsplit(a, ","))) 
  b <- as.numeric(unlist(strsplit(b, ","))) 
  c <- as.numeric(unlist(strsplit(c, ","))) 
  d <- as.numeric(unlist(strsplit(d, ","))) 
  denom = a 
  a = b/denom 
  b = c/denom 
  c = d/denom 
  pietwo = 2.0*pi 
  piefour = 4.0*pi 
  p = a/3.0 
  q = (3*b-a*a)/(9.0) 
  qw = q*q*q 
  r = (9*a*b-27*c-2*a*a*a)/54.0 
  rw = r*r 
  f = qw +  rw 
  if(f<0.0){ 
    message(paste('Three unequal roots')) 
    theta = acos((r/(sqrt(-1*qw)))) 
    qs = sqrt(-1*q) 
    root1 = 2.0*qs*cos(theta/3.0)-p 
    root2 = 2.0*qs*cos((theta + pietwo)/3.0)-p 
    root3 = 2.0*qs*cos((theta + piefour)/3.0)-p 
  } 
  else 
    if(f>0.0){ 
      message(paste('One real root')) 
      dsq = sqrt(f) 
      s = (r+dsq)^(1/3) 
      t = (r-dsq)^(1/3) 
      root1 = as.numeric(s + t-p) 
      root2 = print('Na') 
      root3 = print('Na') 
    } 
  else 
  { 
    message(paste('Three real roots, at least two equal')) 
    rcb= (r)^(1/3) 
    root1 = 2.0*rcb-p 
    root2 = root3 = rcb-p 
  } 
  list(root1=root1, root2=root2, root3=root3) 
  ####cbroots() 
} 