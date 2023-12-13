VFVenc = function(A, t, r){
  VF = A*((1+r)**t-1)/r
  return(VF)
}

pagoVFVenc = function(VF, t, r){
  A = r*VF/((1+r)**t-1)
  return(A)
}

tasaVFVenc = function(VF, t, A){
  dif = VF + 1
  i=1
  while (VF/10000 < dif){
    r = i / 1000000
    aprox = A*((1+r)**t-1)/r
    dif = abs(VF-aprox)
    i=i+1
  }
  return (r)
}

periodosVFVenc = function(VF, A, r){
  t = log(1+r*VF/A)/log(1+r)
  return(t)
}

VAVenc = function(A, t, r){
  VA = A*(1-(1+r)**-t)/r
  return(VA)
}

pagoVAVenc = function(VA, t, r){
  A = r*VA/(1-(1+r)**-t)
  return(A)
}

tasaVAVenc = function(VA, t, A){
  dif = VA +1 
  i=1
  while (VA/10000 < dif){
    r = i / 1000000
    aprox = A*(1-(1+r)**-t)/r
    dif = abs(VA-aprox)
    i=i+1
  }
  return (r)
}

periodosVAVenc = function(VA, A, r){
  t = -log(1-r*VA/A)/log(1+r)
  return(t)
}

VFAnt = function(A, t, r){
  VF = (1+r)*A*((1+r)**t-1)/r
  return(VF)
}

pagoVFAnt = function(VF, t, r){
  A = r*VF/(((1+r)**t-1)*(1+r))
  return(A)
}

tasaVFAnt = function(VF, t, A){
  dif = VF + 1
  i=1
  while (VF/10000 < dif){
    r = i / 1000000
    aprox = (1+r)*A*((1+r)**t-1)/r
    dif = abs(VF-aprox)
    i=i+1
  }
  return (r)
}

periodosVFAnt = function(VF, A, r){
  t = log(1+r*VF/(A*(1+r)))/log(1+r)
  return(t)
}

VAAnt = function(A, t, r){
  VA = (1+r)*A*(1-(1+r)**-t)/r
  return(VA)
}

pagoVAAnt = function(VA, t, r){
  A = r*VA/((1-(1+r)**-t)*(1+r))
  return(A)
}

tasaVAAnt = function(VA, t, A){
  dif = VA + 1 
  i=1
  while (VA/10000 < dif){
    r = i / 1000000
    aprox = (1+r)*A*(1-(1+r)**-t)/r
    dif = abs(VA-aprox)
    i=i+1
  }
  return (r)
}

periodosVAAnt = function(VA, A, r){
  t = -log(1-r*VA/(A*(1+r)))/log(1+r)
  return(t)
}

VADif = function(VA, r, G){
  VA = VA*(1+r)**G
  return(VA)
}

pagoVADif = function(VA, t, r, G){
  VA = VADif(VA, r, G)
  A = r*VA/(1-(1+r)**-t)
  return(A)
}

