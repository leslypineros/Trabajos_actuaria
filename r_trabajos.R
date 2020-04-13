#aproximación por suma parcial-----------------------------------------------------------
x = 1:10000000
sum(1/x^2)
#segundo punto-----------------------------------------------------------
i= 1:100000 
n= length(i) #numero de particiones que se usaran 
a= 0 #limite inferior de la integral
b= 1 #limite superior de la integral
delta_x= (b-a)/n
sum(1/n*exp(-(a+(i*(delta_x)))^2)) #integral 

#tercer punto------------------------------------------------------------
primos = function(x){
  vec = 2:x #se crea un vector que inicia desde 2 hasta el numero dado 
  d = 1
  while(vec[d]^2 <= x){
    for(n in vec){
      if(vec[d] != n){
        if(n%%vec[d]==0){ #se usa modulo cero para identificar los multiplos del numero
          vec = vec[-which(vec==n)]#quita los multiplos al numero que se acaba de marcar
        }
      }
    }
    d=d+1
  }
  vec
}
primos(300)

#cuarto punto------------------------------------------------------------
# Mi primer RScripts
#Raices de a*x^2+b*x +c
# --- Limpiar el ambiente de trabajo
rm(list=ls())
require(Rmpfr)
cat("RaÃ???ces de a*x^2+b*x +c.\n")

# Lectura de coeficientes desde el teclado (ENTER)
a = as.numeric(readline("a = "))
b = as.numeric(readline("b = "))
c = as.numeric(readline("c = "))

a = mpfr(a, precBits = 256)
b = mpfr(b, precBits = 256)
c = mpfr(c, precBits = 256)

# Calculos y criterio de parada
if(a==0) stop("No es cuadratica")
# cÃ¡lculo del Discriminante
d = b^2-4*a*c

# ---
if(d==0){
  x1 = -b/(2*a)
  cat("Una raiz real x1 = ", x1)
}
if(d>0){
  x1 = (-b+sqrt(d))/(2*a)
  x2 = (-b-sqrt(d))/(2*a)
  cat("Dos raices reales")
  print(x1)
  print(x2)
}
if(d<0) cat("Las raices son complejas")

#quinto punto-------------------------------------------------- 
options(digits=16);

n=3
#vector b
b=c(1,-1,1)
#matriz A
a=c(1,0,0,0,-5,0,-1,0,1);
A = matrix(a,n, byrow=TRUE);

#Creacion de U, L Y D

print("matriz A")
print(A)
print("vector b")
print(b)
U=-A
D= matrix(0,n,n)
I=matrix(0,n,n)
L=-A
for( i in 1:n){
  D[i,i]=A[i,i]
  U[i,1:i]=0
  L[1:i,i]=0
  I[i,i]=1
}

#calcular la inversa de D
if(det(D)){
  Di=(solve(D,I))
  #Hallar Cj y Tj
  Cj=Di%*%b
  Tj=Di%*%(L+U)
  
  #x inicial
  x=c(0,0,0)
  #iteracion
  N=100
  for(i in 1:N){
    x=(Tj%*%x)+Cj
  }
  #solucion x
  print("solucion")
  print(x)
}else{
  print("no es posible solucionarlo con este metodo")
}

