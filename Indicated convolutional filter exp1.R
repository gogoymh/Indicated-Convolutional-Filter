rm(list=ls())
gc(reset=T)


#### Hyper-parameter ####
Wt = 4
Ht = 4
k = 2


#### calculate p ####
get_p = function(Wa, Ha){
  return(ceiling((Wt-min(Wa,Ha)+k-1)/2))
}

#### define indicator function ####
Indicator = function(i,j,Wa,Ha,p){
  if(j <= (Wa+2*p+1-Wt) && i <= (Ha+2*p+1-Ht)){
    answer = 1
  }
  else answer = 0
  return(answer)
}

#### initialize basic weight matrix ####
set.seed(1)
w = matrix(rnorm((Wt+k-1)*(Ht+k-1)), nrow=(Wt+k-1), ncol=(Ht+k-1))

#### input Wa, Ha ####
Wa = 3 # j
Ha = 4 # i
p = get_p(Wa, Ha)
set.seed(2)
input_m = matrix(rnorm(Wa*Ha), nrow=Ha, ncol=Wa)

#### zero padding to input matrix ####
zero_padding = function(input_m, p){
  zero_m = matrix(0, nrow=(Ha+2*p), ncol=(Wa+2*p))
  for(i in 1:Ha){
    for(j in 1:Wa){
      zero_m[i+p,j+p] = input_m[i,j]
    }
  }
  return(zero_m)
}
zero_m = zero_padding(input_m, p)
print(input_m)
print(zero_m)

#### create indicated convolutional filter ####
v = c()
for(i in 1:(Ht+k-1)){
  for(j in 1:(Wt+k-1)){
    v = c(v,w[i,j]*Indicator(j,i,Wa,Ha,p))
  }
}

Iconv = matrix(v, nrow=(Ht+k-1), ncol=(Wt+k-1))
print(w)
print(Iconv)

#### additional zero padding with k value ####
add_m1 = matrix(0, nrow=nrow(zero_m), ncol=k)
add_m2 = matrix(0, nrow=k, ncol=(ncol(zero_m)+k))

zero_m_a = cbind(zero_m, add_m1)
zero_m_a = rbind(zero_m_a, add_m2)

#### convolutional operation ####
print(zero_m_a)
print(Iconv)

conv_sum = function(Iconv_row, Iconv_col, target_m, filter_m){
  tmp = 0
  result = 0
  for(i in 1:Iconv_row){
    for(j in 1:Iconv_col){
      tmp = target_m[i,j] * filter_m[i,j]
      result = result + tmp
    }
  }
  return(result)
}


conv_filter = function(zero_m_a, Iconv, Wt, Ht){
  value = c()
  for(i in 1:Ht){
    for(j in 1:Wt){
      value = c(value, conv_sum(nrow(Iconv), ncol(Iconv), zero_m_a[i:(i-1+nrow(Iconv)), j:(j-1+ncol(Iconv))], Iconv))
    }
  }
  feature_m = matrix(value, nrow=Wt, ncol=Ht)
  return(feature_m)
}

print(input_m)
print(Iconv)
conv_filter(zero_m_a, Iconv, Wt, Ht)






