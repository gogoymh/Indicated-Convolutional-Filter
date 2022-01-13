rm(list=ls())
gc(reset=T)


#### Hyper-parameter ####
Wt = 4 # j
Ht = 4 # i

k = 1 # 커널 최소 이용 지수(>=1)

#### search parameter ####
MWa = 7     # 전체 샘플에서 가장 큰 width값
MHa = 8     # 전체 샘플에서 가자 큰 Height값

#### calculate p ####
get_p = function(Wa, Ha){
  
  p1 = (Wt-Wa+k-1)/2
  p2 = (Ht-Ha+k-1)/2
  
  p = ceiling(max(p1, p2, 0))
  
  return(p)
}

#### define indicator function for each channel####
Indicator1 = function(i,j,Ha,Wa,p){
  x = Wa + 2*p + 1 - Wt
  y = Ha + 2*p + 1 - Ht
  if(i <= y & j <= x) answer = 1
  else answer = 0
  return(answer)
}

Indicator2 = function(i,j,Ha,Wa,p){
  x = Wa + 2*p + 1 - Wt
  y = Ha + 2*p + 1 - Ht
  if(i <= y & (MWa+k+1)-x < j) answer = 1
  else answer = 0
  return(answer)
}

Indicator3 = function(i,j,Ha,Wa,p){
  x = Wa + 2*p + 1 - Wt
  y = Ha + 2*p + 1 - Ht
  if((MHa+k+1)-y < i & j <= x) answer = 1
  else answer = 0
  return(answer)
}

Indicator4 = function(i,j,Ha,Wa,p){
  x = Wa + 2*p + 1 - Wt
  y = Ha + 2*p + 1 - Ht
  if((MHa+k+1)-y < i & (MWa+k+1)-x < j) answer = 1
  else answer = 0
  return(answer)
}

#### input matrix ####
Wa = 2 # j
Ha = 2 # i
p = get_p(Wa, Ha)
x = Wa + 2*p + 1 - Wt
y = Ha + 2*p + 1 - Ht

set.seed(2)
input_m = matrix(runif(Wa*Ha), nrow=Ha, ncol=Wa)

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

#### initialize basic weight matrix ####
set.seed(1)
w = matrix(runif((MWa+k+1)*(MHa+k+1)), nrow=(MHa+k+1), ncol=(MWa+k+1)) ## 4개의 채널을 만들기 위해서는 max(Wt)+k에다가 1번씩 더 있어야한다.
print(w)

#### create indicated convolutional filter for each channel ####
v1 = c(); v2 = c(); v3 = c(); v4 = c()
idx = 0
for(j in 1:(MWa+k+1)){
  for(i in 1:(MHa+k+1)){
    idx = idx + 1
    v1[idx] = w[i,j]*Indicator1(i,j,Ha,Wa,p)
    v2[idx] = w[i,j]*Indicator2(i,j,Ha,Wa,p)
    v3[idx] = w[i,j]*Indicator3(i,j,Ha,Wa,p)
    v4[idx] = w[i,j]*Indicator4(i,j,Ha,Wa,p)
  }
}
Iconv1 = matrix(v1, nrow=(MHa+k+1), ncol=(MWa+k+1))
Iconv2 = matrix(v2, nrow=(MHa+k+1), ncol=(MWa+k+1))
Iconv3 = matrix(v3, nrow=(MHa+k+1), ncol=(MWa+k+1))
Iconv4 = matrix(v4, nrow=(MHa+k+1), ncol=(MWa+k+1))

print(w)
print(Iconv1)
print(Iconv2)
print(Iconv3)
print(Iconv4)

#### additional zero padding with k value for each channel ####
add_m1 = matrix(0, nrow=nrow(zero_m), ncol=((Wt+MWa+k)-ncol(zero_m)))
add_m2 = matrix(0, nrow=((Wt+MHa+k)-nrow(zero_m)), ncol=ncol(zero_m))
add_m3 = matrix(0, nrow=((Wt+MHa+k)-nrow(zero_m)), ncol=((Wt+MWa+k)-ncol(zero_m)))

zero_m_a1 = cbind(zero_m, add_m1)
tmp1 = cbind(add_m2, add_m3)
zero_m_a1 = rbind(zero_m_a1, tmp1)

zero_m_a2 = cbind(add_m1, zero_m)
tmp2 = cbind(add_m3, add_m2)
zero_m_a2 = rbind(zero_m_a2, tmp2)

zero_m_a3 = cbind(zero_m, add_m1)
tmp3 = cbind(add_m2, add_m3)
zero_m_a3 = rbind(tmp3, zero_m_a3)

zero_m_a4 = cbind(add_m1, zero_m)
tmp4 = cbind(add_m3, add_m2)
zero_m_a4 = rbind(tmp4, zero_m_a4)

print(zero_m_a1)
print(zero_m_a2)
print(zero_m_a3)
print(zero_m_a4)

#### convolutional operation ####
conv_sum = function(target_m, filter_m, Iconv_col, Iconv_row){
  tmp = 0
  result = 0
  for(j in 1:Iconv_col){
    for(i in 1:Iconv_row){
      tmp = target_m[i,j]*filter_m[i,j]
      result = result + tmp
    }
  }
  return(result)
}

conv_operate = function(zero_m_a, Iconv, Ht, Wt){
  value = c()
  for(j in 1:Wt){
    for(i in 1:Ht){
      value = c(value, conv_sum(zero_m_a[i:(i-1+nrow(Iconv)),j:(j-1+ncol(Iconv))], Iconv, ncol(Iconv), nrow(Iconv1)))
    }
  }
  feature_m = matrix(value, nrow=Ht, ncol=Wt)
  return(feature_m)
}


conv_operate(zero_m_a1, Iconv1, Ht, Wt)
conv_operate(zero_m_a2, Iconv2, Ht, Wt)
conv_operate(zero_m_a3, Iconv3, Ht, Wt)
conv_operate(zero_m_a4, Iconv4, Ht, Wt)
