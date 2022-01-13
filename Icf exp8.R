rm(list=ls())
gc(reset=T)


#### Hyper Parameter ####
Width_output = 3    # col, j
Height_output = 3   # row, i

kernel_index = 1

#### Pre-searched value ####
Max_of_Width_input = 3 # input의 width 중 최댓값 but test data를 고려하여 정해야한다.
pair_of_MWI =  1 # input Width의 최댓값의 순서쌍 중 최솟값 but test data를 고려하여 1로 하는 것이 좋다

pair_of_MHI = 1 # input Height의 최댓값의 순서쌍 중 최솟값 but test data를 고려하여 1로 하는 것이 좋다
Max_of_Height_input = 3 # input Height의 최댓값 but test data를 고려하여 정해야한다.

#### input matrix ####
Width_input = 2 # j
Height_input = 2 # i

set.seed(2)
input_m = matrix(runif(Width_input*Height_input), nrow=Height_input, ncol=Width_input)
print(input_m)

#### calculate padding ####
get_p = function(Width_input, Height_input){
  
  p1 = (Width_output-Width_input+kernel_index-1)/2
  p2 = (Height_output-Height_input+kernel_index-1)/2
  
  p = ceiling(max(p1, p2, 0))
  
  return(p)
}

padding_for_input = get_p(Width_input, Height_input)

#### get full convolutional filter ####
p_max_x = get_p(Max_of_Width_input, pair_of_MWI)
p_max_y = get_p(pair_of_MHI, Max_of_Height_input)

max_x = Max_of_Width_input + 2*p_max_x - Width_output + 1
max_y = Max_of_Height_input + 2*p_max_y - Height_output + 1

set.seed(1)
w = matrix(runif((max_x+1)*(max_y+1), min = -1,max = 1), nrow=(max_y+1), ncol=(max_x+1)) ## 4개의 채널을 만들기 위해서는 max_x, max_y에다가 1번씩 더 있어야한다.
print(w)

#### define indicator function for each channel####
Indicator1 = function(i,j,Height_input,Width_input,p){
  x = Width_input + 2*p + 1 - Width_output
  y = Height_input + 2*p + 1 - Height_output
  if(i <= y & j <= x) answer = 1
  else answer = 0
  return(answer)
}

Indicator2 = function(i,j,Height_input,Width_input,p){
  x = Width_input + 2*p + 1 - Width_output
  y = Height_input + 2*p + 1 - Height_output
  if(i <= y & (max_x+1)-x < j) answer = 1
  else answer = 0
  return(answer)
}

Indicator3 = function(i,j,Height_input,Width_input,p){
  x = Width_input + 2*p + 1 - Width_output
  y = Height_input + 2*p + 1 - Height_output
  if((max_y+1)-y < i & j <= x) answer = 1
  else answer = 0
  return(answer)
}

Indicator4 = function(i,j,Height_input,Width_input,p){
  x = Width_input + 2*p + 1 - Width_output
  y = Height_input + 2*p + 1 - Height_output
  if((max_y+1)-y < i & (max_x+1)-x < j) answer = 1
  else answer = 0
  return(answer)
}

#### zero padding to input matrix ####
zero_padding = function(input_m, padding){
  zero_m = matrix(0, nrow=(Height_input+2*padding), ncol=(Width_input+2*padding))
  for(i in 1:Height_input){
    for(j in 1:Width_input){
      zero_m[i+padding,j+padding] = input_m[i,j]
    }
  }
  return(zero_m)
}
zero_m = zero_padding(input_m, padding_for_input)
print(input_m)
print(zero_m)

#### create indicated convolutional filter for each channel ####
v1 = c(); v2 = c(); v3 = c(); v4 = c()
idx = 0
for(j in 1:(max_x+1)){
  for(i in 1:(max_y+1)){
    idx = idx + 1
    v1[idx] = w[i,j]*Indicator1(i,j,Height_input,Width_input,padding_for_input)
    v2[idx] = w[i,j]*Indicator2(i,j,Height_input,Width_input,padding_for_input)
    v3[idx] = w[i,j]*Indicator3(i,j,Height_input,Width_input,padding_for_input)
    v4[idx] = w[i,j]*Indicator4(i,j,Height_input,Width_input,padding_for_input)
  }
}
Iconv1 = matrix(v1, nrow=(max_y+1), ncol=(max_x+1))
Iconv2 = matrix(v2, nrow=(max_y+1), ncol=(max_x+1))
Iconv3 = matrix(v3, nrow=(max_y+1), ncol=(max_x+1))
Iconv4 = matrix(v4, nrow=(max_y+1), ncol=(max_x+1))

print(w)
x = Width_input + 2*padding_for_input + 1 - Width_output
y = Height_input + 2*padding_for_input + 1 - Height_output
print(Iconv1)
print(Iconv2)
print(Iconv3)
print(Iconv4)

#### additional zero padding with k value for each channel ####
add_m1 = matrix(0, nrow=nrow(zero_m), ncol=((Width_output+max_x)-ncol(zero_m)))
add_m2 = matrix(0, nrow=((Height_output+max_y)-nrow(zero_m)), ncol=ncol(zero_m))
add_m3 = matrix(0, nrow=((Height_output+max_y)-nrow(zero_m)), ncol=((Width_output+max_x)-ncol(zero_m)))

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

print(zero_m)
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

conv_operate = function(zero_m_a, Iconv, Height_output, Width_output){
  value = c()
  for(j in 1:Width_output){
    for(i in 1:Height_output){
      value = c(value, conv_sum(zero_m_a[i:(i-1+nrow(Iconv)),j:(j-1+ncol(Iconv))], Iconv, ncol(Iconv), nrow(Iconv1)))
    }
  }
  feature_m = matrix(value, nrow=Height_output, ncol=Width_output)
  return(feature_m)
}

print(input_m)
ch1 = conv_operate(zero_m_a1, Iconv1, Height_output, Width_output)
ch2 = conv_operate(zero_m_a2, Iconv2, Height_output, Width_output)
ch3 = conv_operate(zero_m_a3, Iconv3, Height_output, Width_output)
ch4 = conv_operate(zero_m_a4, Iconv4, Height_output, Width_output)

#### Reshaping for fully connected layer ####
fcl = c(ch1, ch2, ch3, ch4)

#### Fully connected layer ####
w2 = runif(36*10)

fcl2 = c()
for(mlp in 1:10){
  fcl2[mlp] = sum(fcl*w2[((mlp-1)*36+1):(mlp*36)])
}

#### L2 norm loss function ####
answer =  c(1,0,0,0,0,0,0,0,0,0)

L2 = sum((fcl2-answer)^2)














