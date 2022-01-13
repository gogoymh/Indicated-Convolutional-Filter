rm(list=ls())
gc(reset=T)


p = c()
x = c()
y = c()
k_value = c()
Wi_value = c()
Hi_value = c()
Wo_value = c()
Ho_value = c()

for(k in 1:10){
  for(Wi in 1:10){
    for(Hi in 1:10){
      for(Wo in 1:10){
        for(Ho in 1:10){
          p1 = (Wo - Wi + k - 1)/2
          p2 = (Ho - Hi + k - 1)/2
          
          p_result = ceiling(max(p1, p2, 0))
          
          x_result = Wi + 2*p_result + 1 - Wo
          y_result = Hi + 2*p_result + 1 - Ho
          
          k_value = c(k_value, k)
          p = c(p, p_result)
          x = c(x, x_result)
          y = c(y, y_result)
          Wi_value = c(Wi_value, Wi)
          Hi_value = c(Hi_value, Hi)
          Wo_value = c(Wo_value, Wo)
          Ho_value = c(Ho_value, Ho)
        }
      }
    }
  }
}

aa = data.frame(k_value,p,x,y,Wi_value,Hi_value,Wo_value,Ho_value)

bb = aa[aa$k_value == 1, ]
cc = aa[aa$k_value == 1 & aa$Wi_value == 10, ]



lm = lm(aa$x ~ aa$k_value + aa$Wi_value + aa$Hi_value + aa$Wo_value + aa$Ho_value)
plot(lm)
