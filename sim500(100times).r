

a <- read.csv(file= "C:/Users/Radhey/Documents/dataimp.csv")
#View(a)
b <- a[,c(4:8)]
#View(b)
sigma <- cov(b)
print(sigma)
mean(b$percent_servcices)
mean(b$Unemployment_rate)
mean(b$Life_expectancy)
mean(b$percent_young)
mean(b$percent_old)
mu <- c(52.30,8.93,69.49,28.59,11.08)
colnames(b)
names(mu) <- colnames(b)
print(mu)

##########
library(MASS)
library(yacca)
mobs <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
mobs1 <- data.frame(mobs)
Var1 <- mobs1[,c(1:2)]
Var2 <- mobs1[,c(3:5)]

c1 <- cca(Var1,Var2)

c1$xcrosscorr
##########################333
# multinorm 100 times
library(MASS)
library(yacca)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in 1:100)

{

mobs <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
mobs1 <- data.frame(mobs)
Var1 <- mobs1[,c(1:2)]
Var2 <- mobs1[,c(3:5)]

c1 <- cca(Var1,Var2)

put(c1$xstructcorr[1],c1$xstructcorr[2],c1$xstructcorr[3])
}

t1 <- magic_result_as_dataframe()
#View(t1)
p1 <- mean(t1$`c1$xstructcorr[1]`)
p2 <- mean(t1$`c1$xstructcorr[2]`)
s1 <- sd(t1$`c1$xstructcorr[1]`)
s2 <- sd(t1$`c1$xstructcorr[2]`)


# simulations for categorical 20


library(MASS)
library(yacca)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in 1:100)
  
{
  
  mobs <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs1 <- data.frame(mobs)


k1 <- cut(mobs1$percent_servcices,20,right=FALSE,labels=c(1:20))
k2 <- cut(mobs1$percent_servcices,20,right=FALSE)
r1 <- data.frame(mobs1$percent_servcices,k1,k2)




k3 <- cut(mobs1$Unemployment_rate,20,right=FALSE,labels=c(1:20))
k4 <- cut(mobs1$Unemployment_rate,20,right=FALSE)
r3 <- data.frame(mobs1$Unemployment_rate,k3,k4)



k5 <- cut(mobs1$Life_expectancy,20,right=FALSE,labels=c(1:20))
k6 <- cut(mobs1$Life_expectancy,20,right=FALSE)
r5 <- data.frame(mobs1$Life_expectancy,k5,k6)



k7 <- cut(mobs1$percent_young,20,right=FALSE,labels=c(1:20))
k8 <- cut(mobs1$percent_young,20,right=FALSE)
r7 <- data.frame(mobs1$Life_expectancy,k7,k8)



k9 <- cut(mobs1$percent_old,20,right=FALSE,labels=c(1:20))
k10 <- cut(mobs1$percent_old,20,right=FALSE)
r9 <- data.frame(mobs1$percent_old,k9,k10)



Va1 <- data.frame(as.numeric(r1$k1),as.numeric(r3$k3))
Va2 <- data.frame(as.numeric(r5$k5),as.numeric(r7$k7),as.numeric(r9$k9))


c2 <- cca(Va1,Va2) 
put(c2$xstructcorr[1],c2$xstructcorr[2])

}


t2 <- magic_result_as_dataframe()
p3 <- mean(t2$`c2$xstructcorr[1]`)
p4 <- mean(t2$`c2$xstructcorr[2]`)
s3 <- sd(t2$`c2$xstructcorr[1]`)
s4 <- sd(t2$`c2$xstructcorr[2]`)
# simulations for categorical 19


library(MASS)
library(yacca)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in 1:100)
  
{
  
  mobs <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs1 <- data.frame(mobs)

k11 <- cut(mobs1$percent_servcices,19,right=FALSE,labels=c(1:19))
k12 <- cut(mobs1$percent_servcices,19,right=FALSE)
r11 <- data.frame(mobs1$percent_servcices,k11,k12)



k13 <- cut(mobs1$Unemployment_rate,19,right=FALSE,labels=c(1:19))
k14 <- cut(mobs1$Unemployment_rate,19,right=FALSE)
r13 <- data.frame(mobs1$Unemployment_rate,k13,k14)


k15 <- cut(mobs1$Life_expectancy,19,right=FALSE,labels=c(1:19))
k16 <- cut(mobs1$Life_expectancy,19,right=FALSE)
r15 <- data.frame(mobs1$Life_expectancy,k15,k16)


k17 <- cut(mobs1$percent_young,19,right=FALSE,labels=c(1:19))
k18 <- cut(mobs1$percent_young,19,right=FALSE)
r17 <- data.frame(mobs1$percent_young,k17,k18)


k19 <- cut(mobs1$percent_old,19,right=FALSE,labels=c(1:19))
k20 <- cut(mobs1$percent_old,19,right=FALSE)
r19 <- data.frame(mobs1$percent_old,k19,k20)



Va3 <- data.frame(as.numeric(r11$k11),as.numeric(r13$k13))
Va4 <- data.frame(as.numeric(r15$k15),as.numeric(r17$k17),as.numeric(r19$k19))

c3 <- cca(Va3,Va4) 

put(c3$xstructcorr[1],c3$xstructcorr[2])
}

t3 <- magic_result_as_dataframe()
p5 <- mean(t3$`c3$xstructcorr[1]`)
p6 <- mean(t3$`c3$xstructcorr[2]`)
s5 <- sd(t3$`c3$xstructcorr[1]`)
s6 <- sd(t3$`c3$xstructcorr[2]`)
# simulations for categorical 18
library(MASS)
library(yacca)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in 1:100)
  
{
  
  mobs <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs1 <- data.frame(mobs)




k21 <- cut(mobs1$percent_servcices,18,right=FALSE,labels=c(1:18))
k22 <- cut(mobs1$percent_servcices,18,right=FALSE)
r21 <- data.frame(mobs1$percent_servcices,k21,k22)



k23 <- cut(mobs1$Unemployment_rate,18,right=FALSE,labels=c(1:18))
k24 <- cut(mobs1$Unemployment_rate,18,right=FALSE)
r23 <- data.frame(mobs1$Unemployment_rate,k23,k24)


k25 <- cut(mobs1$Life_expectancy,18,right=FALSE,labels=c(1:18))
k26 <- cut(mobs1$Life_expectancy,18,right=FALSE)
r25 <- data.frame(mobs1$Life_expectancy,k25,k26)


k27 <- cut(mobs1$percent_young,18,right=FALSE,labels=c(1:18))
k28 <- cut(mobs1$percent_young,18,right=FALSE)
r27 <- data.frame(mobs1$percent_young,k27,k28)


k29 <- cut(mobs1$percent_old,18,right=FALSE,labels=c(1:18))
k30 <- cut(mobs1$percent_old,18,right=FALSE)
r29 <- data.frame(mobs1$percent_old,k29,k30)


Va5 <- data.frame(as.numeric(r21$k21),as.numeric(r23$k23))
Va6 <- data.frame(as.numeric(r25$k25),as.numeric(r27$k27),as.numeric(r29$k29))

c4 <- cca(Va5,Va6) 

put(c4$xstructcorr[1],c4$xstructcorr[2])
}

t4 <- magic_result_as_dataframe()
p7 <- mean(t4$`c4$xstructcorr[1]`)
p8 <- mean(t4$`c4$xstructcorr[2]`)
s7 <- sd(t4$`c4$xstructcorr[1]`)
s8 <- sd(t4$`c4$xstructcorr[2]`)



# simulations for categorical 17

library(MASS)
library(yacca)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in 1:100)
  
{
  
  mobs <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs1 <- data.frame(mobs)
  
  

k31 <- cut(mobs1$percent_servcices,17,right=FALSE,labels=c(1:17))
k32 <- cut(mobs1$percent_servcices,17,right=FALSE)
r31 <- data.frame(mobs1$percent_servcices,k31,k32)



k33 <- cut(mobs1$Unemployment_rate,17,right=FALSE,labels=c(1:17))
k34 <- cut(mobs1$Unemployment_rate,17,right=FALSE)
r33 <- data.frame(mobs1$Unemployment_rate,k33,k34)


k35 <- cut(mobs1$Life_expectancy,17,right=FALSE,labels=c(1:17))
k36 <- cut(mobs1$Life_expectancy,17,right=FALSE)
r35 <- data.frame(mobs1$Life_expectancy,k35,k36)

k37 <- cut(mobs1$percent_young,17,right=FALSE,labels=c(1:17))
k38 <- cut(mobs1$percent_young,17,right=FALSE)
r37 <- data.frame(mobs1$percent_young,k37,k38)

k39 <- cut(mobs1$percent_old,17,right=FALSE,labels=c(1:17))
k40 <- cut(mobs1$percent_old,17,right=FALSE)
r39 <- data.frame(mobs1$percent_old,k39,k40)




Va7 <- data.frame(as.numeric(r31$k31),as.numeric(r33$k33))
Va8 <- data.frame(as.numeric(r35$k35),as.numeric(r37$k37),as.numeric(r39$k39))

c5 <- cca(Va7,Va8) 

put(c5$xstructcorr[1],c5$xstructcorr[2])
}

t5 <- magic_result_as_dataframe()
p9 <- mean(t5$`c5$xstructcorr[1]`)
p10 <- mean(t5$`c5$xstructcorr[2]`)
s9 <- sd(t5$`c5$xstructcorr[1]`)
s10 <- sd(t5$`c5$xstructcorr[2]`)


# simulations for categorical 16

library(MASS)
library(yacca)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs2 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs3 <- data.frame(mobs2)


k41 <- cut(mobs3$percent_servcices,16,right=FALSE,labels=c(1:16))
k42 <- cut(mobs3$percent_servcices,16,right=FALSE)
r41 <- data.frame(mobs3$percent_servcices,k41,k42)



k43 <- cut(mobs3$Unemployment_rate,16,right=FALSE,labels=c(1:16))
k44 <- cut(mobs3$Unemployment_rate,16,right=FALSE)
r43 <- data.frame(mobs3$Unemployment_rate,k43,k44)


k45 <- cut(mobs3$Life_expectancy,16,right=FALSE,labels=c(1:16))
k46 <- cut(mobs3$Life_expectancy,16,right=FALSE)
r45 <- data.frame(mobs3$Life_expectancy,k45,k46)

k47 <- cut(mobs3$percent_young,16,right=FALSE,labels=c(1:16))
k48 <- cut(mobs3$percent_young,16,right=FALSE)
r47 <- data.frame(mobs3$percent_young,k47,k48)

k49 <- cut(mobs3$percent_old,16,right=FALSE,labels=c(1:16))
k50 <- cut(mobs3$percent_old,16,right=FALSE)
r49 <- data.frame(mobs3$percent_old,k49,k50)

Va9 <- data.frame(as.numeric(r41$k41),as.numeric(r43$k43))
Va10 <- data.frame(as.numeric(r45$k45),as.numeric(r47$k47),as.numeric(r49$k49))

c6 <- cca(Va9,Va10) 
put(c6$xstructcorr[1],c6$xstructcorr[2])


}


t6 <- magic_result_as_dataframe()
p11<- mean(t6$`c6$xstructcorr[1]`)
p12 <- mean(t6$`c6$xstructcorr[2]`)
s11 <- sd(t6$`c6$xstructcorr[1]`)
s12 <- sd(t6$`c6$xstructcorr[2]`)


# simulations for categorical 15
library(MASS)
library(yacca)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs4 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs5 <- data.frame(mobs4)
  

k51 <- cut(mobs5$percent_servcices,15,right=FALSE,labels=c(1:15))
k52 <- cut(mobs5$percent_servcices,15,right=FALSE)
r51 <- data.frame(mobs5$percent_servcices,k51,k52)



k53 <- cut(mobs5$Unemployment_rate,15,right=FALSE,labels=c(1:15))
k54 <- cut(mobs5$Unemployment_rate,15,right=FALSE)
r53 <- data.frame(mobs5$Unemployment_rate,k53,k54)


k55 <- cut(mobs5$Life_expectancy,15,right=FALSE,labels=c(1:15))
k56 <- cut(mobs5$Life_expectancy,15,right=FALSE)
r55 <- data.frame(mobs5$Life_expectancy,k55,k56)

k57 <- cut(mobs5$percent_young,15,right=FALSE,labels=c(1:15))
k58 <- cut(mobs5$percent_young,15,right=FALSE)
r57 <- data.frame(mobs5$percent_young,k57,k58)

k59 <- cut(mobs5$percent_old,15,right=FALSE,labels=c(1:15))
k60 <- cut(mobs5$percent_old,15,right=FALSE)
r59 <- data.frame(mobs5$percent_old,k59,k60)


Va11 <- data.frame(as.numeric(r51$k51),as.numeric(r53$k53))
Va12 <- data.frame(as.numeric(r55$k55),as.numeric(r57$k57),as.numeric(r59$k59))

c7 <- cca(Va11,Va12) 
put(c7$xstructcorr[1],c7$xstructcorr[2])
}
t7 <- magic_result_as_dataframe()
p13<- mean(t7$`c7$xstructcorr[1]`)
p14 <- mean(t7$`c7$xstructcorr[2]`)
s13 <- sd(t7$`c7$xstructcorr[1]`)
s14 <- sd(t7$`c7$xstructcorr[2]`)
# simulations for categorical 14

library(MASS)
library(yacca)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs4 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs5 <- data.frame(mobs4)

k61 <- cut(mobs5$percent_servcices,14,right=FALSE,labels=c(1:14))
k62 <- cut(mobs5$percent_servcices,14,right=FALSE)
r61 <- data.frame(mobs5$percent_servcices,k61,k62)


k63 <- cut(mobs5$Unemployment_rate,14,right=FALSE,labels=c(1:14))
k64 <- cut(mobs5$Unemployment_rate,14,right=FALSE)
r63 <- data.frame(mobs5$Unemployment_rate,k63,k64)


k65 <- cut(mobs5$Life_expectancy,14,right=FALSE,labels=c(1:14))
k66 <- cut(mobs5$Life_expectancy,14,right=FALSE)
r65 <- data.frame(mobs5$Life_expectancy,k65,k66)

k67 <- cut(mobs5$percent_young,14,right=FALSE,labels=c(1:14))
k68 <- cut(mobs5$percent_young,14,right=FALSE)
r67 <- data.frame(mobs5$percent_young,k67,k68)

k69 <- cut(mobs5$percent_old,14,right=FALSE,labels=c(1:14))
k70 <- cut(mobs5$percent_old,14,right=FALSE)
r69 <- data.frame(mobs5$percent_old,k69,k70)



Va13 <- data.frame(as.numeric(r61$k61),as.numeric(r63$k63))
Va14 <- data.frame(as.numeric(r65$k65),as.numeric(r67$k67),as.numeric(r69$k69))

c8<- cca(Va13,Va14) 
put(c8$xstructcorr[1],c8$xstructcorr[2])

}
t8 <- magic_result_as_dataframe()
p15<- mean(t8$`c8$xstructcorr[1]`)
p16 <- mean(t8$`c8$xstructcorr[2]`)
s15 <- sd(t8$`c8$xstructcorr[1]`)
s16 <- sd(t8$`c8$xstructcorr[2]`)



# simulations for categorical 13
library(MASS)
library(yacca)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs4 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs5 <- data.frame(mobs4)
k71 <- cut(mobs5$percent_servcices,13,right=FALSE,labels=c(1:13))
k72 <- cut(mobs5$percent_servcices,13,right=FALSE)
r71 <- data.frame(mobs5$percent_servcices,k71,k72)


k73 <- cut(mobs5$Unemployment_rate,13,right=FALSE,labels=c(1:13))
k74 <- cut(mobs5$Unemployment_rate,13,right=FALSE)
r73 <- data.frame(mobs5$Unemployment_rate,k73,k74)


k75 <- cut(mobs5$Life_expectancy,13,right=FALSE,labels=c(1:13))
k76 <- cut(mobs5$Life_expectancy,13,right=FALSE)
r75 <- data.frame(mobs5$Life_expectancy,k75,k76)

k77 <- cut(mobs5$percent_young,13,right=FALSE,labels=c(1:13))
k78 <- cut(mobs5$percent_young,13,right=FALSE)
r77 <- data.frame(mobs5$percent_young,k77,k78)

k79 <- cut(mobs5$percent_old,13,right=FALSE,labels=c(1:13))
k80 <- cut(mobs5$percent_old,13,right=FALSE)
r79 <- data.frame(mobs5$percent_old,k79,k80)


Va15 <- data.frame(as.numeric(r71$k71),as.numeric(r73$k73))
Va16 <- data.frame(as.numeric(r75$k75),as.numeric(r77$k77),as.numeric(r79$k79))

c9<- cca(Va15,Va16)
put(c9$xstructcorr[1],c9$xstructcorr[2])

}
t9 <- magic_result_as_dataframe()

p17<- mean(t9$`c9$xstructcorr[1]`)
p18 <- mean(t9$`c9$xstructcorr[2]`)
s17 <- sd(t9$`c9$xstructcorr[1]`)
s18 <- sd(t9$`c9$xstructcorr[2]`)
# simulations for categorical 12

library(MASS)
library(CCA)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs4 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs5 <- data.frame(mobs4)
k81 <- cut(mobs5$percent_servcices,12,right=FALSE,labels=c(1:12))
k82 <- cut(mobs5$percent_servcices,12,right=FALSE)
r81 <- data.frame(mobs5$percent_servcices,k81,k82)



k83 <- cut(mobs5$Unemployment_rate,12,right=FALSE,labels=c(1:12))
k84 <- cut(mobs5$Unemployment_rate,12,right=FALSE)
r83 <- data.frame(mobs5$Unemployment_rate,k83,k84)


k85 <- cut(mobs5$Life_expectancy,12,right=FALSE,labels=c(1:12))
k86 <- cut(mobs5$Life_expectancy,12,right=FALSE)
r85 <- data.frame(mobs5$Life_expectancy,k85,k86)

k87 <- cut(mobs5$percent_young,12,right=FALSE,labels=c(1:12))
k88 <- cut(mobs5$percent_young,12,right=FALSE)
r87 <- data.frame(mobs5$percent_young,k87,k88)

k89 <- cut(mobs5$percent_old,12,right=FALSE,labels=c(1:12))
k90 <- cut(mobs5$percent_old,12,right=FALSE)
r89 <- data.frame(mobs5$percent_old,k89,k90)


Va17 <- data.frame(as.numeric(r81$k81),as.numeric(r83$k83))
Va18 <- data.frame(as.numeric(r85$k85),as.numeric(r87$k87),as.numeric(r89$k89))

c10<- cca(Va17,Va18) 
put(c10$xstructcorr[1],c10$xstructcorr[2])

}
t10 <- magic_result_as_dataframe()
p19<- mean(t10$`c10$xstructcorr[1]`)
p20 <- mean(t10$`c10$xstructcorr[2]`)
s19 <- sd(t10$`c10$xstructcorr[1]`)
s20 <- sd(t10$`c10$xstructcorr[2]`)

# simulations for categorical 11
library(MASS)
library(CCA)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs4 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs5 <- data.frame(mobs4)
k91 <- cut(mobs5$percent_servcices,11,right=FALSE,labels=c(1:11))
k92 <- cut(mobs5$percent_servcices,11,right=FALSE)
r91 <- data.frame(mobs5$percent_servcices,k91,k92)



k93 <- cut(mobs5$Unemployment_rate,11,right=FALSE,labels=c(1:11))
k94 <- cut(mobs5$Unemployment_rate,11,right=FALSE)
r93 <- data.frame(mobs5$Unemployment_rate,k93,k94)


k95 <- cut(mobs5$Life_expectancy,11,right=FALSE,labels=c(1:11))
k96 <- cut(mobs5$Life_expectancy,11,right=FALSE)
r95 <- data.frame(mobs5$Life_expectancy,k95,k96)

k97 <- cut(mobs5$percent_young,11,right=FALSE,labels=c(1:11))
k98 <- cut(mobs5$percent_young,11,right=FALSE)
r97 <- data.frame(mobs5$percent_young,k97,k98)

k99 <- cut(mobs5$percent_old,11,right=FALSE,labels=c(1:11))
k100 <- cut(mobs5$percent_old,11,right=FALSE)
r99 <- data.frame(mobs5$percent_old,k99,k100)

Va19 <- data.frame(as.numeric(r91$k91),as.numeric(r93$k93))
Va20 <- data.frame(as.numeric(r95$k95),as.numeric(r97$k97),as.numeric(r99$k99))

c11<- cca(Va19,Va20) 
put(c11$xstructcorr[1],c11$xstructcorr[2])

}
t11 <- magic_result_as_dataframe()
p21<- mean(t11$`c11$xstructcorr[1]`)
p22 <- mean(t11$`c11$xstructcorr[2]`)
s21 <- sd(t11$`c11$xstructcorr[1]`)
s22 <- sd(t11$`c11$xstructcorr[2]`)

# simulations for categorical 10

library(MASS)
library(CCA)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs4 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs5 <- data.frame(mobs4)


g1 <- cut(mobs5$percent_servcices,10,right=FALSE,labels=c(1:10))
g2 <- cut(mobs5$percent_servcices,10,right=FALSE)
h1 <- data.frame(mobs5$percent_servcices,g1,g2)




g3 <- cut(mobs5$Unemployment_rate,10,right=FALSE,labels=c(1:10))
g4 <- cut(mobs5$Unemployment_rate,10,right=FALSE)
h3 <- data.frame(mobs5$Unemployment_rate,g3,g4)



g5 <- cut(mobs5$Life_expectancy,10,right=FALSE,labels=c(1:10))
g6 <- cut(mobs5$Life_expectancy,10,right=FALSE)
h5 <- data.frame(mobs5$Life_expectancy,g5,g6)



g7 <- cut(mobs5$percent_young,10,right=FALSE,labels=c(1:10))
g8 <- cut(mobs5$percent_young,10,right=FALSE)
h7 <- data.frame(mobs5$Life_expectancy,g7,g8)



g9 <- cut(mobs5$percent_old,10,right=FALSE,labels=c(1:10))
g10 <- cut(mobs5$percent_old,10,right=FALSE)
h9 <- data.frame(mobs5$percent_old,g9,g10)

v1 <- data.frame(as.numeric(h1$g1),as.numeric(h3$g3))
v2 <- data.frame(as.numeric(h5$g5),as.numeric(h7$g7),as.numeric(h9$g9))

c12 <- cca(v1,v2) 
put(c12$xstructcorr[1],c12$xstructcorr[2])



}
t12 <- magic_result_as_dataframe()
p23<- mean(t12$`c12$xstructcorr[1]`)
p24 <- mean(t12$`c12$xstructcorr[2]`)
s23 <- sd(t12$`c12$xstructcorr[1]`)
s24 <- sd(t12$`c12$xstructcorr[2]`)
# simulations for categorical 9
library(MASS)
library(CCA)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs6 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs7 <- data.frame(mobs6)
  

g11 <- cut(mobs7$percent_servcices,9,right=FALSE,labels=c(1:9))
g12 <- cut(mobs7$percent_servcices,9,right=FALSE)
h11 <- data.frame(mobs7$percent_servcices,g11,g12)




g13 <- cut(mobs7$Unemployment_rate,9,right=FALSE,labels=c(1:9))
g14 <- cut(mobs7$Unemployment_rate,9,right=FALSE)
h13 <- data.frame(mobs7$Unemployment_rate,g13,g14)



g15 <- cut(mobs7$Life_expectancy,9,right=FALSE,labels=c(1:9))
g16 <- cut(mobs7$Life_expectancy,9,right=FALSE)
h15 <- data.frame(mobs7$Life_expectancy,g15,g16)


g17 <- cut(mobs7$percent_young,9,right=FALSE,labels=c(1:9))
g18 <- cut(mobs7$percent_young,9,right=FALSE)
h17 <- data.frame(mobs7$Life_expectancy,g17,g18)



g19 <- cut(mobs7$percent_old,9,right=FALSE,labels=c(1:9))
g20 <- cut(mobs7$percent_old,9,right=FALSE)
h19 <- data.frame(mobs7$percent_old,g19,g20)


v3 <- data.frame(as.numeric(h11$g11),as.numeric(h13$g13))
v4 <- data.frame(as.numeric(h15$g15),as.numeric(h17$g17),as.numeric(h19$g19))

c13 <- cca(v3,v4) 
put(c13$xstructcorr[1],c13$xstructcorr[2])
}
t13 <- magic_result_as_dataframe()
p25<- mean(t13$`c13$xstructcorr[1]`)
p26<- mean(t13$`c13$xstructcorr[2]`)
s25 <- sd(t13$`c13$xstructcorr[1]`)
s26 <- sd(t13$`c13$xstructcorr[2]`)

# simulations for categorical 8

library(MASS)
library(CCA)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs6 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs7 <- data.frame(mobs6)
  


g21 <- cut(mobs7$percent_servcices,8,right=FALSE,labels=c(1:8))
g22 <- cut(mobs7$percent_servcices,8,right=FALSE)
h21 <- data.frame(mobs7$percent_servcices,g21,g22)




g23 <- cut(mobs7$Unemployment_rate,8,right=FALSE,labels=c(1:8))
g24 <- cut(mobs7$Unemployment_rate,8,right=FALSE)
h23 <- data.frame(mobs7$Unemployment_rate,g23,g24)


g25 <- cut(mobs7$Life_expectancy,8,right=FALSE,labels=c(1:8))
g26 <- cut(mobs7$Life_expectancy,8,right=FALSE)
h25 <- data.frame(mobs7$Life_expectancy,g25,g26)



g27 <- cut(mobs7$percent_young,8,right=FALSE,labels=c(1:8))
g28 <- cut(mobs7$percent_young,8,right=FALSE)
h27 <- data.frame(mobs7$Life_expectancy,g27,g28)



g29 <- cut(mobs7$percent_old,8,right=FALSE,labels=c(1:8))
g30 <- cut(mobs7$percent_old,8,right=FALSE)
h29 <- data.frame(mobs7$percent_old,g29,g30)


v5 <- data.frame(as.numeric(h21$g21),as.numeric(h23$g23))
v6 <- data.frame(as.numeric(h25$g25),as.numeric(h27$g27),as.numeric(h29$g29))

c14 <- cca(v5,v6) 
put(c14$xstructcorr[1],c14$xstructcorr[2])

}
t14 <- magic_result_as_dataframe()
p27<- mean(t14$`c14$xstructcorr[1]`)
p28<- mean(t14$`c14$xstructcorr[2]`)
s27 <- sd(t14$`c14$xstructcorr[1]`)
s28 <- sd(t14$`c14$xstructcorr[2]`)
# simulations for categorical 7
library(MASS)
library(CCA)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs6 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs7 <- data.frame(mobs6)
  

g31 <- cut(mobs7$percent_servcices,7,right=FALSE,labels=c(1:7))
g32 <- cut(mobs7$percent_servcices,7,right=FALSE)
h31 <- data.frame(mobs7$percent_servcices,g31,g32)




g33 <- cut(mobs7$Unemployment_rate,7,right=FALSE,labels=c(1:7))
g34 <- cut(mobs7$Unemployment_rate,7,right=FALSE)
h33 <- data.frame(mobs7$Unemployment_rate,g33,g34)



g35 <- cut(mobs7$Life_expectancy,7,right=FALSE,labels=c(1:7))
g36 <- cut(mobs7$Life_expectancy,7,right=FALSE)
h35 <- data.frame(mobs7$Life_expectancy,g35,g36)



g37 <- cut(mobs7$percent_young,7,right=FALSE,labels=c(1:7))
g38 <- cut(mobs7$percent_young,7,right=FALSE)
h37 <- data.frame(mobs7$Life_expectancy,g37,g38)



g39 <- cut(mobs7$percent_old,7,right=FALSE,labels=c(1:7))
g40 <- cut(mobs7$percent_old,7,right=FALSE)
h39 <- data.frame(mobs7$percent_old,g39,g40)


v7 <- data.frame(as.numeric(h31$g31),as.numeric(h33$g33))
v8 <- data.frame(as.numeric(h35$g35),as.numeric(h37$g37),as.numeric(h39$g39))

c15 <- cca(v7,v8) 
put(c15$xstructcorr[1],c15$xstructcorr[2])
}
t15 <- magic_result_as_dataframe()
p29<- mean(t15$`c15$xstructcorr[1]`)
p30<- mean(t15$`c15$xstructcorr[2]`)
s29 <- sd(t15$`c15$xstructcorr[1]`)
s30 <- sd(t15$`c15$xstructcorr[2]`)
# simulations for categorical 6

library(MASS)
library(CCA)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs6 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs7 <- data.frame(mobs6)
  


g41 <- cut(mobs7$percent_servcices,6,right=FALSE,labels=c(1:6))
g42 <- cut(mobs7$percent_servcices,6,right=FALSE)
h41 <- data.frame(mobs7$percent_servcices,g41,g42)




g43 <- cut(mobs7$Unemployment_rate,6,right=FALSE,labels=c(1:6))
g44 <- cut(mobs7$Unemployment_rate,6,right=FALSE)
h43 <- data.frame(mobs7$Unemployment_rate,g43,g44)



g45 <- cut(mobs7$Life_expectancy,6,right=FALSE,labels=c(1:6))
g46 <- cut(mobs7$Life_expectancy,6,right=FALSE)
h45 <- data.frame(mobs7$Life_expectancy,g45,g46)



g47 <- cut(mobs7$percent_young,6,right=FALSE,labels=c(1:6))
g48 <- cut(mobs7$percent_young,6,right=FALSE)
h47 <- data.frame(mobs7$Life_expectancy,g47,g48)



g49 <- cut(mobs7$percent_old,6,right=FALSE,labels=c(1:6))
g50 <- cut(mobs7$percent_old,6,right=FALSE)
h49 <- data.frame(mobs7$percent_old,g49,g50)


v9 <- data.frame(as.numeric(h41$g41),as.numeric(h43$g43))
v10 <- data.frame(as.numeric(h45$g45),as.numeric(h47$g47),as.numeric(h49$g49))

c16 <- cca(v9,v10) 
put(c16$xstructcorr[1],c16$xstructcorr[2])

}
t16 <- magic_result_as_dataframe()
p31<- mean(t16$`c16$xstructcorr[1]`)
p32<- mean(t16$`c16$xstructcorr[2]`)
s31 <- sd(t16$`c16$xstructcorr[1]`)
s32 <- sd(t16$`c16$xstructcorr[2]`)
# simulations for categorical 5
library(MASS)
library(CCA)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs8 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs9 <- data.frame(mobs8)
  
g51 <- cut(mobs9$percent_servcices,5,right=FALSE,labels=c(1:5))
g52 <- cut(mobs9$percent_servcices,5,right=FALSE)
h51 <- data.frame(mobs9$percent_servcices,g51,g52)




g53 <- cut(mobs9$Unemployment_rate,5,right=FALSE,labels=c(1:5))
g54 <- cut(mobs9$Unemployment_rate,5,right=FALSE)
h53 <- data.frame(mobs9$Unemployment_rate,g53,g54)



g55 <- cut(mobs9$Life_expectancy,5,right=FALSE,labels=c(1:5))
g56 <- cut(mobs9$Life_expectancy,5,right=FALSE)
h55 <- data.frame(mobs9$Life_expectancy,g55,g56)



g57 <- cut(mobs9$percent_young,5,right=FALSE,labels=c(1:5))
g58 <- cut(mobs9$percent_young,5,right=FALSE)
h57 <- data.frame(mobs9$Life_expectancy,g57,g58)



g59 <- cut(mobs9$percent_old,5,right=FALSE,labels=c(1:5))
g60 <- cut(mobs9$percent_old,5,right=FALSE)
h59 <- data.frame(mobs9$percent_old,g59,g60)


v11 <- data.frame(as.numeric(h51$g51),as.numeric(h53$g53))
v12 <- data.frame(as.numeric(h55$g55),as.numeric(h57$g57),as.numeric(h59$g59))

c17 <- cca(v11,v12) 
put(c17$xstructcorr[1],c17$xstructcorr[2])
}
t17 <- magic_result_as_dataframe()
p33<- mean(t17$`c17$xstructcorr[1]`)
p34<- mean(t17$`c17$xstructcorr[2]`)
s33 <- sd(t17$`c17$xstructcorr[1]`)
s34 <- sd(t17$`c17$xstructcorr[2]`)

# simulations for categorical 4
library(MASS)
library(CCA)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs8 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs9 <- data.frame(mobs8)

g61 <- cut(mobs9$percent_servcices,4,right=FALSE,labels=c(1:4))
g62 <- cut(mobs9$percent_servcices,4,right=FALSE)
h61 <- data.frame(mobs9$percent_servcices,g61,g62)




g63 <- cut(mobs9$Unemployment_rate,4,right=FALSE,labels=c(1:4))
g64 <- cut(mobs9$Unemployment_rate,4,right=FALSE)
h63 <- data.frame(mobs9$Unemployment_rate,g63,g64)



g65 <- cut(mobs9$Life_expectancy,4,right=FALSE,labels=c(1:4))
g66 <- cut(mobs9$Life_expectancy,4,right=FALSE)
h65 <- data.frame(mobs9$Life_expectancy,g65,g66)



g67 <- cut(mobs9$percent_young,4,right=FALSE,labels=c(1:4))
g68 <- cut(mobs9$percent_young,4,right=FALSE)
h67 <- data.frame(mobs9$Life_expectancy,g67,g68)



g69 <- cut(mobs9$percent_old,4,right=FALSE,labels=c(1:4))
g70 <- cut(mobs9$percent_old,4,right=FALSE)
h69 <- data.frame(mobs9$percent_old,g69,g70)



v13 <- data.frame(as.numeric(h61$g61),as.numeric(h63$g63))
v14 <- data.frame(as.numeric(h65$g65),as.numeric(h67$g67),as.numeric(h69$g69))

c18 <- cca(v13,v14) 
put(c18$xstructcorr[1],c18$xstructcorr[2])

}
t18 <- magic_result_as_dataframe()
p35<- mean(t18$`c18$xstructcorr[1]`)
p36<- mean(t18$`c18$xstructcorr[2]`)
s35 <- sd(t18$`c18$xstructcorr[1]`)
s36 <- sd(t18$`c18$xstructcorr[2]`)


# simulations for categorical 3

library(MASS)
library(CCA)
library(magicfor)

magic_for(put,silent=TRUE)
for ( i in (1:100))
  
{
  
  mobs8 <-  mvrnorm(n = 500, mu = mu, Sigma = sigma)
  mobs9 <- data.frame(mobs8)

g71 <- cut(mobs9$percent_servcices,3,right=FALSE,labels=c(1:3))
g72 <- cut(mobs9$percent_servcices,3,right=FALSE)
h71 <- data.frame(mobs9$percent_servcices,g71,g72)




g73 <- cut(mobs9$Unemployment_rate,3,right=FALSE,labels=c(1:3))
g74 <- cut(mobs9$Unemployment_rate,3,right=FALSE)
h73 <- data.frame(mobs9$Unemployment_rate,g73,g74)



g75 <- cut(mobs9$Life_expectancy,3,right=FALSE,labels=c(1:3))
g76 <- cut(mobs9$Life_expectancy,3,right=FALSE)
h75 <- data.frame(mobs9$Life_expectancy,g75,g76)



g77 <- cut(mobs9$percent_young,3,right=FALSE,labels=c(1:3))
g78 <- cut(mobs9$percent_young,3,right=FALSE)
h77 <- data.frame(mobs9$Life_expectancy,g77,g78)



g79 <- cut(mobs9$percent_old,3,right=FALSE,labels=c(1:3))
g80 <- cut(mobs9$percent_old,3,right=FALSE)
h79 <- data.frame(mobs9$percent_old,g79,g80)


v15 <- data.frame(as.numeric(h71$g71),as.numeric(h73$g73))
v16<- data.frame(as.numeric(h75$g75),as.numeric(h77$g77),as.numeric(h79$g79))


c19 <- cca(v15,v16) 
put(c19$xstructcorr[1],c19$xstructcorr[2])

}
t19 <- magic_result_as_dataframe()
p37<- mean(t19$`c19$xstructcorr[1]`)
p38<- mean(t19$`c19$xstructcorr[2]`)
s37 <- sd(t19$`c19$xstructcorr[1]`)
s38 <- sd(t19$`c19$xstructcorr[2]`)