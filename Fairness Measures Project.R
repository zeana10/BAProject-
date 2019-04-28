#Gentrification Fairness New Dataset
library(tree)

#Reading CSV
df  = read.csv("C:/Users/adity/Desktop/Columbia University/Spring 19/Business Analytics/Project/zipcode_neighborhood_df_test_exc_income.csv")

#Defining White Majority Neighbourhoods
df$whitemajority = ifelse(df$X..White>=df$X..African.American & df$X..White>=df$X..Asian & df$X..White>=df$X..Native.Hawaiian.and.Other.Pacific.Islander & df$X..White>=df$X..Some.other.race,1,0)
df$whitemajority

#Defining African American Majority Neighbourhoods
df$afammajority = ifelse(df$X..African.American>=df$X..White & df$X..African.American>=df$X..Asian & df$X..African.American>=df$X..Native.Hawaiian.and.Other.Pacific.Islander & df$X..African.American>=df$X..Some.other.race,1,0)
df$afammajority

#Defining Asian Majority Neighbourhoods
df$asianmajority = ifelse(df$X..Asian>=df$X..White & df$X..Asian>=df$X..African.American & df$X..Asian>=df$X..Native.Hawaiian.and.Other.Pacific.Islander & df$X..Asian>=df$X..Some.other.race,1,0)
df$asianmajority

#89 White Majority, 40 African American Majority, 14 Asian Majority

#Tree Generation
attach(df)
df$gent = df$gentrification_label

#Regression Tree
gent_tree = tree(gent~ starbucks_count + cafe_count + bars + avg_price + avg_rating + whitemajority +afammajority + asianmajority +X..White + X..African.American + X..Asian + X..Native.Hawaiian.and.Other.Pacific.Islander + X..Some.other.race, df)
plot(gent_tree)
text(gent_tree, pretty=0)

#Classification Tree
gent_class = tree(as.factor(gent)~  starbucks_count + cafe_count + bars + avg_price + avg_rating + whitemajority +afammajority + asianmajority +X..White + X..African.American + X..Asian + X..Native.Hawaiian.and.Other.Pacific.Islander + X..Some.other.race, df)
plot(gent_class)
text(gent_class, pretty = 0)


#Prune Regression Tree
prune_gent = prune.tree(gent_tree, best=10)
plot(prune_gent)
text(prune_gent)
summary(prune_gent)

#Prune Classifictaion Tree
prune_class = prune.tree(gent_class, best = 10)
plot(prune_class)
text(prune_class)
summary(prune_class)

gent_predict = predict(prune_gent, newdata = df )

#Fairness Measures

gent_predict = df
gent_predict$result = as.integer(predict(prune_gent, newdata = df)>= 0.15)

#White Majority
#Demographic Parity
PDW1 =sum((gent_predict$result==1)&(gent_predict$whitemajority==1))/sum(gent_predict$whitemajority==1)
PDW0 =sum((gent_predict$result==1)&(gent_predict$whitemajority==0))/sum(gent_predict$whitemajority==0)
PDW1 #0.2134
PDW0 #0.5735

#Accuracy Parity
gent_predict$accurate = ifelse(gent_predict$result==gent_predict$gent,1,0)
PAW1 = sum((gent_predict$accurate==1)&(gent_predict$whitemajority==1))/sum(gent_predict$whitemajority==1)
PAW0 =sum((gent_predict$accurate==1)&(gent_predict$whitemajority==0))/sum(gent_predict$whitemajority==0)
PAW1 #0.7640
PAW0 #0.5441

#Unawareness Parity
gent_predict$unaware = as.integer(predict(prune_gent, newdata = df[,-25])>= 0.15)
PUW1 = sum(gent_predict$unaware)/length(gent_predict$unaware)
PUW0 = sum(gent_predict$result)/length(gent_predict$result)
PUW1 #0.3694
PUW0 #0.3694


#African American Majority
#Demographic Parity
PDB1 =sum((gent_predict$result==1)&(gent_predict$afammajority==1))/sum(gent_predict$afammajority==1)
PDB0 =sum((gent_predict$result==1)&(gent_predict$afammajority==0))/sum(gent_predict$afammajority==0)
PDB1 #0.525
PDB0 #0.3162

#Accuracy Parity
PAB1 = sum((gent_predict$accurate==1)&(gent_predict$afammajority==1))/sum(gent_predict$afammajority==1)
PAB0 =sum((gent_predict$accurate==1)&(gent_predict$afammajority==0))/sum(gent_predict$afammajority==0)
PAB1 #0.525
PAB0 #0.7179

#Unawareness Parity
gent_predict$unawareafam = as.integer(predict(prune_gent, newdata = df[,-26])>= 0.15)
PUB1 = sum(gent_predict$unawareafam)/length(gent_predict$unawareafam)
PUB1 #0.3694


#Asian Majority
#Demographic Parity
PDC1 =sum((gent_predict$result==1)&(gent_predict$asianmajority==1))/sum(gent_predict$asianmajority==1)
PDC0 =sum((gent_predict$result==1)&(gent_predict$asianmajority==0))/sum(gent_predict$asianmajority==0)
PDC1 #0.2142
PDC0 #0.3846

#Accuracy Parity
PAC1 = sum((gent_predict$accurate==1)&(gent_predict$asianmajority==1))/sum(gent_predict$asianmajority==1)
PAC0 =sum((gent_predict$accurate==1)&(gent_predict$asianmajority==0))/sum(gent_predict$asianmajority==0)
PAC1 #0.7857
PAC0 #0.6573

#Unawareness Parity
gent_predict$unawareasian = as.integer(predict(prune_gent, newdata = df[,-27])>= 0.15)
PUC1 = sum(gent_predict$unawareasian)/length(gent_predict$unawareasian)
PUC1 #0.3694

#Thus, Model has Fairness in Unawareness, but is not Fair in Demographic Parity and Accuracy Parity. Hence, model seems to be unfair.
#This gives insights into relationship between rents in neighbourhoods and racial diversity there.
