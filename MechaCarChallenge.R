mechacar_mpg <- read.csv("MechaCar_mpg.csv", check.names = F, stringsAsFactors = F)
names(mechacar_mpg)
head(mechacar_mpg)
#create a matrix of variables
mechacar_matric <- as.matrix(mechacar_mpg[, c("vehicle length", "vehicle weight", "ground clearance", "AWD", "mpg")])
#correlation of mpg vs vehicle weight
cor(mechacar_mpg$mpg, mechacar_mpg$"vehicle length")
cor(mechacar_mpg$mpg, mechacar_mpg$"ground clearance")


#correlation using matrix of variables
cor(mechacar_matric)
#linear model (linear regression)
model_vl <- lm(mpg ~ `vehicle length`, mechacar_mpg)
yval_vl <- model_vl$coefficients[2]  * mechacar_mpg$"vehicle length" + model_vl$coefficients["(Intercept)"]
head(yval_vl)
model_vl
summary(model_vl)

model_wgt <- lm(mpg ~ `vehicle weight`, mechacar_mpg)
yval_wgt <- model_wgt$coefficients[2]  * mechacar_mpg$"vehicle weight" + model_wgt$coefficients["(Intercept)"]
model_wgt
summary(model_wgt)

model_gc<- lm(mpg ~ `ground clearance`, mechacar_mpg)
yval_gc <- model_gc$coefficients[2]  * mechacar_mpg$"ground clearance" + model_gc$coefficients["(Intercept)"]
model_gc
summary(model_gc)

#multiple linear regression
multi_model1 <- lm(mpg ~ `vehicle length` + `vehicle weight` + `spoiler angle`, mechacar_mpg)
summary(multi_model1)
multi_model2 <- lm(mpg ~ `vehicle length` + `vehicle weight` + `ground clearance` + AWD, mechacar_mpg)
summary(multi_model2)

multi_model3 <- lm(mpg ~ `vehicle length` + `vehicle weight` + `ground clearance`, mechacar_mpg)
summary(multi_model3)

multi_model4 <- lm(mpg ~ `vehicle length` + `ground clearance`, mechacar_mpg)
summary(multi_model4)

plt <- ggplot(mechacar_mpg, aes(x=`vehicle length`, y=mpg))
plt + geom_point() + geom_line(aes(y=yval_vl), color="red")

plt <- ggplot(mechacar_mpg, aes(y=mpg, x=`vehicle weight`))
plt + geom_point() + geom_line(aes(y=yval_wgt), color="red")

plt <- ggplot(mechacar_mpg, aes(y=mpg, x=`ground clearance`))
plt + geom_point() + geom_line(aes(y=yval_gc), color="red")  






