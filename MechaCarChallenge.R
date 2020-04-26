mechacar_mpg <- read.csv("MechaCar_mpg.csv", check.names = F, stringsAsFactors = F)
names(mechacar_mpg)
head(mechacar_mpg)
#create a matrix of variables
mechacar_matric <- as.matrix(mechacar_mpg[, c("vehicle length", "vehicle weight", "AWD", "mpg")])
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


#Suspension Coil Summary
suspension_coil = read.csv("Suspension_Coil.csv")
typeof(suspension_coil$PSI)

psi_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarise(Mean_PSI= mean(PSI), Median_PSI = median(PSI), Variance_PSI = var(PSI), STD_PSI = sd(PSI))
psi_summary

plt <- ggplot(psi_summary,aes(x=Manufacturing_Lot, y=Variance_PSI))
plt + geom_point(size=4) + labs(x="Manufacturing Lot", y="Variance PSI")

#Suspension Coild T-Test
plt <- ggplot(suspension_coil, aes(x=PSI))
plt + geom_density()

sample_data <- suspension_coil %>% sample_n(25)
head(sample_data)

plt <- ggplot(sample_data, aes(x=log10(PSI)))
plt + geom_density()














