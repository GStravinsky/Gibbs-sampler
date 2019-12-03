# no frequency
b_min1 <- glm(Gold~age:carduse+mcardwdl+mcashcr+mcashwd+second+sex+type+age+carduse, family = binomial, data = gold)
summary(b_min1)
# omit cash credit
b_min2 <-  glm(Gold~age:carduse+mcardwdl+mcashwd+second+sex+type+age+carduse, family = binomial, data = gold)
summary(b_min2)
# omit carduse
b_min3 <-  glm(Gold~age:carduse+mcashwd+mcardwdl+second+sex+type+age, family = binomial, data = gold)
summary(b_min3)
# ommit second
b_min4 <-  glm(Gold~age:carduse+mcashwd+mcardwdl+sex+type+age, family = binomial, data = gold)
summary(b_min4)
# ommit type
b_min5 <-  glm(Gold~age:carduse+mcashwd+mcardwdl+sex+age, family = binomial, data = gold)
summary(b_min5)
# ommit age
b_min6 <-  glm(Gold~age:carduse+mcashwd+mcardwdl+sex, family = binomial, data = gold)
summary(b_min6)

Anova(a,type="III")
# ommit frequency
Anova(b_min1, type="III",test.statistic = "Wald")
# ommit cashcr
Anova(b_min2,type="III",test.statistic = "Wald")
# ommit carduse
Anova(b_min3, type="III",test.statistic = "Wald")
# ommit type
Anova(b_min4, type="III",test.statistic = "Wald")
# ommit  second
Anova(b_min5, type="III",test.statistic = "Wald")
# ommit  age
Anova(b_min6, type="III",test.statistic = "Wald")
# STOP

y.pred_B <- predict(b_min6,type="response")
roc_B <- roc.curve(scores.class0 = y.pred_B, weights.class0 = y, curve=T)
plot(roc_B)

a <- glm(Gold~., family = binomial, data = gold2)
Anova(a,type="III")
# ommit Weekly
b_min1 <- glm(Gold~interaction+mcardwdl+mcashcr+mcashwd+second+sex+type+age+carduse+Monthly, family = binomial, data = gold2)
Anova(b_min1, type="III",test.statistic = "Wald")
# ommit cashcr
b_min2 <- glm(Gold~interaction+mcardwdl+mcashwd+second+sex+type+age+carduse+Monthly, family = binomial, data = gold2)
Anova(b_min2,type="III",test.statistic = "Wald")
# ommit Monthly
b_min3 <- glm(Gold~interaction+mcardwdl+mcashwd+second+sex+type+age+carduse, family = binomial, data = gold2)
Anova(b_min3, type="III",test.statistic = "Wald")
# ommit carduse
b_min4 <- glm(Gold~interaction+mcardwdl+mcashwd+second+sex+type+age, family = binomial, data = gold2)
Anova(b_min4, type="III",test.statistic = "Wald")
# ommit  second
b_min5 <- glm(Gold~interaction+mcardwdl+mcashwd+sex+type+age, family = binomial, data = gold2)
Anova(b_min5, type="III",test.statistic = "Wald")
# ommit  type
b_min6 <- glm(Gold~interaction+mcardwdl+mcashwd+sex+age, family = binomial, data = gold2)
Anova(b_min6, type="III",test.statistic = "Wald")
# ommit age
b_min7 <- glm(Gold~interaction+mcardwdl+mcashwd+sex, family = binomial, data = gold2)
Anova(b_min7, type="III",test.statistic = "Wald")
# ommit cashwd
b_min8 <- glm(Gold~interaction+mcardwdl+sex, family = binomial, data = gold2)
Anova(b_min8, type="III",test.statistic = "Wald")
# STOP

