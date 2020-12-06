# Representative bureaucracy - recycling 

rp <- read.csv("recycling.csv")

r1<-subset(rp, easy!="NA" & gender!="NA" & q15=="united states")
r2<-subset(rp, mid!="NA" & gender!="NA" & q15=="united states")
r3<-subset(rp, hard!="NA" & gender!="NA" & q15=="united states")

rpeasyf <- subset(rp, easy!="NA" & gender=="female"& q15=="united states")
rpeasym <- subset(rp, easy!="NA" & gender=="male"& q15=="united states")

rpmidf <- subset(rp, mid!="NA" & gender=="female"& q15=="united states")
rpmidm <- subset(rp, mid!="NA" & gender=="male"& q15=="united states")

rphardf <- subset(rp, hard!="NA" & gender=="female"& q15=="united states")
rphardm <- subset(rp, hard!="NA" & gender=="male"& q15=="united states")

#descriptive statistics
#use - summary r1$gender, race, age, income
r1chief.f<-subset(r1, chief=="1")
r1chief.m<-subset(r1, chief=="0")


# regression output, all participants

lm.easy <- lm(easy~chief, data=r1) # ols
anova(lm.easy)
logit.plastics <- glm(plastics ~ chief, data = r1, family = "binomial") #logit

lm.mid <- lm(mid~chief, data = r2) #ols
anova(lm.mid)

logit.light <- glm(light~chief, data = r2, family = "binomial") #logit

lm.hard <- lm(hard~chief, data = r3) #ols
anova(lm.hard)

logit.heavy <- glm(heavy~chief, data = r3, family = "binomial") #logit

## women respondents only
lm.easy.f <- lm(easy~chief, data=rpeasyf) #women respondents only, ols
anova(lm.easy.f)
logit.plastics.f <- glm(plastics~chief, data = rpeasyf, family = "binomial") #logit

lm.mid.f <- lm(mid~chief, data=rpmidf) #women respondents only, ols
anova(lm.mid.f)
logit.light.f <- glm(light~chief, data = rpmidf, family = "binomial") #logit

lm.hard.f <- lm(hard~chief, data=rphardf) #women respondents only, ols
anova(lm.hard.f)
logit.heavy.f <- glm(heavy~chief, data = rphardf, family = "binomial") #logit

## men respondents only 
lm.easy.m <- lm(easy~chief, data=rpeasym) #ols
anova(lm.easy.m)
logit.plastics.m <- glm(plastics~chief, data = rpeasym, family = "binomial") #logit

lm.mid.m <- lm(mid~chief, data=rpmidm) #ols
anova(lm.mid.m)
logit.light.m <- glm(light~chief, data = rpmidm, family = "binomial") #logit

lm.hard.m <- lm(hard~chief, data=rphardm) #ols
anova(lm.hard.m)
logit.heavy.m <- glm(heavy~chief, data = rphardm, family = "binomial") #logit


stargazer(lm.easy, lm.easy.f, lm.easy.m, 
          lm.mid, lm.mid.f, lm.mid.m, 
          lm.hard, lm.hard.f, lm.hard.m, type="html", digits=2, 
          title="The Effects of Ordinal Gender Representativeness on Recycling", 
          dep.var.caption = "Willingness to recycle",
          single.row=F, out="recycling ols.htm")

stargazer(logit.plastics, logit.plastics.f, logit.plastics.m,
          logit.light, logit.light.f, logit.light.m,
          logit.heavy, logit.heavy.f, logit.heavy.m, type="html", digits=2, 
          title="The Effects of Ordinal Gender Representativeness on Recycling", 
          dep.var.caption = "Willingness to recycle",
          single.row=F, out="recycling logit.htm")


### general - all 
rpeasy1 <- subset(r1, chief=="1")
rpeasy0 <- subset(r1, chief=="0")

t.test(rpeasy1$easy, rpeasy0$easy, na.rm=T) #no sig

rpmid1 <- subset(r2, chief=="1")
rpmid0 <- subset(r2, chief=="0")

t.test(rpmid1$mid, rpmid0$mid, na.rm=T) #no sig

rphard1 <- subset(r3, chief=="1")
rphard0 <- subset(r3, chief=="0")
t.test(rphard1$hard, rphard0$hard, na.rm=T) # no sig

### t-test women
rpeasyf1 <- subset(rpeasyf, chief=="1")
rpeasyf0 <- subset(rpeasyf, chief=="0")

rpmidf1 <- subset(rpmidf, chief=="1")
rpmidf0 <- subset(rpmidf, chief=="0")

rphardf1 <- subset(rphardf, chief=="1")
rphardf0 <- subset(rphardf, chief=="0")

t.test(rpeasyf1$easy, rpeasyf0$easy, na.rm=T) #no sig
t.test(rpmidf1$mid, rpmidf0$mid, na.rm=T) #marginal sig
t.test(rphardf1$hard, rphardf0$hard, na.rm=T) #no sig

### t.test men # no sig
rpeasym1 <- subset(rpeasym, chief=="1")
rpeasym0 <- subset(rpeasym, chief=="0")

rpmidm1 <- subset(rpmidm, chief=="1")
rpmidm0 <- subset(rpmidm, chief=="0")

rphardm1 <- subset(rphardm, chief=="1")
rphardm0 <- subset(rphardm, chief=="0")

t.test(rpeasym1$easy, rpeasym0$easy, na.rm=T) #no sig
t.test(rpmidm1$mid, rpmidm0$mid, na.rm=T) #no sig
t.test(rphardm1$hard, rphardm0$hard, na.rm=T) #no sig