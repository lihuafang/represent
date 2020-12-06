# Ordinal representativeness - emergency preparedness 

rp<-read.csv("emergency.csv")


rpt <- subset(rp, gtime!="NA" & repbureau100!="NA" & gender!="NA")
rpm <- subset(rp, gmoney!="NA" & repbureau100!="NA" & gender!="NA")
rpb <- subset(rp, gblood!="NA" & repbureau100!="NA" & gender!="NA")

rptf <- subset(rp, gtime!="NA" & repbureau100!="NA" & gender=="Female")
rptm <- subset(rp, gtime!="NA" & repbureau100!="NA" & gender=="Male")

rpmf <- subset(rp, gmoney!="NA" & repbureau100!="NA" & gender=="Female")
rpmm <- subset(rp, gmoney!="NA" & repbureau100!="NA" & gender=="Male")

rpbf <- subset(rp, gblood!="NA" & repbureau100!="NA" & gender=="Female")
rpbm <- subset(rp, gblood!="NA" & repbureau100!="NA" & gender=="Male")


# descriptive statistics 
# use - summary() gender, race, age, income 
f.chief <- subset(rpt, chief=="1")
m.chief <- subset(rpt, chief=="0")


# regression output , all - female chief, not sig
lmt <- lm(gtime~chief, data=rpt) #not sig
anova(lmt)
rpt1 <- subset(rpt, chief=="1")
rpt0 <- subset(rpt, chief=="0")
t.test(rpt1$gtime, rpt0$gtime, na.rm=T)
logit.t <- glm(factor(time)~chief,data = rpt, family = "binomial") # not sig

lmm <- lm(gmoney~chief, data=rpm) #not sig
anova(lmm)
rpm1 <- subset(rpm, chief=="1")
rpm0 <- subset(rpm, chief=="0")
t.test(rpm1$gmoney, rpm0$gmoney, na.rm=T)
logit.m <- glm(money ~ chief, data = rpm, family = "binomial") #logit, no sig

lmb <- lm(gblood~chief, data=rpb) #not sig
anova(lmb)
rpb1 <- subset(rpb, chief=="1")
rpb0 <- subset(rpb, chief=="0")
t.test(rpb1$gblood, rpb0$gblood, na.rm=T)
logit.b <- glm(blood~chief, data = rpb, family = "binomial") # not sig

## women only 
lmtf <- lm(gtime~chief, data=rptf) #women respondents only
anova(lmtf)
logit.t.f <- glm(factor(time)~chief,data = rptf, family = "binomial") # not sig

lmmf <- lm(gmoney~chief, data=rpmf) #not sig
anova(lmmf)
logit.m.f <- glm(money ~ chief, data = rpmf, family = "binomial") # not sig

lmbf <- lm(gblood~chief, data=rpbf) #not sig
anova(lmbf)
logit.b.f <- glm(blood ~ chief, data = rpbf, family = "binomial") # not sig

## men only
lmtm <- lm(gtime~chief, data=rptm) #men respondents
anova(lmtm)
logit.t.m <- glm(factor(time)~chief,data = rptm, family = "binomial") # sig. 0.1

lmmm <- lm(gmoney~chief, data=rpmm) #not sig
anova(lmmm)
logit.m.m <- glm(money ~ chief, data = rpmm, family = "binomial") # not sig

lmbm <- lm(gblood~chief, data=rpbm) # not sig
anova(lmbm)
logit.b.m <- glm(blood ~ chief, data = rpbm, family = "binomial") # not sig


stargazer(lmt, lmtf, lmtm, 
          lmm, lmmf, lmmm, 
          lmb, lmbf, lmbm, type="html", digits = 2,
          title="The Impacts of Ordinal Gender Representativeness on Emergency Preparedness", 
          dep.var.caption = "Emergency Preparedness",
          covariate.labels=c("Female chief","Female chief"),
          single.row=F, out="ols.htm")


stargazer(logit.t, logit.t.f, logit.t.m, 
          logit.m, logit.m.f, logit.m.m,
          logit.b, logit.b.f, logit.b.m, type="html", digits = 2,
          title="The Impacts of Ordinal Gender Representativeness on Emergency Preparedness", 
          dep.var.caption = "Emergency Preparedness",
          covariate.labels=c("Female chief","Female chief"),
          single.row=F, out="logit.htm")

t.test(rptf$gtime, rptm$gtime, na.rm=T) #no sig
t.test(rpmf$gmoney, rpmm$gmoney, na.rm=T) #no sig
t.test(rpbf$gblood, rpbm$gblood, na.rm=T) # no sig

### women
rptf1 <- subset(rptf, chief=="1")
rptf0 <- subset(rptf, chief=="0")

rpmf1 <- subset(rpmf, chief=="1")
rpmf0 <- subset(rpmf, chief=="0")

rpbf1 <- subset(rpbf, chief=="1")
rpbf0 <- subset(rpbf, chief=="0")

t.test(rptf1$gtime, rptf0$gtime, na.rm=T) #no sig
t.test(rpmf1$gmoney, rpmf0$gmoney, na.rm=T) #no sig
t.test(rpbf1$gblood, rpbf0$gblood, na.rm=T) #no sig


### men, no sig 
rptm1 <- subset(rptm, chief=="1")
rptm0 <- subset(rptm, chief=="0")

rpmm1 <- subset(rpmm, chief=="1")
rpmm0 <- subset(rpmm, chief=="0")

rpbm1 <- subset(rpbm, chief=="1")
rpbm0 <- subset(rpbm, chief=="0")

t.test(rptm1$gtime, rptm0$gtime, na.rm=T) #no sig
t.test(rpmm1$gmoney, rpmm0$gmoney, na.rm=T) #no sig
t.test(rpbm1$gblood, rpbm0$gblood, na.rm=T) #no sig
