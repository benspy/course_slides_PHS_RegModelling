
# read in dataset
dat<-read.csv("data/data_caerphilly_full.csv", sep = ";")
str(dat)

# make factors out of character variables
dat<- dat |> mutate(mi = factor(mi, levels=c(0, 1), labels=c("No", "Yes")),
                    socclass = factor(socclass, levels=c("I", "II", "IIINM", "IIIM", "IV", "V")), 
                    diabetes=factor(diabetes, levels=c("No/uncertain", "Yes")), 
                    smoking=factor(smoking, levels=c("Never smoked", " Ex>5 years", "Ex 1-4 years" ,"<15 per day", ">15 per day")),
                    hbpsyst = factor(hbpsyst, levels=c(0, 1), labels=c("No", "Yes")),
                    hbpdias = factor(hbpdias, levels=c(0, 1), labels=c("No", "Yes")),
                    bmicat = factor(bmicat, levels=c("Underweight", "Normal", "Overweight", "Obese")),
                    cursmoke = factor(cursmoke, levels=c("No", "Yes")))
str(dat)
head(dat,10)

# Pre-selection of variables: 
# dependent variable: mi
# Candidate independent variables: "socclass" "diabetes" "cursmoke" "smoking"  "fibrin"   "totchol"  "hdlchol"  "bpsyst"   "bpdias"  "bmi"  
# This drops (for now) categorical variables that reflect categories of continuous variables (we prefer to keep the full information of the continuous vars)
vars<-c("socclass", "diabetes", "cursmoke", "smoking", "fibrin", "totchol", "hdlchol", "bpsyst", "bpdias", "bmi" )


##
# Step 1 - Univariable regressions regression -> selection for inclusion P<0.2 (based on Wald or LR)
for (var in vars) {
  formula <- paste("mi ~", var )
  model<-glm(formula, data=dat, family=binomial(link="logit"))
  cat("\n")
  print(var)
  print(summary(model)$coef)
  print("p-value LR-test:")
  print(anova(model, test="Chisq")[["Pr(>Chi)"]][2])
}


##
# Step 2
# Include all
mod_f<-glm(mi ~ socclass + diabetes + cursmoke + smoking + fibrin + totchol + hdlchol + bpsyst + bpdias + bmi, data=dat, family=binomial(link="logit"))
summary(mod_f)

table(dat$smoking, dat$cursmoke)
# can you explain why the model leaves out the category “smoking>15 per day“?
# Reason: cursmoke = Yes indicates all current smokers which are made up of those with >15 and <15 cigarettes per day
# That is the variables have 3 categories together representing only 2 categories, one has to be dropped
# We could equivalently drop cursmoke and the same model fit should result, let's check
mod_f_alt<-glm(mi ~ socclass + diabetes  + smoking + fibrin + totchol + hdlchol + bpsyst + bpdias + bmi, data=dat, family=binomial(link="logit"))
summary(mod_f_alt) # exactly the same residual deviance
# This means that we only need to include one of the variables


##
# Step 3
# P-vaues of Wald tests for bmi, bpsyst >0.1 (Let's use a stricter criterion here) -> suggest for exclusion
# Check LR tests for categorical variables: smoking history, socclass (for these we only have Wald tests of individual categories which does not tell us whether the variable as whole is important)
mod_f<-glm(mi ~ socclass + diabetes + cursmoke + smoking + fibrin + totchol + hdlchol + bpdias , data=dat, family=binomial(link="logit"))
mod_r1<-glm(mi ~ diabetes + cursmoke + smoking + fibrin + totchol + hdlchol + bpdias , data=dat, family=binomial(link="logit")) # without socclass
mod_r2<-glm(mi ~ socclass + diabetes + cursmoke + fibrin + totchol + hdlchol + bpdias , data=dat, family=binomial(link="logit")) # without smoking
anova(mod_f, mod_r1, test="Chisq") # suggest socclass for exclusion
anova(mod_f, mod_r2, test="Chisq") # keep smoking BUT in that case you can drop cursmoke


# Suggest for exclusion: bmi, bpsyst, socclass, cursmoke
mod_r<-glm(mi ~ diabetes + smoking + fibrin + totchol + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
anova(mod_f, mod_r, test="Chisq") # Exclude the these vars
# IMPORTANT HERE also to check any major confounding effects of excluded variables on remaining variables
# The only coefficient that has a major change is bpdias, but this is understandable as diastolic blood pressure are correlated
# bpdias is appears to be adequately able to replace the little explanatory information that bpsyst was adding

##
# Step 4
# New model 
mod_n<-glm(mi ~ diabetes + smoking + fibrin + totchol + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
summary(mod_n)

##
# Step 5: Repeat step 3 and 4
# Check again LR test for smoking
mod_r<-glm(mi ~ diabetes + cursmoke  + fibrin + totchol + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
anova(mod_n, mod_r, test="Chisq") # Keep smoking -> keep the new model above (mod_n) not further repetitions of 3 and 4 neaded

##
# Step 6: Add any variables excluded at step 2 -> none excluded -> stay with current model

##
# Step 7: Check linearity of associations for continuous variables

# fibrin
# define a factor variable representing quintils of fibrin
hist(dat$fibrin)
quint <- quantile(dat$fibrin, seq(0,1,0.2))
dat$fibrin_q <- cut(dat$fibrin, quint, include.lowest = TRUE)
# a numeric variable representing means within quintiles
qmeans <- tapply(dat$fibrin, dat$fibrin_q, mean)
dat$fibrin_qm <- qmeans[dat$fibrin_q]
table(dat$fibrin_q, dat$fibrin_qm)
# run regressions
temp1<-glm(mi ~ diabetes + smoking + fibrin_q+ totchol + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
summary(temp1)
temp2<-glm(mi ~ diabetes + smoking + fibrin_qm + totchol + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
summary(temp2)
anova(temp1, temp2, test="Chisq") # We can keep the restriction and assume linearity


# totchol
# define a factor variable representing quintils of totchol
hist(dat$totchol)
quint <- quantile(dat$totchol, seq(0,1,0.2))
dat$totchol_q <- cut(dat$totchol, quint, include.lowest = TRUE)
# a numeric variable representing means within quintiles
qmeans <- tapply(dat$totchol, dat$totchol_q, mean)
dat$totchol_qm <- qmeans[dat$totchol_q]
table(dat$totchol_q, dat$totchol_qm)
# run regressions
temp1<-glm(mi ~ diabetes + smoking + fibrin + totchol_q+ hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
summary(temp1)
temp2<-glm(mi ~ diabetes + smoking + fibrin + totchol_qm + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
summary(temp2)
anova(temp1, temp2, test="Chisq") # We can keep the restriction and assume linearity

# hdlchol
# define a factor variable representing quintils of hdlchol
hist(dat$hdlchol)
quint <- quantile(dat$hdlchol, seq(0,1,0.2))
dat$hdlchol_q <- cut(dat$hdlchol, quint, include.lowest = TRUE)
# a numeric variable representing means within quintiles
qmeans <- tapply(dat$hdlchol, dat$hdlchol_q, mean)
dat$hdlchol_qm <- qmeans[dat$hdlchol_q]
table(dat$hdlchol_q, dat$hdlchol_qm)
# run regressions
temp1<-glm(mi ~ diabetes + smoking + fibrin + totchol+ hdlchol_q +  bpdias , data=dat, family=binomial(link="logit"))
summary(temp1)
temp2<-glm(mi ~ diabetes + smoking + fibrin + totchol + hdlchol_qm +  bpdias , data=dat, family=binomial(link="logit"))
summary(temp2)
anova(temp1, temp2, test="Chisq") # We can keep the restriction and assume linearity

# bpdias
# define a factor variable representing quintiles of bpdias
hist(dat$bpdias)
quint <- quantile(dat$bpdias, seq(0,1,0.2))
dat$bpdias_q <- cut(dat$bpdias, quint, include.lowest = TRUE)
# a numeric variable representing means within quintiles
qmeans <- tapply(dat$bpdias, dat$bpdias_q, mean)
dat$bpdias_qm <- qmeans[dat$bpdias_q]
table(dat$bpdias_q, dat$bpdias_qm)
# run regressions
temp1<-glm(mi ~ diabetes + smoking + fibrin + totchol+ hdlchol +  bpdias_q , data=dat, family=binomial(link="logit"))
summary(temp1)
temp2<-glm(mi ~ diabetes + smoking + fibrin + totchol + hdlchol +  bpdias_qm , data=dat, family=binomial(link="logit"))
summary(temp2)
anova(temp1, temp2, test="Chisq") # We can keep the restriction and assume linearity

##
# Step 7: Any plausible interactions -> suggest totchol and smoking 
mod_n_int<-glm(mi ~ diabetes + smoking * totchol + fibrin + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
summary(mod_n_int)
anova(mod_n, mod_n_int, test="Chisq") # -> exclude interaction


##
# final model -> mod_n
summary(mod_n)



