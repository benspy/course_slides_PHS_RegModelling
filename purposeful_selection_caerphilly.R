
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


# Step 1 - Univariable logistic regression
for (var in vars) {
  formula <- paste("mi ~", var )
  model<-glm(formula, data=dat, family=binomial(link="logit"))
  cat("\n")
  print(var)
  print(summary(model)$coef)
  print("p-value LR-test:")
  print(anova(model, test="Chisq")[["Pr(>Chi)"]][2])
}


# Include all
