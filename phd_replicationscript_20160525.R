
# ----------------------------------------------------------------------------------------

# Replication Script, "Interim Governments and the Stability of Post-Interim Peace"
# Author: Julia Strasheim, julia.strasheim@giga-hamburg.de
# Affiliation: GIGA German Institute of Global and Area Studies & University of Heidelberg
# Date: 12.08.2016 

# ----------------------------------------------------------------------------------------

# This script replicates the main analysis and robustness checks of my dissertation,
# "Interim Governments and the Stability of Post-Interim Peace" (Univ. of Heidelberg, 2016).
# For reasons of parsimony, scripts of graphs are available on request. 
# A codebook is available in Appendix A of the dissertation. 


# Load the data set (adapt link if necessary) & necessary additional packages

load("C:/Users/Julia/Desktop/Dissertation/R/Replication/interimgovernment_dataset20160819.Rda")

library(plyr)
library(dplyr)
library(survival)
library(reshape2)
library(ggplot2)
library(stargazer)


# TABLES FROM CHAPTER 4

# Table 4.2: Descriptive Statistics

descriptive <- interimgov %>% 
  select(powersharing, inter_lenient, parallel, participation,
         ethnic, incompatibility, intensity, gdpcap, population)

stargazer(descriptive, summary = TRUE, header = FALSE, float = FALSE, style = "apsr")


# Table 4.3 (Cox Proportional Hazard Models)

model1 <- coxph(ig_survobject ~ powersharing + inter_lenient + cluster(gwnoa), data = interimgov)

model2 <- coxph(ig_survobject ~ powersharing + inter_lenient + ethnic + incompatibility + 
                  intensity + log(gdpcap) + log(population) + cluster(gwnoa), data = interimgov)

model3 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                  cluster(gwnoa), data = interimgov)

model4 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                  ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                  cluster(gwnoa), data = interimgov)


# Table 4.4: Hazard Ratios

hazardratios <- as.data.frame(exp(model4$coefficients))


# Table 4.5: Schoenfeld Residuals Test

schoenfeld <- cox.zph(model4, transform = log)
schoenfeld <- as.data.frame(schoenfeld$table)

# Table 4.6 Robustness Check I: Subsetting

ig_national <- interimgov[which(interimgov$national == 1),] # Creates Subset "National"

model5 <-  coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   cluster(gwnoa), data = ig_national)  

ig_trans <- interimgov[!(interimgov$igname == "BOSNIA" | 
                          interimgov$igname == "CROATIA" |
                          interimgov$igname == "CAMBODIA" |
                          interimgov$igname == "KOSOVO" |
                          interimgov$igname == "NAMIBIA" |
                          interimgov$igname == "EAST TIMOR"),] # Creates Subset "No TAs"

model6 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                  ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                  cluster(gwnoa), data = ig_trans)   

ig_coup <- interimgov[which(interimgov$coup == 0),] # Creates Subset "No Coups"

model7 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                  ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                  cluster(gwnoa), data = ig_coup)  

ig_cc <- interimgov[!(interimgov$igname == "BOSNIA" |
                      interimgov$igname == "SUDAN" |
                      interimgov$igname == "MOZAMBIQUE" |
                      interimgov$igname == "COMOROS 1"),] # Creates Subset "Critical Cases"

model8 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                  ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                  cluster(gwnoa), data = ig_cc)  


# Table 4.7: Robustness Check II: Recoding Variables 1

model9 <- coxph(ig_survobject ~ powersharing + inter_strict + parallel + participation + 
                  ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                  cluster(gwnoa), data = interimgov)

model10 <- coxph(ig_survobject ~ powersharing + inter_cont + parallel + participation + 
                  ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                  cluster(gwnoa), data = interimgov) 

model11 <- coxph(ig_survobject ~ powersharing + inter_lenient + 
                   parallel_pol + parallel_mil + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   cluster(gwnoa), data = interimgov) 


# Table 4.8: Robustness Check III: Additional Control Variables 1

model12 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                  ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                  condurweeks + cluster(gwnoa), data = interimgov)

model13 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   rebelstrength + cluster(gwnoa), data = interimgov)

model14 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   demo_lenient + cluster(gwnoa), data = interimgov)

model15 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   demo_strict + cluster(gwnoa), data = interimgov)


# Table 4.9: Robustness Check IV: Additional Control Variables 2

model16 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   agreement + cluster(gwnoa), data = interimgov)

model17 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   igdurweeks + cluster(gwnoa), data = interimgov)

model18 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   igviolence + cluster(gwnoa), data = interimgov)

model19 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   resources + cluster(gwnoa), data = interimgov)



# Table 4.11: Robustness Check VI: Subsetting 2

igethnic <- interimgov[which(interimgov$ethnic == 1),]

model20 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                  incompatibility + intensity + log(gdpcap) + log(population) + 
                  cluster(gwnoa), data = igethnic) 

iginc <- interimgov[which(interimgov$incompatibility == 2),]

model21 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + intensity + log(gdpcap) + log(population) + 
                   cluster(gwnoa), data = iginc) 

igcw <- interimgov[which(interimgov$intensity == 1),]

model22 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + log(gdpcap) + log(population) + 
                   cluster(gwnoa), data = igcw) 

igpa <- interimgov[which(interimgov$agreement == 1),]

model23 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   cluster(gwnoa), data = igpa) 


# Figure A.1: Influential Observations

dfbeta <- as.data.frame(residuals(model4, type="dfbeta"))
names(dfbeta) <- c("Power-Sharing IG",  "International IG", "Parallel Institutions",
                   "Participation", "Ethnic Conflict","Incompatibility",
                   "Conflict Intensity", "GDP/Capita","Population Size")

dfbeta$ID <- seq(1, nrow(dfbeta))
dfbeta <- melt(dfbeta, "ID", value.name = "dfbeta")

ggplot(dfbeta, aes(x=dfbeta)) + geom_histogram(bins=10) + facet_wrap(~variable, ncol = 3)


# Table A.4: Correlation Matrix

correlate <- as.data.frame(cor(descriptive, use = "pairwise.complete"))
correlate[upper.tri(correlate)] <- NA


# Table A.5: Robustness Check VII: Recoding Variables 2

model24 <- coxph(ig_survobject ~ powersharing + inter_lenient + cluster(gwnoa), data = interimgov)


model25 <- coxph(ig_survobject ~ powersharing + inter_lenient + ethnic + incompatibility + 
                  intensity + log(gdpcap) + log(population) + cluster(gwnoa), data = interimgov)

model26 <- coxph(ig_survobject ~ powersharing + inter_lenient + factor(parallel) + 
                   factor(participation) + cluster(gwnoa), data = interimgov)

model27 <- coxph(ig_survobject ~ powersharing + inter_lenient + factor(parallel) + 
                   factor(participation) + ethnic + incompatibility + intensity + 
                   log(gdpcap) + log(population) +
                   cluster(gwnoa), data = interimgov)


# Table A.6: Hazard Ratios 2

hazardratios_2 <- as.data.frame(exp(model27$coefficients))


# Table A.7: Robustness Check VII: Frailty Models

model28 <- coxph(ig_survobject ~ powersharing + inter_lenient + frailty(gwnoa), data = interimgov)

model29 <- coxph(ig_survobject ~ powersharing + inter_lenient + ethnic + incompatibility + 
                  intensity + log(gdpcap) + log(population) + frailty(gwnoa), data = interimgov)

model30 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   frailty(gwnoa), data = interimgov)

model31 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                  ethnic + incompatibility + intensity + log(gdpcap) + log(population) + 
                   frailty(gwnoa), data = interimgov)


# Table A.8 Robustness Check IX: Interaction Models 1

model32 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + 
                   powersharing * parallel + participation + 
                   cluster(gwnoa), data = interimgov)

model33 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   powersharing * parallel + ethnic + incompatibility + intensity + 
                   log(gdpcap) + log(population) + cluster(gwnoa), data = interimgov)

model34 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + 
                   inter_lenient * parallel + participation + 
                   cluster(gwnoa), data = interimgov)

model35 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   inter_lenient * parallel + ethnic + incompatibility + intensity + 
                   log(gdpcap) + log(population) + cluster(gwnoa), data = interimgov)


# Table A.9 Robustness Check X: Interaction Models 2

model36 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   powersharing * inter_lenient + ethnic + incompatibility + intensity + 
                   log(gdpcap) + log(population) + cluster(gwnoa), data = interimgov)

model37 <- coxph(ig_survobject ~ powersharing + inter_strict + parallel + participation + 
                   powersharing * inter_strict + ethnic + incompatibility + intensity + 
                   log(gdpcap) + log(population) + cluster(gwnoa), data = interimgov)

model38 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   demo_lenient + participation * demo_lenient + 
                   ethnic + incompatibility + intensity + 
                   log(gdpcap) + log(population) + cluster(gwnoa), data = interimgov)

model39 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   demo_strict + participation * demo_strict + 
                   ethnic + incompatibility + intensity + 
                   log(gdpcap) + log(population) + cluster(gwnoa), data = interimgov)

# Table A.11 Robustness Check XII: Additional Control Variables 3

model40 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                  ethnic + incompatibility + intensity + gdpgrowth + log(population) + 
                  cluster(gwnoa), data = interimgov)

model41 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + infantmortality + log(population) + 
                   cluster(gwnoa), data = interimgov)

model42 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + rebelcount +
                   cluster(gwnoa), data = interimgov)

model43 <- coxph(ig_survobject ~ powersharing + inter_lenient + parallel + participation + 
                   ethnic + incompatibility + intensity + log(gdpcap) + log(population) + rebelcohesion +
                   cluster(gwnoa), data = interimgov)

