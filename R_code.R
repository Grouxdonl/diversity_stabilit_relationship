library(piecewiseSEM)
library(lmerTest)
library(nlme)

## Full models for bat communities

sem_bats = list(
                lme(species_richness ~ agriculture+log(urbanization+3), method="ML", data = data_bats, random = ~ 1 | region),
                lme(weighted_mpd ~ agriculture+log(urbanization+3), method="ML", data = data_bats, random = ~ 1 | region),
                lme(log(1/weighted_mean_cvpop) ~ agriculture+log(urbanization+3)+species_richness+weighted_mpd, method="ML", data = data_bats, random = ~ 1 | region),
                lme(log(1/synchrony) ~ agriculture+log(urbanization+3)+species_richness+weighted_mpd, method="ML", data = data_bats, random = ~ 1 | region),
                lme(log(1/CV_com) ~ log(1/weighted_mean_cvpop)+log(1/synchrony), method="ML", data = data_bats, random = ~ 1 | region)
                )

# Extraction of model summary
sem.coefs(sem_bats, data_bats, standardize='scale', corr.errors=c("log(1/weighted_mean_cvpop)~~log(1/synchrony)"))
# Fit of the model
sem.fit(sem_bats, corr.errors=c("log(1/weighted_mean_cvpop)~~log(1/synchrony)"), data_bats)

## Full models for bird communities

sem_birds = list(
                gls(species_richness ~ agriculture+log(urbanization+3), method = "ML", corr=corExp(c(300000,0.7), form=~longitude+latitude, nugget=T), na.action = na.omit, data = rescom),
                gls(weighted_mpd ~ agriculture+log(urbanization+3), method = "ML", corr=corExp(c(600000,0.7), form=~longitude+latitude, nugget=T), na.action = na.omit, data = rescom),
                gls(log(1/weighted_mean_cvpop) ~ agriculture+log(urbanization+3)+species_richness+weighted_mpd, corr=corExp(c(600000,0.7), form=~longitude+latitude, nugget=T), method = "ML", data = rescom),
                gls(logphi ~ agriculture+log(urbanization+3)+species_richness+weighted_mpd, na.action = na.omit, method = "ML", data = rescom),
                lm(log(1/CV_com) ~ log(1/weighted_mean_cvpop)+log(1/synchrony), na.action = na.omit, data = rescom)
)

# Extraction of model summary
sem.coefs(sem_birds, rescom, standardize = "scale", corr.errors=c("log(1/weighted_mean_cvpop)~~log(1/synchrony)"))
# Fit of the model
sem.fit(sem_birds, corr.errors=c("log(1/weighted_mean_cvpop)~~log(1/synchrony)"), data_birds)

## Full models for butterflies communities

sem_butt = list(
              gls(richtot ~ agriculture+log(urbanization+3), method="ML", corr=corGaus(c(160000,0.8), form=~longitude+latitude, nugget=T), data = data_butterflies),
              gls(weighted_mpd ~ agriculture+log(urbanization+3), method="ML", corr=corGaus(c(500000,0.5), form=~longitude+latitude, nugget=T), data = data_butterflies),
              gls(log(1/weighted_mean_cvpop) ~ agriculture+log(urbanization+3)+species_richness+weighted_mpd, method="ML", data = data_butterflies),
              gls(log(1/synchrony) ~ agriculture+log(urbanization+3)+species_richness+weighted_mpd, method="ML", data = data_butterflies),
              lm(log(1/CV_com) ~ log(1/weighted_mean_cvpop)+log(1/synchrony), na.action = na.omit, data = data_butterflies)
)

# Extraction of model summary
coef = sem.coefs(sem_butt, data_butterflies, standardize = "scale", corr.errors=c("log(1/weighted_mean_cvpop)~~log(1/synchrony)"))
# Fit of the model
sem.fit(sem_butt, corr.errors=c("log(1/weighted_mean_cvpop)~~log(1/synchrony)"), data_butterflies)
