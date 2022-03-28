## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bate)

## -----------------------------------------------------------------------------
data("NLSY_IQ")

## -----------------------------------------------------------------------------
names(NLSY_IQ)

## -----------------------------------------------------------------------------
NLSY_IQ$age <- factor(NLSY_IQ$age)
NLSY_IQ$race <- factor(NLSY_IQ$race)

## -----------------------------------------------------------------------------
library(vtable)
vtable::st(NLSY_IQ)

## -----------------------------------------------------------------------------
parameters <- bate::collect_par(data=NLSY_IQ,
            outcome="iq_std",
            treatment="BF_months",
            control=c("age","sex","income","motherAge","motherEDU","mom_married","race"),
            other_regressors = c("sex","age"))

## -----------------------------------------------------------------------------
(parameters)

## -----------------------------------------------------------------------------
# Upper bound of Rmax
Rhigh <- 0.61
# Lower bound of delta
deltalow <- 0.01
# Upper bound of delta
deltahigh <- 0.99
# step size to construct grid
e <- 0.01

## ---- fig.width=6, fig.height=4-----------------------------------------------
bate::urrplot(parameters = parameters, deltalow = deltalow, 
              deltahigh = deltahigh, Rlow = parameters$Rtilde,
              Rhigh = 0.61, e=0.01)

## ---- message=FALSE-----------------------------------------------------------
OVB <- bate::ovbias(
  parameters = parameters,
  deltalow=deltalow, 
  deltahigh=deltahigh,
  Rhigh=Rhigh, 
  e=e)

## -----------------------------------------------------------------------------
(OVB$bias_Distribution)

## -----------------------------------------------------------------------------
(OVB$bstar_Distribution)

## ---- fig.width=6, fig.height=4-----------------------------------------------
cplotbias(OVB$Data)

## ---- fig.width=6, fig.height=4-----------------------------------------------
dplotbate(OVB$Data)

## -----------------------------------------------------------------------------
bate::osterbds(parameters = parameters, Rmax=0.61)

## -----------------------------------------------------------------------------
bate::osterdelstar(parameters = parameters, Rmax=0.61)

## ---- fig.width=6, fig.height=4-----------------------------------------------
bate::delfplot(parameters = parameters)

## ---- message=FALSE-----------------------------------------------------------
OVB.par <- ovbias_par(
  data=NLSY_IQ,
  outcome="iq_std",
  treatment="BF_months",
  control=c("age","sex","income","motherAge","motherEDU","mom_married","race"),
  other_regressors = c("sex","age"),
  deltalow=deltalow, 
  deltahigh=deltahigh,
  Rhigh=Rhigh, 
  e=e)

## -----------------------------------------------------------------------------
(OVB.par$bias_Distribution)

## -----------------------------------------------------------------------------
(OVB.par$bstar_Distribution)

## -----------------------------------------------------------------------------
reg_col1 <- lm(
  iq_std ~ BF_months + factor(age) + sex,
  data = NLSY_IQ
)

## -----------------------------------------------------------------------------
reg_col2 <- lm(
  iq_std ~ BF_months + factor(age) + sex +
    income + motherAge + motherEDU + mom_married +
    factor(race),
  data = NLSY_IQ
)

## -----------------------------------------------------------------------------
reg_aux <- lm(
  BF_months ~ factor(age) + sex +
    income + motherAge + motherEDU + mom_married +
    factor(race),
  data = NLSY_IQ
)

## ---- message=FALSE-----------------------------------------------------------
OVB.lm <- ovbias_lm(
  lm_shrt = reg_col1,
  lm_int = reg_col2,
  lm_aux = reg_aux,
  deltalow=deltalow, 
  deltahigh=deltahigh,
  Rhigh=Rhigh, 
  e=e)

## -----------------------------------------------------------------------------
(OVB.lm$bias_Distribution)

## -----------------------------------------------------------------------------
(OVB.lm$bstar_Distribution)

