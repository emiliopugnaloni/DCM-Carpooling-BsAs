#+++++++++++++++++++++++++++++++++
#+++++++ Apollo HMXL Model +++++++
#+++++++++++++++++++++++++++++++++

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #


### Clean & Folder setup
rm(list = ls())
setwd("C:/Users/epugnalo/OneDrive - Telefonica/Documents/Investigador UCA/Carpooling BsAs/Scripts/HDCM_Carpooling_Supp")
database = read.table("cpSupp_wide_epanel.txt")

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "cpool supp HMXL 20230728",
  modelDescr = "Car Pooling Hybrid MXL. Estimations set due to MXL & Apollo 0.1.0",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 8
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

# Run "cpool dem dataprep" script to generate "database"
database = read.csv("cpSupp_wide_epanel.txt", sep="\t")

#Delete IDs that select only 1 alternative
ids_dominancia = read.csv("dominancia_supply.csv")
ids_dominancia = ids_dominancia[ids_dominancia$n_uniques_alter_selected==1,"idLS"]
length(ids_dominancia) #58, 58x15 = 870

database =  database[!(database$ID %in% ids_dominancia), ] 

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(b0ca = 0,
              b0cp = -1.0,
#              b0ch = -1.1,
#              b0tr = -0.5,
              b0chmu  = -1.1, b0chsd = 0.5,
              b0trmu  = -0.5, b0trsd = 0.5,
              btimemu = -3, btimesd = 0.5,
              bcostmu = -3, bcostsd = 0.5,
 
              bL1 = 0,
              bL2 = 0,
              bL3 = 0,
              
              gamma1_male= 0.79,
              gamma1_age= 0,
              gamma1_age2= 0,
              #gamma1_age3= 0,
              gamma1_nse= 0,
              gamma1_modeCar=0,
              #gamma1_dist= 0,
              gamma2_male= 0,
              gamma2_age= 0,
              gamma2_age2= 0,
              #gamma2_age3= 0,
              gamma2_nse= 0.1,
              #gamma2_dist= 0,
              gamma2_modeCar=0,
              gamma3_male= 0.89,
              gamma3_age= 0.11,
              gamma3_age2= 0.11,
              # gamma3_age3= 0,
              gamma3_nse= 0,
              gamma3_dist= 0.16,
              gamma3_modeCar=0,


              lambda11=1.05,lambda15=1.37,lambda16=1,  #lambda12=1,lambda13=1,
               sigma11=1.53, sigma15=1.33, sigma16=1.68,  # sigma12=1, sigma13=1,

              lambda22=0.23,lambda24=0.24, lambda28=0.22,  #lambda21=1,lambda23=1,lambda25=1,lambda27=1, lambda26=1,
               sigma22=0.85, sigma24=1, sigma28=0.81,  # sigma21=1, sigma23=1, sigma25=1,  sigma27=1, sigma26=1,

              lambda32=0.8,lambda34=0.77, lambda36=0.81,  #lambda31=1,lambda33=1, lambda35=1,
               sigma32=0.87, sigma34=0.85, sigma36=1  # sigma31=1, sigma33=1, sigma35=1,
              )

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("b0ca")
apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed, "1 - MXL Modelo Base 20230324", overwriteFixed=FALSE) #levanto los de mnl


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "pmc",     #pmc | mlhs | halton
  interNDraws = 2000,         
  interUnifDraws = c(),      
  interNormDraws = c("eta1", "eta2", "eta3",
                   "draws_btime","draws_bcost",
                   "draws_b0ch", "draws_b0tr"),
  intraDrawsType = '',
  intraNDraws = 0,          
  intraUnifDraws = c(),     
  intraNormDraws = c()      
)

### Create random parameters
apollo_randCoeff <- function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["Z1"]] = (gamma1_male * male_cen +
                       gamma1_age * age_cen +
                       gamma1_age2 * age2_cen +
                       #gamma1_age3 * age3_cen +
                       gamma1_nse * income_cen +
                       #gamma1_dist * dist_cen +
                       gamma1_modeCar * modecar_cen +
                       eta1)
  randcoeff[["Z2"]] = (gamma2_male * male_cen +
                      gamma2_age * age_cen +
                      gamma2_age2 * age2_cen +
                      #gamma2_age3 * age3_cen +
                      gamma2_nse * income_cen +
                      #gamma2_dist * dist_cen +
                      gamma2_modeCar * modecar_cen +
                      eta2)
  randcoeff[["Z3"]] = (gamma3_male * male_cen +
                       gamma3_age * age_cen +
                       gamma3_age2 * age2_cen +
                       # gamma3_age3 * age3_cen +
                       gamma3_nse * income_cen +
                       gamma3_dist * dist_cen +
                       gamma3_modeCar * modecar_cen +
                       eta3)

  randcoeff[["b_time"]] = -exp(btimemu + btimesd * draws_btime)
  randcoeff[["b_cost"]] = -exp(bcostmu + bcostsd * draws_bcost)
  randcoeff[["b0ch"]] = b0chmu + b0chsd * draws_b0ch
  randcoeff[["b0tr"]] = b0trmu + b0trsd * draws_b0tr

    return(randcoeff)}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Likelihood of indicators
  
  # Passion for cars: V1, V5, V6
  set11 = list(outcomeNormal = cE1_1_cen, xNormal = lambda11*Z1,
               mu = 0, sigma = sigma11, rows = (tarea==1), componentName = "w11")
  set15 = list(outcomeNormal = cE1_5_cen, xNormal = lambda15*Z1,
               mu = 0, sigma = sigma15, rows = (tarea==1), componentName = "w15")
  set16 = list(outcomeNormal = cE1_6_cen, xNormal = lambda16*Z1,
               mu = 0, sigma = sigma16, rows = (tarea==1), componentName = "w16")
  P[["w11"]] = apollo_normalDensity(set11, functionality)
  P[["w15"]] = apollo_normalDensity(set15, functionality)
  P[["w16"]] = apollo_normalDensity(set16, functionality)
  
  # Sociability: V2, V4, V8
  set22 = list(outcomeNormal = cE3_2_cen, xNormal = lambda22*Z2,
               mu = 0, sigma = sigma22, rows = (tarea==1), componentName = "w22")
  set24 = list(outcomeNormal = cE3_4_cen, xNormal = lambda24*Z2,
               mu = 0, sigma = sigma24, rows = (tarea==1), componentName = "w24")
  #set26 = list(outcomeNormal = cE3_6_cen, xNormal = lambda26*Z2,
               #mu = 0, sigma = sigma26, rows = (tarea==1), componentName = "w26")
  set28 = list(outcomeNormal = cE3_8_cen, xNormal = lambda28*Z2,
               mu = 0, sigma = sigma28, rows = (tarea==1), componentName = "w28")
  P[["w22"]] = apollo_normalDensity(set22, functionality)
  P[["w24"]] = apollo_normalDensity(set24, functionality)
  #P[["w26"]] = apollo_normalDensity(set26, functionality)
  P[["w28"]] = apollo_normalDensity(set28, functionality)
  
  # Environment: V2, V4, V6
  set32 = list(outcomeNormal = cE4_2_cen, xNormal = lambda32*Z3,
               mu = 0, sigma = sigma32, rows = (tarea==1), componentName = "w32")
  set34 = list(outcomeNormal = cE4_4_cen, xNormal = lambda34*Z3,
               mu = 0, sigma = sigma34, rows = (tarea==1), componentName = "w34")
  set36 = list(outcomeNormal = cE4_6_cen, xNormal = lambda36*Z3,
               mu = 0, sigma = sigma36, rows = (tarea==1), componentName = "w36")
  P[["w32"]] = apollo_normalDensity(set32, functionality)
  P[["w34"]] = apollo_normalDensity(set34, functionality)
  P[["w36"]] = apollo_normalDensity(set36, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['ca']] = b0ca + b_time * time_ca + b_cost * cost_ca
  V[['cp']] = b0cp + b_time * time_cp + b_cost * cost_cp + bL1 * Z1 + bL2 * Z2 + bL3 * Z3
  V[['ch']] = b0ch + b_time * time_ch + b_cost * cost_ch
  V[['tr']] = b0tr + b_time * time_tr + b_cost * cost_tr
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(ca=1, cp=2, ch=3, tr=4), 
    avail = list(ca=1, cp=1, ch=1, tr=1), 
    choiceVar     = choice,
    V             = V
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs,
                        estimate_settings=list(hessianRoutine="maxLik"))  #numDeriv | maxLik | none

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

#model=apollo_loadModel('cpool supp HMXL 5_02')

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS                                          ----
# ----------------------------------------------------------------- #

forecast <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="indic_quality")

# ----------------------------------------------------------------- #
#---- CONDITIONALS AND UNCONDITIONALS                            ----
# ----------------------------------------------------------------- #

conditionals <- apollo_conditionals(model,apollo_probabilities,apollo_inputs)

unconditionals <- apollo_unconditionals(model,apollo_probabilities,apollo_inputs)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()

