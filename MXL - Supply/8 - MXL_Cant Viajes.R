# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())
setwd("C:/Users/emiba/OneDrive - UCA/Documentos/Investigador UCA/Carpooling BsAs/Scripts/MXL_Carpooling_Apollo_epanel3")


### Load Apollo library
library(apollo)
library(dplyr)
library(tidyr)


### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="8 - MXL Modelo viajes (binaria)",
  modelDescr ="Modelo MXl de cant veces q va a bsas por semana <4",
  mixing=TRUE,
  indivID    ="idLS",
  nCores = 3
)


# ################################################################# #
####       2- LOAD DATA AND APPLY ANY TRANSFORMATIONS             ####
# ################################################################# #

database = read.table("OfertaEpanel.txt", header=T, sep="\t",dec=".")
database$CantViajesMenor4 = 0; database$CantViajesMenor4[ database$Viajes_por_Semana < 4/7] <-1

# ################################################################# #
####       3- DEFINE MODEL PARAMETERS                           ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation. se ponen todos los parametros, y se los pone a 0.  nota: acordasre de mirar el cost_income_elast en algun apunte
apollo_beta=c(asc_ca = 0,
              mu_asc_cp = 0,
              sd_asc_cp=0,
              mu_asc_ch = -1,
              sd_asc_ch=0,
              mu_asc_tr =-1,
              sd_asc_tr=0,
              
              mu_time = -3,
              sd_time=0,
              mu_cost = -3,
              sd_cost=0,
              
              b_cant_viajes_menor_4=0

              
)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_ca")

### Read in starting values for at least some parameters from existing model output file. Lo que se hace es decirle a la maquina que empieze a estimar los parametros desde los valores que estimo de un modelo mas simple
apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed, "1 - MXL Modelo Base", overwriteFixed=FALSE)
#Este comando lee los Bi que se estimaron para el modelos base (si se guardo el modelo) asi estima mas rapido

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_b0cp","draws_b0ch","draws_b0tr","draws_btime","draws_bcost"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["asc_cp"]] = mu_asc_cp + sd_asc_cp * draws_b0cp
  randcoeff[["asc_ch"]] = mu_asc_ch + sd_asc_ch * draws_b0ch
  randcoeff[["asc_tr"]] = mu_asc_tr + sd_asc_tr * draws_b0tr
  randcoeff[["b_time"]] = -exp(mu_time + sd_time * draws_btime)
  randcoeff[["b_cost"]] = -exp(mu_cost + sd_cost * draws_bcost)
  
  
  return(randcoeff)
}

# ################################################################# #
####       4 - GROUP AND VALIDATE INPUTS                         ####  Lo que hace sto es validar de que el database, apollobeta, apollocontrol y otras cosa anden. Ademas añade una columna para identificar la cant de obs por ID
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
####   5 - DEFINE MODEL AND LIKELIHOOD FUNCTION                  ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){  #esto es una funcion que se crea y al final retorna probabilidades
  
  ###  Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)         #lo que hace esto es que permite llamar a los database$income como solo income
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P. Va a contener una lista de probabilidades
  P = list()    
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['ca']] = asc_ca  + b_time * time_au + b_cost * cost_au
  V[['cp']] = (asc_cp + b_cant_viajes_menor_4 * CantViajesMenor4 ) + b_time * time_cp + b_cost * cost_cp 
  V[['ch']] = asc_ch + b_time * time_ch + b_cost * cost_ch
  V[['tr']] = asc_tr + b_time * time_tr + b_cost * cost_tr
  
  ### Define settings for MNL model component. Es un parametro que se necesita para hacer las probabilidades. Dentro de este se coloca todo lo que se ve
  mnl_settings = list(
    alternatives = c(ca=1, cp=2, ch=3, tr=4),
    avail        = list(ca=1, cp=1, ch=1, tr=1), ##Esto es para decir que alteernativas son validas para distinso usuarios"
    choiceVar    = DChoice,  ##Aca se dice la eleccion de cada fila
    V            = V        ##Aca se dice cuales son las Ut. de cada alternativa
  )
  
  ### Compute probabilities using MNL model  Esta funcion retorna la probabilidad. Necesita el mnl settings, que esta especificado arriba.
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
  
}

# ################################################################# #
####   6-   MODEL ESTIMATION                                    ####
# ################################################################# #

model = apollo_estimate(apollo_beta,                 ##Contiene info de desde donde se va a empezar a estiamar los parametros 
                        apollo_fixed,                ##Contiente los parametros que se mantienen fijos
                        apollo_probabilities,        ##Contiene las funciones de probabilidad del modelo, las formulas de utilidades
                        apollo_inputs,              ##Contiene info de que el modelo aanda massomenos bien
                        estimate_settings=list(hessianRoutine="maxLik"))   ##es una fomra de estimar le hessiano

#model = apollo_loadModel("1 - MXL Modelo Base")

# ################################################################# #
####  7 -   MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #
modelOutput_settings=list(printPVal=1)
apollo_modelOutput(model,modelOutput_settings)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)



#Modelo Base MXL
model_MXL_base = apollo_loadModel("1 - MXL Modelo Base")
apollo_modelOutput(model_MXL_base,modelOutput_settings)


#likehood ratio test
apollo_lrTest("1 - MXL Modelo Base",model)
