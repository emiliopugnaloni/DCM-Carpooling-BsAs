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
library(ggplot2)
library(ggthemes)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="5 - MXL Modelo Distancia",
  modelDescr ="Modelo MXl DistFast ",
  mixing=TRUE,
  indivID    ="idLS",
  nCores = 3
)


# ################################################################# #
####       2- LOAD DATA AND APPLY ANY TRANSFORMATIONS             ####
# ################################################################# #

database = read.table("OfertaEpanel.txt", header=T, sep="\t",dec=".")




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
              
              b_DistFast=0

              )



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_ca")

### Read in starting values for at least some parameters from existing model output file. Lo que se hace es decirle a la maquina que empieze a estimar los parametros desde los valores que estimo de un modelo mas simple
     apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed, "1 - MXL Modelo Base", overwriteFixed=FALSE)


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_b0cp","draws_b0ch","draws_b0tr","draws_btime","draws_bcost"),
  intraDrawsType = "pmc",
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
  V[['cp']] = (asc_cp + 
                        b_DistFast * DistFast

               ) 
                                              + b_time * time_cp + b_cost * cost_cp 
  
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


# ---------ñ-------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)



# ################################################################# #
##### Distirbucion de Parametros                                       ####
# ################################################################# #


############ ASC

#CP
mu_b<- model$estimate[[2]]
sd_b<- model$estimate[[3]]

p_mayor_0 <- pnorm(0, mean=mu_b, sd=sd_b,lower.tail = FALSE)

ASC_CP <- ggplot( data.frame( x= c(-10,10)), aes(x=x))  +
      stat_function(fun=dnorm, args = list(mu_b,sd_b)) +
      stat_function( fun=dnorm, geom="area", fill="steelblue", xlim =c(0,10), args = list(mu_b,sd_b))+
      theme_light()

ASC_CP



##Ch
mu_b<- model$estimate[[4]]
sd_b<- abs(model$estimate[[5]])

p_mayor_0 <- pnorm(0, mean=mu_b, sd=sd_b, lower.tail = FALSE)

ASC_CH <- ggplot( data.frame( x= c(-10,10)), aes(x=x))  +
  stat_function(fun=dnorm, args = list(mu_b,sd_b)) +
  stat_function( fun=dnorm, geom="area", fill="steelblue", xlim =c(0,10), args = list(mu_b,sd_b))+
  theme_light()

ASC_CH

##TR
mu_b<- model$estimate[[6]]
sd_b<- abs(model$estimate[[7]])

p_mayor_0 <- pnorm(0, mean=mu_b, sd=sd_b, lower.tail = FALSE)

ASC_TR <- ggplot( data.frame( x= c(-10,10)), aes(x=x))  +
  stat_function(fun=dnorm, args = list(mu_b,sd_b)) +
  stat_function( fun=dnorm, geom="area", fill="steelblue", xlim =c(0,10), args = list(mu_b,sd_b))+
  theme_light()

ASC_TR

############ Bt y Bc

mu_normal_t<-abs(model$estimate[[8]])
sd_normal_t<-abs(model$estimate[[9]])
mu_b_t <- exp( mu_normal_t + 1/2* sd_normal_t^2)
sd2_b_t <- exp(2*mu_normal_t + sd_normal_t^2) * (exp(sd_normal_t^2)-1)

mu_normal_c<-abs(model$estimate[[10]])
sd_normal_c<-abs(model$estimate[[11]])
mu_b_c <- exp( mu_normal_c + 1/2* sd_normal_c^2)
sd2_b_c <- exp(2*mu_normal_c + sd_normal_c^2) * (exp(sd_normal_c^2)-1)

mu_normal_WTP = abs(abs(mu_normal_t)-abs(mu_normal_c))
sd2_normal_WTP = sd_normal_t^2 + sd_normal_c^2

mu_WTP_ln = exp(mu_normal_WTP + 0.5*sd2_normal_WTP)
sd2_WTP_ln = exp(2*mu_normal_WTP + sd2_normal_WTP) * (exp(sd2_normal_WTP)-1)

###
B <- ggplot( data.frame( x= c(-10,10)), aes(x=x))  +
  stat_function(fun = dnorm, args = list(mean = -3.19, sd = 1.22), color = "black") 
B
B <- ggplot( data.frame( x= c(0,10)), aes(x=x))  +
  stat_function(fun = dlnorm, args = list(meanlog = 0.05, sdlog = 0.0023), color = "black") 
B


B <- ggplot( data.frame( x= c(0,100)), aes(x=x))  +
  stat_function(fun = dlnorm, args = list(meanlog = mu_WTP_ln, sdlog = sqrt(sd2_WTP_ln)), color = "black") +
  #stat_function(fun = dlnorm, args = list(meanlog = mu_b_c, sdlog = sqrt(sd2_b_c)), color = "orange") +
  #stat_function(fun = dlnorm, args = list(meanlog = mu_b_t, sdlog =sqrt(sd2_b_t)), color = "cornflowerblue")+
  scale_x_continuous(name="$/min", limits= c(0,20))+
  scale_y_continuous(name="Probabilidad",labels = scales::percent_format(accuracy = 1), limits=c(0,0.1))+
  ggtitle("Willing to pay")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))

B

exp(mu_normal_WTP)
exp(mu_normal_t)/exp(mu_normal_c)
#####




# ################################################################# #
##### GRAFICAS ASC                                  ####
# ################################################################# #
mu_cp<- model$estimate[[2]]
sd_cp<- model$estimate[[3]]
mu_ch<- model$estimate[[4]]
sd_ch<- abs(model$estimate[[5]])
mu_tp<- model$estimate[[6]]
sd_tp<- abs(model$estimate[[7]])

p_mayor_0_cp <- pnorm(0, mean=mu_cp, sd=sd_cp, lower.tail = FALSE)
p_mayor_0_ch <- pnorm(0, mean=mu_ch, sd=sd_ch, lower.tail = FALSE)
p_mayor_0_tr <- pnorm(0, mean=mu_tp, sd=sd_tp, lower.tail = FALSE)

Graficas <- ggplot( data.frame( x= c(-10,7)), aes(x=x))  +
  stat_function(fun=dnorm, args = list(mu_ch,sd_ch), color="dodgerblue3") +
  stat_function( fun=dnorm, geom="area", fill="steelblue", xlim =c(0,10), args = list(mu_ch,sd_ch))+
  scale_x_continuous(name="Utilidad")+
  scale_y_continuous(name="Probabilidad",labels = scales::percent_format(accuracy = 1), limits=c(0,0.2))+
  ggtitle("Utilidad parcial del charter vs auto")+
  theme_gray()+
  theme(plot.title = element_text(hjust = 0.4))
  


  

Graficas



# ################################################################# #
##### Comparar con Modelo                                      ####
# ################################################################# #

#Modelo Actual MXL

mu_normal<-model$estimate[[8]]
sd_normal<-(model$estimate[[9]])
mu_b_t <- -exp( mu_normal + 1/2* sd_normal^2)
sd_b_t <- -exp(2*mu_normal + sd_normal^2) * (exp(sd_normal^2)-1)

mu_normal<-model$estimate[[10]]
sd_normal<-(model$estimate[[11]])
mu_b_c <- -exp( mu_normal + 1/2* sd_normal^2)
sd_b_c <- -exp(2*mu_normal + sd_normal^2) * (exp(sd_normal^2)-1)


apollo_modelOutput(model,modelOutput_settings)
mu_b_t
sd_b_t
mu_b_c
sd_b_c




#Modelo Base MNL
model_MNL = apollo_loadModel("MNL_CP_Apollo")
apollo_modelOutput(model_MNL,modelOutput_settings)


#likehood ratio test
apollo_lrTest("MNL_CP_Apollo",model)

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
#sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

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
