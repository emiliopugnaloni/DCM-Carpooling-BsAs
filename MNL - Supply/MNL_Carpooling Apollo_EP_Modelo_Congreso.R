# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())
setwd("C:/Users/Usuario/OneDrive - Paradigma/Documentos/Investigador UCA/Carpooling BsAs/Scripts/MNL_Carpooling_Apollo")

### Load Apollo library
library(apollo)
library(dplyr)
library(tidyr)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="MNL_CP_Apollo",
  modelDescr ="Simple MNL model on mode choice carpoling",
  indivID    ="ID"
)


# ################################################################# #
####       2- LOAD DATA AND APPLY ANY TRANSFORMATIONS             ####
# ################################################################# #

database = read.csv("Oferta_Unido_Variables_Importantes.txt",header=TRUE)

# Esto esta comentado porque a partir de esto se creaba un archivo y con excel lo correguia. NO se porque salta un error de que las "filas no estan ordenadas" en apollo input
#                 database <- rename(database,ID=idLS)
#                 database <- select(database,-X)
#                 database <- mutate(database,alt_chr = case_when(alt == 1 ~ 'ca',
#                                                                alt == 2 ~ 'cp',
#                                                                alt == 3 ~ 'ch',
#                                                                alt == 4 ~ 'tr'))
#                 database <- arrange(database,ID,tarea)
#                 database<- pivot_wider(database,id_cols = c(ID, tarea, base_ca_t,base_cp_t,base_ch_t ,  base_tr_t , base_ca_c,  base_cp_c, base_ch_c, base_tr_c, base_cp_comp, DistFast , Vive_con_conyuge ,NSE ,EDAD ,  PasionAuto , Individualidad  ,  ecologia, DiasxMes_uso_auto, MotivoTrabajo ,Tiene_Hijos),
#                             names_from = alt_chr,
#                             values_from = c(time, cost, choice))
#                 database <-mutate(database,choice = 1*choice_ca+2*choice_cp+3*choice_ch+4*choice_tr) 
#                 database<-select(database,-choice_ca, -choice_cp, -choice_ch, -choice_tr) 
#                 write.csv(database,file="database.txt")

database = read.csv("OfertaUnido_Wide.csv",header=TRUE)

database$Joven<-0; database$Joven[database$EDAD<30]<-1; database$Adulto<-0;database$Adulto[database$EDAD>50]<-1
database$compensacion <- database$cost_ca - database$cost_cp
database$ahorroHOV <- database$time_ca - database$time_cp
database$NSEBajo<-0; database$NSEBajo[database$NSE>=4]<-1


# ################################################################# #
####       3- DEFINE MODEL PARAMETERS                           ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation. se ponen todos los parametros, y se los pone a 0.  nota: acordasre de mirar el cost_income_elast en algun apunte
apollo_beta=c(asc_ca = 0,
              asc_cp = 0,
              asc_ch = 0,
              asc_tr = 0,
              
              b_time_au_cp = 0,
              #b_time_cp = 0,
              b_time_ch_tp = 0,
              #b_time_tp = 0,
              
              b_cost_au_cp= 0,
              #b_cost_cp= 0,
              b_cost_ch_tp= 0,
              #b_cost_tp= 0,
              
              b_comp =0
              )




### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_ca")

### Read in starting values for at least some parameters from existing model output file. Lo que se hace es decirle a la maquina que empieze a estimar los parametros desde los valores que estimo de un modelo mas simple
      #apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed, "Apollo_example_1", overwriteFixed=FALSE)

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
  V[['ca']] = asc_ca + b_time_au_cp * time_ca + b_cost_au_cp * cost_ca
  V[['cp']] = asc_cp + b_time_au_cp  * time_cp + b_cost_au_cp * cost_cp +b_comp*compensacion/cost_ca 
  V[['ch']] = asc_ch + b_time_ch_tp * time_ch + b_cost_ch_tp* cost_ch
  V[['tr']] = asc_tr + b_time_ch_tp * time_tr +b_cost_ch_tp* cost_tr
  
  ### Define settings for MNL model component. Es un parametro que se necesita para hacer las probabilidades. Dentro de este se coloca todo lo que se ve
  mnl_settings = list(
    alternatives = c(ca=1, cp=2, ch=3, tr=4),
    avail        = list(ca=1, cp=1, ch=1, tr=1), ##Esto es para decir que alteernativas son validas para distinso usuarios"
    choiceVar    = choice,  ##Aca se dice la eleccion de cada fila
    V            = V        ##Aca se dice cuales son las Ut. de cada alternativa
  )
  
  ### Compute probabilities using MNL model  Esta funcion retorna la probabilidad. Necesita el mnl settings, que esta especificado arriba.
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
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
                        apollo_inputs)               ##Contiene info de que el modelo aanda massomenos bien

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

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- LR TEST AGAINST SIMPLE MNL MODEL                           ----
# ----------------------------------------------------------------- #

            # apollo_lrTest("Apollo_example_2", "Apollo_example_3")
            # apollo_lrTest("Apollo_example_2", model)



# ----------------------------------------------------------------- #
#---- 8 -   MODEL PREDICTIONS AND ELASTICITY CALCULATIONS              ----
# ----------------------------------------------------------------- #


# -------------- CURVA DE OFERTA -----------------------#


database$c_auto_sin_park <-database$base_cp_comp/0.4  #Se calcula el costo del auto sin park a paritr de la compensacion base (que es el 40% del costo sin parking)


database$time_ca<-database$base_ca_t
database$time_cp<-database$base_ca_t      #Sin compensacion HOV
database$time_ch<-database$base_ch_t
database$time_tr<-database$base_tr_t
database$cost_ca<-database$base_ca_c
database$cost_cp<-database$base_ca_c      #primero veo con compensacion de 0
database$cost_ch<-database$base_ch_c
database$cost_tr<-database$base_tr_c
database$compensacion<- database$cost_ca- database$cost_cp
database$ahorroHOV <-database$time_ca-database$time_cp

database$choice<-0
database<-select(database,-c(tarea))
database<- database[!duplicated(database),]

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate")  #esto es una funcion que se crea y al final retorna probabilidades
{            
  ###  Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)         #lo que hace esto es que permite llamar a los database$income como solo income
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P. Va a contener una lista de probabilidades
  P = list()  
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant

  V = list()
  V[['ca']] = asc_ca + b_time_au_cp * time_ca + b_cost_au_cp * cost_ca
  V[['cp']] = asc_cp + b_time_au_cp  * time_cp + b_cost_au_cp * cost_cp +b_comp*compensacion/cost_ca 
  V[['ch']] = asc_ch + b_time_ch_tp * time_ch + b_cost_ch_tp* cost_ch
  V[['tr']] = asc_tr + b_time_ch_tp * time_tr +b_cost_ch_tp* cost_tr
  
  ### Define settings for MNL model component. Es un parametro que se necesita para hacer las probabilidades. Dentro de este se coloca todo lo que se ve
  mnl_settings = list(
    alternatives = c(ca=1, cp=2, ch=3, tr=4),
    avail        = list(ca=1, cp=1, ch=1, tr=1), ##Esto es para decir que alteernativas son validas para distinso usuarios"
    choiceVar    = choice,  ##Aca se dice la eleccion de cada fila
    V            = V        ##Aca se dice cuales son las Ut. de cada alternativa
  )
  
  ### Compute probabilities using MNL model  Esta funcion retorna la probabilidad. Necesita el mnl settings, que esta especificado arriba.
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_inputs = apollo_validateInputs()
"Compensacion =0%"
predictions_new = apollo_prediction(model, apollo_probabilities, apollo_inputs)

"Compensacion = 10%"
database$cost_cp<-database$base_ca_c - 0.1*database$c_auto_sin_park
database$compensacion<-0.1*database$c_auto_sin_park
apollo_inputs = apollo_validateInputs()
predictions_new = apollo_prediction(model, apollo_probabilities, apollo_inputs)


i<-0
for (i in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
{
  
  
  database$cost_cp<-database$base_ca_c - i*database$c_auto_sin_park
  database$compensacion<-i*database$c_auto_sin_park
  
  apollo_inputs = apollo_validateInputs()
  print(paste("                                 Compensacion = ",i))
  predictions_new = apollo_prediction(model, apollo_probabilities, apollo_inputs)
  
  
}





### 5) Otras cosas no vistas: Work with predictions at estimates
predictions_base=predictions_base[["at_estimates"]]
### Compute change in probabilities
change=(predictions_new-predictions_base)/predictions_base

### Not interested in chosen alternative now, so drop last column
change=change[,-ncol(change)]
### First two columns (change in ID and task) also not needed
change=change[,-c(1,2)]

### Look at first individual
change[database$ID==1,]
### And person 9, who has all 4 modes available
change[database$ID==9,]

### Summary of changes (possible presence of NAs for unavailable alternatives)
summary(change)

### Look at mean changes for subsets of the data, ignoring NAs
colMeans(change,na.rm=TRUE)
colMeans(subset(change,database$business==1),na.rm=TRUE)
colMeans(subset(change,database$business==0),na.rm=TRUE)
colMeans(subset(change,(database$income<quantile(database$income,0.25))),na.rm=TRUE)
colMeans(subset(change,(database$income>=quantile(database$income,0.25))|(database$income<=quantile(database$income,0.75))),na.rm=TRUE)
colMeans(subset(change,(database$income>quantile(database$income,0.75))),na.rm=TRUE)

### Compute own elasticity for rail:
log(sum(predictions_new[,6])/sum(predictions_base[,6]))/log(1.01)

### Compute cross-elasticities for other modes
log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.01)
log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.01)
log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.01)



# ----------------------------------------------------------------- #
#---- RECOVERY OF SHARES FOR ALTERNATIVES IN DATABASE            ----
# ----------------------------------------------------------------- #

sharesTest_settings = list()
sharesTest_settings=list()
sharesTest_settings[["alternatives"]] = c(ca=1, cp=2, ch=3, tr=4)
sharesTest_settings[["choiceVar"]]    = database$choice


apollo_sharesTest(model,apollo_probabilities,apollo_inputs,sharesTest_settings)

# ----------------------------------------------------------------- #
#---- MODEL PERFORMANCE IN SUBSETS OF DATABASE                   ----
# ----------------------------------------------------------------- #

fitsTest_settings = list()

fitsTest_settings[["subsamples"]] = list()
fitsTest_settings$subsamples[["business"]] = database$business==1
fitsTest_settings$subsamples[["leisure"]] = database$business==0
apollo_fitsTest(model,apollo_probabilities,apollo_inputs,fitsTest_settings)

# ----------------------------------------------------------------- #
#---- FUNCTIONS OF MODEL PARAMETERS                              ----
# ----------------------------------------------------------------- #

deltaMethod_settings=list(operation="ratio", parName1="b_tt_car", parName2="b_cost")
apollo_deltaMethod(model, deltaMethod_settings)

deltaMethod_settings=list(operation="ratio", parName1="b_tt_car", parName2="b_cost", multPar1 = 60)
apollo_deltaMethod(model, deltaMethod_settings)

deltaMethod_settings=list(operation="diff", parName1="b_tt_car", parName2="b_tt_rail")
apollo_deltaMethod(model, deltaMethod_settings)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()

