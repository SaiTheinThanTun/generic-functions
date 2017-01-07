#this function transforms the parameters vector, which is to be used with deSolve function,
#into shiny input formats
library(stringr)

extractVar <- function(x){
  a <- strsplit(x,"\n") #splitting the entire string by newline character, putting them in a list
  b <- lapply(a,str_trim,side="left") #using apply to trim the list of character vectors
  d <- lapply(b,grep,pattern="^[a-zA-Z]", value=T)  #if the string doesn't start with a variable, discard that string
  e <- sapply(d,word,1) #extracting the first word, which is the variable name
  #lapply above is sure to work
  e
}

extractLabel <- function(x){
  a <- strsplit(x,"\n") #splitting the entire string by newline character, putting them in a list
  b <- lapply(a,str_trim,side="both") #using apply to trim the list of character vectors
  d <- lapply(b,grep,pattern="^[a-zA-Z]", value=T)  #if the string doesn't start with a variable, discard that string
  e <- sapply(d,str_extract,"#.*") #extracting the comments starting with #
  #[later version must end in @ where VarType begins]
  e
}

extractValue <- function(x){
  a <- strsplit(x,"\n") #splitting the entire string by newline character, putting them in a list
  b <- lapply(a,str_trim,side="left") #using apply to trim the list of character vectors
  d <- lapply(b,grep,pattern="^[a-zA-Z]", value=T)  #if the string doesn't start with a variable, discard that string
  e <- lapply(d,str_extract,"[=<].*[,#]?") #extracting the values starting with = and ending in ,
  f <- sapply(e,str_extract,"[[:digit:]]+[./]?[Ee]?[+]?[[:digit:]]*") #extracting the values
  f
}


extractVarType <- function(x){
  a <- strsplit(x,"\n") #splitting the entire string by newline character, putting them in a list
  b <- lapply(a,str_trim,side="left") #using apply to trim the list of character vectors
  d <- lapply(b,grep,pattern="^[a-zA-Z]", value=T)  #if the string doesn't start with a variable, discard that string
  e <- lapply(d,str_extract,"@.*") #extracting the values starting with @ which must be at the end of the comment sentence
  f <- sapply(e,str_extract,"[[:alpha:]]+") #extracting the varTypes
  f
}

extractVarTypeLisa <- function(x){
  a <- strsplit(x,"\n") #splitting the entire string by newline character, putting them in a list
  b <- lapply(a,str_trim,side="left") #using apply to trim the list of character vectors
  d <- lapply(b,grep,pattern="^[a-zA-Z]", value=T)  #if the string doesn't start with a variable, discard that string
  e <- lapply(d,str_extract,"#.*") #extracting the comments starting with #
  f <- sapply(e,str_extract,"\\[.*\\]") #extracting the values starting with @ which must be at the end of the comment sentence
  #f <- sapply(e,str_extract,"[[:alpha:]]+") #extracting the varTypes
  #f
  f
}

turn2shinyInputServer <- function(y){
  a <- lapply(y,function(x) paste(x," = input$",x,",", sep = ""))
  cat(unlist(a), sep="\n")
}


turn2shinyInputUI <- function(x,y,z){
  a <- mapply(function(x,y,z) paste("Input(inputId=\"", x,"\", label = \"",y,"\", value = ",z,"),", sep = ""),x,y,z)
  cat(a, sep="\n")
}

#v2 has slider min and max values
turn2shinyInputUIv2 <- function(x,x2,y,z, begins, ends){
  a <- mapply(function(x,x2,y,z, begins, ends) paste(x2,"Input(inputId=\"", x,"\", label = \"",y,"\", value = ",z, ", min=", begins,", max=", ends,"),", sep = ""),x,x2,y,z, begins, ends)
  cat(a, sep="\n")
}

#v3 turns checkboxes values into logical
turn2shinyInputUIv3 <- function(x,x2,y,z, begins, ends){
  a <- mapply(function(x,x2,y,z, begins, ends) paste(x2,"Input(inputId=\"", x,"\", label = \"",y,"\", value = ",z, ", min=", begins,", max=", ends,"),", sep = ""),x,x2,y,z, begins, ends)
  a[grep("checkbox",a)] <- gsub("[[:alnum:]], min=NA, max=NA","TRUE",a[grep("checkbox",a)] )
  cat(a, sep="\n")
}

#global function
turn2shinyInput <- function(x, var_type=TRUE){
  varNames <- extractVar(x)
  labelNames <- extractLabel(x)
  values <- extractValue(x)
  varTypes <- extractVarTypeLisa(x)
  turn2shinyInputServer(varNames)
  if(var_type==TRUE){
    nopick <- which(varTypes!="[N]") #currently "[N]" is assigned as not to be picked, will have conflict with numeric var in the future!
    varNames <- varNames[nopick]
    varTypes <-  varTypes[nopick]
    labelNames <- labelNames[nopick]
    values <- values[nopick]
    
    varTypes <- gsub("\\[C\\]", "checkbox", varTypes)
    slideValues <- str_extract(varTypes,"\\[[[:digit:]]*.to.[[:digit:]]*\\]") #extracting slider values
    varTypes <- gsub("\\[[[:digit:]]*.to.[[:digit:]]*\\]","slider", varTypes)

    begins <-  str_extract(slideValues, "[[:digit:]]+")
    ends <- str_trim(str_extract(slideValues,"[ ][[:digit:]]+"), side="left")
    
    turn2shinyInputUIv3(varNames,varTypes,labelNames,values,begins,ends) #v2 includes the use of variable type
  }
  else {
    turn2shinyInputUI(varNames, labelNames, values)
  }
  
}

#example usage
# turn2shinyInput("kf = 0.9,            # maximum fatigue due to low proportion testing positive (0 to 1)
#     # biological parameters
#     omega = 1/2,         # rate of loss of immunity = 1/(average duration of immunity)     
#     nuC = 365/10,        # rate of loss of symptoms in the absence of treatment
#     nuA = 365/30,        # 1/(duration of super-microscopic asymtomatic infection)
#     nuU = 365/120,")

turn2shinyInput("    # process indicators
    timei = 2018,                # timing of intervention [N]
    EDATon = scenario[1],        # switch on scale up of EDAT [C]
    EDATscale = 3,               # years to scale up EDAT [1 to 3]
    covEDATi = 90,               # new percentage of all villages covered by VMW [0 to 100]
    ITNon = scenario[2],         # switch on scale up of ITN [C]
    ITNscale = 0.5,              # years to scale up ITN [1 to 3]
    covITNi = 90,                # new coverage of ITN (%) [0 to 90]
    effITN = 30,                 # percentage of new infections averted due to owenership of ITN [0 to 50]
    RCDon = scenario[3],         # switch on scale up of RCD default radial search   [C] 
    RCDscale = 2,                # years to scale up RCD [1 to 3]
    RCDcoex = scenario[4],     # Change RCD to co-exposure search   [C] 
    covRCDi = 50,                # new coverage of RCD (%) [0 to 100]
    effRCD = 1,                  # number of additional clinical cases found for each index case [0 to 2] 
    IRSon = scenario[5],         # switch on scale up of IRS [C]  
    IRSscale = 1,                # years to scale up IRS [1 to 3]
    covIRSi = 90,                # new coverage of IRS (%) [0 to 90]")