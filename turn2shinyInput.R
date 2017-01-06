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
  f <- sapply(e,str_extract,"[[:alpha:]]+") #extracting the values
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

turn2shinyInputUIv2 <- function(x,x2,y,z){
  a <- mapply(function(x,x2,y,z) paste(x2,"Input(inputId=\"", x,"\", label = \"",y,"\", value = ",z,"),", sep = ""),x,x2,y,z)
  cat(a, sep="\n")
}

#global function
turn2shinyInput <- function(x){
  varNames <- extractVar(x)
  labelNames <- extractLabel(x)
  values <- extractValue(x)
  varTypes <- extractVarType(x)
  turn2shinyInputServer(varNames)
  turn2shinyInputUI(varNames, labelNames, values)
  turn2shinyInputUIv2(varNames,varTypes,labelNames,values) #v2 includes the use of variable type
}

#example usage
turn2shinyInput("kf = 0.9,            # maximum fatigue due to low proportion testing positive (0 to 1)
    # biological parameters
    omega = 1/2,         # rate of loss of immunity = 1/(average duration of immunity)     
    nuC = 365/10,        # rate of loss of symptoms in the absence of treatment
    nuA = 365/30,        # 1/(duration of super-microscopic asymtomatic infection)
    nuU = 365/120,")