naturalFrequencies <- function(rvflag, TP, FP, FN, TN){
  # browser()
  if(rvflag) return(NULL)
  
  TP <- 90
  FP <- 10
  FN <- 12
  TN <- 200
  
  # # push variables into the global environment for DiagrameR
  # TP <<- TP
  # FP <<- FP
  # FN <<- FN
  # TN <<- TN
  # rvflag <<- FALSE
  
  diagram <- DiagrammeR::grViz("
strict digraph graph2 {

graph [layout = dot]

# node definitions with substituted label text
node [shape = rectangle, width = 4, fillcolor = Biege]
a [label = '@@1']
b [label = '@@2']
c [label = '@@3']
d [label = '@@4']
e [label = '@@5']
f [label = '@@6']
g [label = '@@7']

a -> b  a -> c 
b -> d  b -> e
c -> f c  -> g
}

[1]: paste0('Population = ', (TP + FP + FN + TN))
[2]: paste0('Dpos = ', (TP + FN))
[3]: paste0('Dneg = ', (FP + TN))
[4]: paste0('TP = ', TP)
[5]: paste0('FN = ', FN)
[6]: paste0('FP = ', FP)
[7]: paste0('TN = ', TN)

")
  return(diagram)  
}

