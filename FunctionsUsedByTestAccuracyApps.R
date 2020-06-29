##########################################################
# FunctionsUsedByTestAccuracyApp.R()
#
# non-reactive functions used by the Shiny App to explore clinical accuracy measurements
#
# installation of colour picker from htTP://deanattali.com/blog/plot-colour-helper/
#
# install.packages("devtools")
# devtools::install_github("daattali/colourpicker")
#
##########################################################
#
# load packages used by the App
LoadPackages <- function() {
  library(shiny)
  library(tidyverse) # Imports: broom, DBI, dplyr, forcats, ggplot2, haven, httr, hms, jsonlite, lubridate, magrittr, modelr, purrr, readr, readxl, stringr, tibble, rvest, tidyr, xml2
  library(rsconnect) # needed to upload to Shinyio
  library(readr) # needed for GET()
  library(vcd) # mosaic() plot htTP://www.statmethods.net/advgraphs/mosaic.html
  library(colourpicker) # htTP://deanattali.com/blog/plot-colour-helper/
  library(shinythemes)
  library(DT)
  library(knitr)
  library(rmarkdown)
  library(shinycssloaders)
  #      library(proportion)  package no longer being maintained :-(
  # library(PropCIs)
  library(binom)
  library(ggrepel)
  library(glue)
  library(plotly)
  library(gt)
  library(DiagrammeR)
  # ...
}

marginInsidePlot <- 0.01
#################### confidence interval on a proportion ###############################
#
### this is a wrapper to allow the choice of CI method to be easily changed
#
# use PropCIs::scoreci() to calculate Wilson's confidence interval for a single proportion.
#             Score CI based on inverting the asymptotic normal test using the null standard error
# Arguments:
# x	           Number of successes
# n            Total sample size
# conf.level   Confidence level

ciprop <- function(x, n, alpha = 0.05) {
  if (x > n | x < 0 | n <= 0) {
    message(paste("CI set to [NA, NA] because ciprop called with", x, n))
    # stop("CI set to NA - NA")
    return(
      data.frame(
        ciL = NA,
        ciU = NA
      )
    )
  }

  CIs <- binom::binom.confint(x, n, conf.level = 0.95, methods = "agresti-coull")
  CI <- data.frame(
    ciL = max(0, CIs[CIs$method == "agresti-coull", ]$lower),
    ciU = min(1, CIs[CIs$method == "agresti-coull", ]$upper)
  )
  return(CI)
}

#
# sensitivity <- 0.7
# specificity <- 0.9
# n <- 1000
# prevalence <- 0.15


TPfn <- function(sensitivity, specificity, n, prevalence) {
  if (!is.null(sensitivity) & !is.null(specificity) & !is.null(n) & !is.null(prevalence)) {
    n * prevalence * sensitivity
  }
}

FPfn <- function(sensitivity, specificity, n, prevalence) {
  if (!is.null(sensitivity) & !is.null(specificity) & !is.null(n) & !is.null(prevalence)) {
    n * (1 - prevalence) * (1 - specificity)
  }
}

FNfn <- function(sensitivity, specificity, n, prevalence) {
  if (!is.null(sensitivity) & !is.null(specificity) & !is.null(n) & !is.null(prevalence)) {
    n * prevalence * (1 - sensitivity)
  }
}

TNfn <- function(sensitivity, specificity, n, prevalence) {
  if (!is.null(sensitivity) & !is.null(specificity) & !is.null(n) & !is.null(prevalence)) {
    n * (1 - prevalence) * specificity
  }
}




DxStats <- function(n, prevalence, sensitivity, specificity) {
  # prevalence <- min(prevalence,0.9999)
  # prevalence <- max(prevalence,0.0001)

  Dpos <- n * prevalence
  Dneg <- n - Dpos

  TP <- round(sensitivity * round(Dpos))
  TN <- round(specificity * round(Dneg))

  FN <- round(Dpos) - round(round(Dpos) * sensitivity)
  FP <- round(Dneg) - round(round(Dneg) * specificity)

  sensitivity <- sensitivity
  specificity <- specificity

  PPV <- TP / (TP + FP)
  NPV <- TN / (TN + FN)

  LRp <- sensitivity / (1 - specificity)
  LRn <- (1 - sensitivity) / (specificity)

  PreTestOddsP <- prevalence / (1 - prevalence)
  PreTestOddsN <- (prevalence) / (1 - prevalence)

  PostTestOddsP <- PreTestOddsP * LRp
  PostTestOddsN <- PreTestOddsN * LRn

  PostTesTProbP <- PostTestOddsP / (PostTestOddsP + 1) # = PPV
  PostTesTProbN <- PostTestOddsN / (PostTestOddsN + 1) # = (1 - NPV)

  cidf <- ciprop(TP, TP + TP) # CI for post-positive test probability
  PPV_ciL <- cidf$ciL
  PPV_ciU <- cidf$ciU

  cidf <- ciprop(TN, TN + FN) # CI for post-negative test probability
  NPV_ciL <- cidf$ciL
  NPV_ciU <- cidf$ciU

  data.frame(
    Dpos = Dpos,
    Dneg = Dneg,

    TP = TP,
    TN = TN,

    FN = FN,
    FP = FP,

    PPV = PPV,
    PPV_ciL = PPV_ciL,
    PPV_ciU = PPV_ciU,

    NPV = NPV,
    NPV_ciL = NPV_ciL,
    NPV_ciU = NPV_ciU,

    LRp = LRp,
    LRn = LRn

    # PreTestOddsP = PreTestOddsP,
    # PreTestOddsN = PreTestOddsN,
    #
    # PostTestOddsP = PostTestOddsP,
    # PostTestOddsN = PostTestOddsP,
    #
    # PostTesTProbP = PostTesTProbP,
    # PostTesTProbN = PostTesTProbN,
  )
}

# prevalence needs to be the 1st argument using map() to call DxStatsExact()
prev1st <- function(prevalence, sensitivity, specificity, n) {
  DxStatsExact(sensitivity, specificity, n, prevalence)
}


DxStatsExact <- function(sensitivity, specificity, n, prevalence) {

  # # remove comments for debugging
  # n <- 719
  # prevalence <- 0
  # sensitivity <- 0.758
  # specificity <- 0.853

  Dpos <- n * prevalence
  Dneg <- n - Dpos

  TP <- sensitivity * Dpos
  TN <- specificity * Dneg

  FN <- Dpos - Dpos * sensitivity
  FP <- Dneg - Dneg * specificity

  PPV <- TP / (TP + FP)
  NPV <- TN / (TN + FN)

  LRp <- sensitivity / (1 - specificity)
  LRn <- (1 - sensitivity) / (specificity)


  # CI for PPV
  PPV_ciL <- ciprop(TP, (TP + FP))$ciL
  PPV_ciU <- ciprop(TP, (TP + FP))$ciU

  # CI for NPV
  NPV_ciL <- ciprop(TN, (TN + FN))$ciL
  NPV_ciU <- ciprop(TN, (TN + FN))$ciU

  # CI for sensitivity
  sens_ciL <- ciprop(TP, (TP + FN))$ciL
  sens_ciU <- ciprop(TP, (TP + FN))$ciU
  if ((TP + FN) == 0) {
    sens_ciL <- sensitivity
    sens_ciU <- sensitivity
  }

  # CI for specificity
  specificity <- specificity
  spec_ciL <- ciprop(TN, (TN + FP))$ciL
  spec_ciU <- ciprop(TN, (TN + FP))$ciU
  if ((TN + FP) == 0) {
    spec_ciL <- specificity
    spec_ciU <- specificity
  }

  data.frame(
    Dpos = Dpos,
    Dneg = Dneg,

    TP = TP,
    TN = TN,

    FN = FN,
    FP = FP,

    sensitivity = sensitivity,
    sens_ciL = sens_ciL,
    sens_ciU = sens_ciU,

    specificity = specificity,
    spec_ciL = spec_ciL,
    spec_ciU = spec_ciU,

    prevalence = prevalence,
    prev_ciL = ciprop(TP, n)$ciL,
    prev_ciU = ciprop(TP, n)$ciU,

    PPV = PPV,
    PPV_ciL = PPV_ciL,
    PPV_ciU = PPV_ciU,

    NPV = NPV,
    NPV_ciL = NPV_ciL,
    NPV_ciU = NPV_ciU,

    LRp = LRp,
    LRn = LRn

    # PreTestOddsP = PreTestOddsP,
    # PreTestOddsN = PreTestOddsN,
    #
    # PostTestOddsP = PostTestOddsP,
    # PostTestOddsN = PostTestOddsP,
    #
    # PostTesTProbP = PostTesTProbP,
    # PostTesTProbN = PostTesTProbN
  )
}




linesDf <- function(n, prevalence, sensitivity, specificity) {
  Dx <- DxStatsExact(sensitivity, specificity, n, prevalence)

  # Dpos <- round((n * input$prevalence))
  #  Dneg <- round(input$n - Dpos)
  # FN <- round((1 - input$sensitivity) * Dpos)
  # TN <- round(input$specificity * Dneg)

  return({
    data.frame(
      ### define computed line segments for vertical line separating Dpos from Dneg
      vx = Dx$Dpos / n,
      vxlci = ciprop(Dx$Dpos, n)$ciL,
      vxuci = ciprop(Dx$Dpos, n)$ciU,

      ### define computed line segments for horizontal lines separating TesTPos from TesTNeg
      hy1 = Dx$FN / Dx$Dpos,
      hy1lci = ciprop(Dx$FN, Dx$Dpos)$ciL,
      hy1uci = ciprop(Dx$FN, Dx$Dpos)$ciU,

      hy2 = Dx$TN / Dx$Dneg,
      hy2lci = ciprop(Dx$TN, Dx$Dneg)$ciL,
      hy2uci = ciprop(Dx$TN, Dx$Dneg)$ciU
    )
  })
}


dx2x2Table <- function(sensitivity, specificity, n, prevalence) {
  Dx <- DxStatsExact(sensitivity, specificity, n, prevalence)
  table <- data.frame(
    . = c("Test positive", "Test negative", "Totals"),
    ConditionPresent = c(Dx$TP, Dx$FN, Dx$Dpos),
    ConditionAbsent = c(Dx$FP, Dx$TN, Dx$Dneg),
    Totals = c(Dx$TP + Dx$FP, Dx$FN + Dx$TN, n)
  )
  colnames(table) <- c("", "Condition present", "Condition absent", "Total")
  return(table)
}

dxStatsTable <- function(sensitivity, specificity, n, prevalence) {
  Dx <- DxStatsExact(sensitivity, specificity, n, prevalence)
  table <- data.frame(
    labels1 = c("sensitivity =", "specificity =", "prevalence ="),
    values1 = c(sensitivity, specificity, prevalence),
    ConfInts = c(
      paste0(
        "(95% CI ", as.character(round(Dx$sens_ciL, digits = 2)),
        " - ", as.character(round(Dx$sens_ciU, digits = 2)), ")"
      ),
      paste0(
        "(95% CI ", as.character(round(Dx$spec_ciL, digits = 2)),
        " - ", as.character(round(Dx$spec_ciU, digits = 2)), ")"
      ),
      paste0(
        "(95% CI ", as.character(round(Dx$prev_ciL, digits = 2)),
        " - ", as.character(round(Dx$prev_ciU, digits = 2)), ")"
      )
    ),
    labels2 = c("+ve PV =", "-ve PV =", "study size ="),
    values2 = c(
      as.character(round(Dx$PPV, digits = 2)),
      as.character(round(Dx$NPV, digits = 2)),
      as.character(round(n))
    ),

    labels3 = c("LR+ =", "LR- =", ""),
    values3 = c(Dx$LRp, Dx$LRn, NA)
  )
  colnames(table) <- rep(c(""), 7)

  return(table)
}

tab2x2gtStats <- function(sensitivity, specificity, n, prevalence, tab2x2Labels) {

  # # for testing when not commented out
  #     tab2x2Labels <- tibble(
  #     title = "index test evaluated against reference test",
  #     subtitle = "for condition xxx",
  #     stubheadLabel = "reference test result",
  #     rowgroupLabel = "Index test",
  #     rownames = list(c("test +ve", "test -ve", "totals")),
  #     summaryLabel = "Totals",
  #     spannerColumnLabel = "Reference test",
  #     columnLabels = list(c("present", "absent", "totals")),
  #     footnote = "",
  #     sourcenotes = ""
  #   )

  return(
    tab2x2gt(
      TPfn(sensitivity, specificity, n, prevalence),
      FPfn(sensitivity, specificity, n, prevalence),
      FNfn(sensitivity, specificity, n, prevalence),
      TNfn(sensitivity, specificity, n, prevalence),
      tab2x2Labels
    )
  )
}

tab2x2gt <- function(TP, FP, FN, TN, tab2x2Labels) {

  # # set variables for testing
  # TP = 105
  # FP = 85
  # FN = 45
  # TN = 765


  tab2x2 <- tibble(
    rowname = tab2x2Labels$rownames[[1]],
    present = c(TP, FN, TP + FN),
    absent = c(FP, TN, FP + TN),
    totals = c(TP + FP, FN + TN, TP + FP + FN + TN)
  )

  prevalence <- as.character(round((TP + FN) / (TP + FP + FN + TN), digits = 3))

  sensSourceNote <- paste0(
    "sensitivity = ", as.character(round(TP / (TP + FN), digits = 3)),
    " (95% CI = ",
    as.character(round(ciprop(TP, (TP + FN))$ciL, digits = 3)),
    " to ",
    as.character(round(ciprop(TP, (TP + FN))$ciU, digits = 3)),
    ")"
  )

  specSourceNote <- paste0(
    "specificity = ", as.character(round(TN / (FP + TN), digits = 3)),
    " (95% CI = ",
    as.character(round(ciprop(TN, (FP + TN))$ciL, digits = 3)),
    " to ",
    as.character(round(ciprop(TN, (FP + TN))$ciU, digits = 3)),
    ")"
  )

  ppvSourceNote <- paste0(
    "PPV (with prevalence of ", prevalence, ") = ",
    as.character(round(TP / (TP + FP), digits = 3)),
    " (95% CI = ",
    as.character(round(ciprop(TP, (TP + FP))$ciL, digits = 3)),
    " to ",
    as.character(round(ciprop(TP, (TP + FP))$ciU, digits = 3)),
    ")"
  )

  npvSourceNote <- paste0(
    "NPV (with prevalence of ", prevalence, ") = ",
    as.character(round(TN / (TN + FN), digits = 3)),
    " (95% CI = ",
    as.character(round(ciprop(TN, (TN + FN))$ciL, digits = 3)),
    " to ",
    as.character(round(ciprop(TN, (TN + FN))$ciU, digits = 3)),
    ")"
  )

  g2x2t <- gt(tab2x2) %>%
    tab_header(
      title = tab2x2Labels$title,
      subtitle = tab2x2Labels$subtitle
    ) %>%
    fmt_number(
      columns = vars(present, absent, totals),
      suffixing = TRUE
    ) %>%
    # tab_stubhead(label = tab2x2Labels$stubheadLabel) %>%
    tab_row_group(
      group = tab2x2Labels$rowgroupLabel,
      rows = 1:2
    ) %>%
    tab_spanner(
      label = tab2x2Labels$spannerColumnLabel,
      columns = vars(present, absent)
    ) %>%
    tab_source_note("") %>%
    tab_source_note(sensSourceNote) %>%
    tab_source_note(specSourceNote) %>%
    tab_source_note(ppvSourceNote) %>%
    tab_source_note(npvSourceNote)

  return(g2x2t)
}





### coordinates and labels for contingency matrix graphic
contingencyM <- function(n, prevalence, sensitivity, specificity) {
  Dx <- DxStats(n, prevalence, sensitivity, specificity)


  return({
    data.frame(
      cmX = c(
        0.5 * Dx$Dpos / n, # TP
        (Dx$Dpos + 0.5 * Dx$Dneg) / n, # FP
        0.5 * Dx$Dpos / n, # FN
        (Dx$Dpos + 0.5 * Dx$Dneg) / n, # TN
        0.5 * Dx$Dpos / n, # ppv
        (Dx$Dpos + 0.5 * Dx$Dneg) / n # npv
      ),
      cmY = c(
        (Dx$FN + 0.5 * Dx$TP) / (Dx$FN + Dx$TP), # TP
        (Dx$TN + 0.5 * Dx$FP) / (Dx$TN + Dx$FP), # FP
        0.5 * Dx$FN / (Dx$FN + Dx$TP), # FN
        0.5 * Dx$TN / (Dx$TN + Dx$FP), # TN
        (Dx$FN + 0.5 * Dx$TP) / (Dx$FN + Dx$TP) - 0.04, # ppv
        0.5 * Dx$TN / (Dx$TN + Dx$FP) - 0.04 # npv
      ),
      labs = c(
        paste("TP = ", round(Dx$TP)),
        paste("FP = ", round(Dx$FP)),
        paste("FN = ", round(Dx$FN)),
        paste("TN = ", round(Dx$TN)),
        paste("ppv = ", paste(format(100 * Dx$TP / (Dx$TP + Dx$FP), digits = 2), "%", sep = "")),
        paste("npv = ", paste(format(100 * Dx$TN / (Dx$TN + Dx$FN), digits = 2), "%", sep = ""))
      )
    )
  })
}


pvdf <- function(n, prevalence, sensitivity, specificity) {
  Dx <- DxStats(n, prevalence, sensitivity, specificity)

  table2 <- data.frame(
    PredictiveValues = c(
      paste(format(100 * Dx$TP / (Dx$TP + Dx$FP), digits = 3), "%", sep = ""),
      paste(format(100 * Dx$TN / (Dx$TN + Dx$FN), digits = 3), "%", sep = "")
    ),
    ATPrevalence = c(paste(format(100 * prevalence, digits = 2), "%", sep = "")),
    Measure = c("Sensitivity", "Specificity"),
    LL95CI = c(
      paste(trimws(format((sensitivity * 100 - ciprop(sensitivity * 100, n)$ciL), digits = 2)), "%", sep = ""),
      paste(trimws(format((specificity * 100 - ciprop(specificity * 100, n)$ciL), digits = 2)), "%", sep = "")
    ),
    Mid = c(
      paste(trimws(format(100 * sensitivity, digits = 2)), "%", sep = ""),
      paste(trimws(format(100 * specificity, digits = 3)), "%", sep = "")
    ),
    UL95CI = c(
      paste(trimws(format((100 * sensitivity + ciprop(100 * sensitivity, n)$ciU), digits = 2)), "%", sep = ""),
      paste(trimws(format((100 * specificity + ciprop(100 * specificity, n)$ciU), digits = 2)), "%", sep = "")
    ),
    row.names = c("PPV", "NPV")
  )

  colnames(table2) <- c("Predictive values", "Prevalence", "Accuracy measure", "Lower 95% CI", "Mid point", "Upper 95% CI")
  return(table2)
}


populationdf <- function(n, prevalence, sensitivity, specificity, sorted) {
  Dx <- DxStats(n, prevalence, sensitivity, specificity)


  if (sorted) {
    x <- c(
      runif(round(round(Dx$Dpos) * sensitivity), min = marginInsidePlot, max = round(Dx$Dpos) / n - marginInsidePlot),
      runif(round(Dx$Dpos) - round(round(Dx$Dpos) * sensitivity), min = marginInsidePlot, max = round(Dx$Dpos) / n - marginInsidePlot),
      runif(round(Dx$Dneg) - round(round(Dx$Dneg) * specificity), min = round(Dx$Dpos) / n + marginInsidePlot, max = 1 - marginInsidePlot),
      runif(round(round(Dx$Dneg) * specificity), min = round(Dx$Dpos) / n + marginInsidePlot, max = 1 - marginInsidePlot)
    )
    y <- c(
      runif(round(round(Dx$Dpos) * sensitivity), min = round(Dx$FN, 0) / (round(Dx$TP, 0) + round(Dx$FN, 0)) + marginInsidePlot, max = 1 - marginInsidePlot),
      runif(round(Dx$Dpos) - round(round(Dx$Dpos) * sensitivity), min = marginInsidePlot, max = Dx$FN / (Dx$TP + Dx$FN) - marginInsidePlot),
      runif(round(Dx$Dneg) - round(round(Dx$Dneg) * specificity), min = round(Dx$TN, 0) / (round(Dx$FP, 0) + round(Dx$TN, 0)) + marginInsidePlot, max = 1 - marginInsidePlot), # need to fix this if FPs less than 1!!
      runif(round(round(Dx$Dneg) * specificity), min = marginInsidePlot, max = Dx$TN / (Dx$FP + Dx$TN) - marginInsidePlot)
    )
  } else {
    x <- c(runif(n, min = marginInsidePlot, max = 1 - marginInsidePlot))
    y <- c(runif(n, min = marginInsidePlot, max = 1 - marginInsidePlot))
  }


  return({
    data.frame(
      ID = 1:n,
      condition = c(
        rep(paste("Present  = ", Dx$Dpos), times = round(Dx$Dpos)),
        rep(paste("Absent = ", n - Dx$Dpos), times = n - round(Dx$Dpos))
      ),
      conditionShape = c(
        rep(21, times = round(Dx$Dpos)),
        rep(22, times = n - round(Dx$Dpos))
      ),

      testResult = c(
        rep(paste("TesTPos = ", Dx$FP + Dx$TP),
          times = round(round(Dx$Dpos) * sensitivity) + round(Dx$Dneg) - round(round(Dx$Dneg) * specificity)
        ),
        rep(paste("TesTNeg = ", round(round(Dx$Dneg) * specificity) + round(Dx$Dpos) - round(round(Dx$Dpos) * sensitivity)),
          times = (round(round(Dx$Dneg) * specificity) + round(Dx$Dpos) - round(round(Dx$Dpos) * sensitivity))
        )
      ),

      result = c(
        rep(paste("TruePos = ", round(round(Dx$Dpos) * sensitivity)), times = round(round(Dx$Dpos) * sensitivity)),
        rep(paste("FalseNeg = ", round(Dx$Dpos) - round(round(Dx$Dpos) * sensitivity)), times = round(Dx$Dpos) - round(round(Dx$Dpos) * sensitivity)),
        rep(paste("FalsePos = ", round(Dx$Dneg) - round(round(Dx$Dneg) * specificity)), times = round(Dx$Dneg) - round(round(Dx$Dneg) * specificity)),
        rep(paste("TrueNeg = ", round(round(Dx$Dneg) * specificity)), times = round(round(Dx$Dneg) * specificity))
      ),
      resultShape = c(
        rep(21, times = round(round(Dx$Dpos) * sensitivity)), ## need to sort out these!
        rep(22, times = round(Dx$Dpos) - round(round(Dx$Dpos) * sensitivity)),
        rep(23, times = round(Dx$Dneg) - round(round(Dx$Dneg) * specificity)),
        rep(24, times = round(round(Dx$Dneg) * specificity))
      ),
      x,
      y
    )
  })
}





naturalFrequencies <- function() {

  # # for testing
  # TP <- 100
  # FP <- 10
  # FN <- 10
  # TN <- 1000
  # sensitivity <- TP/(TP + FP)
  # specificity <- TN/(FP + TN)
  # prevalence <- (TP + FP)/(TP + FP + FP + TN)
  # TPglobal <<- assign("TPglobal", round(TP), envir = .GlobalEnv)
  # FPglobal <<- assign("FPglobal", round(FP), envir = .GlobalEnv)
  # FNglobal <<- assign("FNglobal", round(FN), envir = .GlobalEnv)
  # TNglobal <<- assign("TNglobal", round(TN), envir = .GlobalEnv)
  # assign("SensPCTglobal", round(sensitivity*100, digits = 1), envir = .GlobalEnv)
  # assign("SensPCTglobal", round(100 - sensitivity*100, digits = 1), envir = .GlobalEnv)
  # assign("SpecPCTglobal", round(specificity*100, digits = 1), envir = .GlobalEnv)
  # assign("SpecPCTglobal", round(100 -specificity*100, digits = 1), envir = .GlobalEnv)
  # assign("PrevPCTglobal", round(prevalence*100, digits = 1), envir = .GlobalEnv)
  # assign("PrevPCTglobal", round(100 - prevalence*100, digits = 1), envir = .GlobalEnv)


  diagram <- DiagrammeR::grViz("
strict digraph graph2 {

graph [layout = dot]

# node definitions with substituted label text
node [shape = rectangle, width = 4, fillcolor = Biege]
test [label = '@@1']

testpos [label = '@@2']
testneg [label = '@@3']

tp [label = '@@4']
fp [label = '@@5']
fn [label = '@@6']
tn [label = '@@7']

tpnb [label = '@@8']
fpnb [label = '@@9']
fnnb [label = '@@10']
tnnb [label = '@@11']

testposnb [label = '@@12']
testnegnb [label = '@@13']

testnb [label = '@@14']

test -> testpos  test -> testneg

testpos -> tp testpos -> fp 
testneg -> fn testneg -> tn 

tp -> tpnb
fp -> fpnb
fn -> fnnb
tn -> tnnb

tpnb -> testposnb
fpnb -> testposnb
fnnb -> testnegnb
tnnb -> testnegnb

testposnb -> testnb
testnegnb -> testnb
}

[1]: paste0('Population = ', (TPglobal + FPglobal + FNglobal + TNglobal))
[2]: paste0('Test result +ve = ', (TPglobal + FPglobal))
[3]: paste0('Test result -ve = ', (FNglobal + TNglobal))
[4]: paste0('True Positive = ', TPglobal)
[5]: paste0('False Positive = ', FPglobal)
[6]: paste0('False Negative = ', FNglobal)
[7]: paste0('True Negative = ', TNglobal)
[8]: paste0('Net benefit of TP = balance of consequent:\\nTP benefits, TP harms, TP costs')
[9]: paste0('Net benefit of FP = balance of consequent:\\nFP benefits, FP harms, FP costs')
[10]: paste0('Net benefit of FN = balance of consequent:\\nFN benefits, FN harms, FN costs')
[11]: paste0('Net benefit of TN = balance of consequent:\\nTN benefits, TN harms, TN costs')
[12]: paste0('Net benefit of +ve test:\\n', SensPCTglobal, '% TP net benefits + ', SensPCT1Mglobal, '% FN net benefits')
[13]: paste0('Net benefit of -ve test:\\n', SpecPCT1Mglobal, '% FP net benefits + ', SpecPCTglobal, '% TN net benefits')
[14]: paste0('Net benefit of test:\\n', PrevPCTglobal, '% of net benefits of +ve result  +  ', PrevPCT1Mglobal, '% of net benefits of -ve result')

")
  return(diagram)
}


predValuesPlot1 <- function(sensitivity, specificity, n, prevalence, ciFlag) {

  # # # # for testing function in console before trying in shiny
  # n <- 719
  # prevalence <- 0.4144645341
  # sensitivity <- 0.7583892617
  # specificity <- 0.8527315914
  #  ciFlag <- TRUE
  # # # #
  # # # #
  # #
  NPV <- DxStatsExact(sensitivity, specificity, n, prevalence)$NPV
  PPV <- DxStatsExact(sensitivity, specificity, n, prevalence)$PPV

  prevalenceLine <- tibble(
    x = c(prevalence, prevalence, prevalence, prevalence),
    y = c(
      0,
      NPV,
      PPV,
      1
    ),
    labels = c(
      "",
      paste0(
        "NPV = ", round(100 * NPV),
        "%\n for prev = ", round(prevalence * 100, digits = 1), "%"
      ),
      paste0(
        "PPV = ", round(100 * PPV),
        "%\n for prev = ", round(prevalence * 100, digits = 1), "%"
      ),
      ""
    )
  )

  pvPoints <- tibble(
    x = c(prevalence, prevalence),
    y = c(
      NPV,
      PPV
    ),
    grp = c("PPV", "NPV")
  )

  Ribbons <- tibble(
    prevalence = seq(0, 1, by = 0.01),
    PPV_ciL = prevalence,
    PPV = prevalence,
    PPV_ciU = prevalence,

    NPV_ciL = prevalence,
    NPV = prevalence,
    NPV_ciU = prevalence
  )

  Ribbons <- Ribbons$prevalence %>%
    map(prev1st, sensitivity, specificity, n) %>%
    bind_rows()

  predValues <- Ribbons %>%
    select(prevalence, PPV, NPV) %>%
    pivot_longer(-prevalence, names_to = "Pred", values_to = "Predictive_Value") %>%
    group_by(Pred)


  p0 <- ggplot()

  if (ciFlag) {
    p0 <- p0 +
      geom_ribbon(aes(x = prevalence, ymin = PPV_ciL, ymax = PPV_ciU), data = Ribbons, fill = "mintcream", alpha = 0.9) +
      geom_ribbon(aes(x = prevalence, ymin = NPV_ciL, ymax = NPV_ciU), data = Ribbons, fill = "seashell", alpha = 0.9)
  }

  p0 <- p0 +
    geom_point(data = predValues, aes(prevalence, Predictive_Value, colour = Pred), size = 1, stroke = 0) +
    scale_color_manual(values = c("cyan3", "firebrick")) +
    # scale_color_manual(values=c("cyan3", "firebrick", "firebrick", "cyan3")) +
    geom_line(data = prevalenceLine, aes(x, y)) +
    geom_text(data = prevalenceLine, aes(x, y, label = labels)) +
    geom_point(data = pvPoints, aes(x, y, colour = grp, fill = grp), size = 2, shape = 23, stroke = 1) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0)) +
    geom_segment(aes(x = 0, y = NPV, xend = prevalence, yend = NPV)) +
    geom_segment(aes(x = 0, y = PPV, xend = prevalence, yend = PPV)) +
    scale_x_continuous(limits = c(-0.025, 1.025), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_y_continuous(limits = c(-0.025, 1.025), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    # theme(legend.title=element_blank()) +
    theme(legend.position = "none") +
    labs(y = "Predictive value", x = "Prevalence/Clinical probability") +
    coord_fixed()

  p0 <- ggplotly(p0, tooltip = c("prevalence", "Predictive_Value"))
}



predValuesPlot2 <- function(sensitivity, specificity, n, prevalence) {
  # plot predictive values against prevalence for a range of sensitivities and specificities

  # # # # for testing function in console before trying in shiny
  # n <- 719
  # prevalence <- 0.4144645341
  # sensitivity <- 0.6583892617
  # specificity <- 0.9527315914
  # # # #
  # # # #
  # #

  NPV <- DxStatsExact(sensitivity, specificity, n, prevalence)$NPV
  PPV <- DxStatsExact(sensitivity, specificity, n, prevalence)$PPV

  prevalenceLine <- tibble(
    x = c(prevalence, prevalence, prevalence, prevalence),
    y = c(
      0,
      NPV,
      PPV,
      1
    ),
    labels = c(
      "",
      paste0(
        "NPV = ", round(100 * NPV),
        "%\n for prev = ", round(prevalence * 100, digits = 1), "%"
      ),
      paste0(
        "PPV = ", round(100 * PPV),
        "%\n for prev = ", round(prevalence * 100, digits = 1), "%"
      ),
      ""
    )
  )

  pvPoints <- tibble(
    x = c(prevalence, prevalence),
    y = c(
      NPV,
      PPV
    ),
    grp = c("PPV", "NPV")
  )

  pvLines <- tibble(
    prevalence = seq(0, 1, by = 0.01),
    PPV = prevalence,
    NPV = prevalence,
  )


  pvLineSe1 <- pvLines$prevalence %>%
    map(prev1st, sensitivity - (-0.5 + sensitivity) / 2, specificity, n) %>%
    bind_rows() %>%
    select(prevalence, PPV, NPV) %>%
    mutate(statValue = paste0("sensitivity = ", as.character(round((sensitivity - (-0.5 + sensitivity) / 2) * 100, digits = 1)), "%")) %>%
    mutate(statName = "sensitivity")

  pvLineSe2 <- pvLines$prevalence %>%
    map(prev1st, sensitivity, specificity, n) %>%
    bind_rows() %>%
    select(prevalence, PPV, NPV) %>%
    mutate(statValue = paste0("sensitivity fixed at ", as.character(round(sensitivity * 100, digits = 1)), "%")) %>%
    mutate(statName = "sensitivity")

  pvLineSe3 <- pvLines$prevalence %>%
    map(prev1st, sensitivity + (1 - sensitivity) / 2, specificity, n) %>%
    bind_rows() %>%
    select(prevalence, PPV, NPV) %>%
    mutate(statValue = paste0("sensitivity3 = ", as.character(round((sensitivity + (1 - sensitivity) / 2) * 100, digits = 1)), "%")) %>%
    mutate(statName = "sensitivity")

  pvLineSp1 <- pvLines$prevalence %>%
    map(prev1st, sensitivity, specificity - (-0.5 + specificity) / 2, n) %>%
    bind_rows() %>%
    select(prevalence, PPV, NPV) %>%
    mutate(statValue = paste0("specificity = ", as.character(round((specificity - (-0.5 + specificity) / 2) * 100, digits = 1)), "%")) %>%
    mutate(statName = "specificity")

  pvLineSp2 <- pvLines$prevalence %>%
    map(prev1st, sensitivity, specificity, n) %>%
    bind_rows() %>%
    select(prevalence, PPV, NPV) %>%
    mutate(statValue = paste0("specificity fixed at ", as.character(round(specificity * 100, digits = 1)), "%")) %>%
    mutate(statName = "specificity")

  pvLineSp3 <- pvLines$prevalence %>%
    map(prev1st, sensitivity, specificity + (1 - specificity) / 2, n) %>%
    bind_rows() %>%
    select(prevalence, PPV, NPV) %>%
    mutate(statValue = paste0("specificity3 = ", as.character(round((specificity + (1 - specificity) / 2) * 100, digits = 1)), "%")) %>%
    mutate(statName = "specificity")

  statFactorLevels <- c(
    pvLineSe1$statValue[1],
    pvLineSe2$statValue[1],
    pvLineSe3$statValue[1],
    pvLineSp1$statValue[1],
    pvLineSp2$statValue[1],
    pvLineSp3$statValue[1]
  )

  pvSensSpecLines <- bind_rows(pvLineSe1, pvLineSe2, pvLineSe3, pvLineSp1, pvLineSp2, pvLineSp3) %>%
    pivot_longer(-prevalence,
      cols = c(PPV, NPV),
      names_to = "Pred", values_to = "Predictive_Value"
    ) %>%
    mutate(Pred = factor(Pred, levels = c("PPV", "NPV"))) %>%
    mutate(statValue = factor(statValue, levels = statFactorLevels))




  p0 <- ggplot(pvSensSpecLines, aes(prevalence, Predictive_Value, colour = statValue)) +
    geom_point(size = 0.5) +
    scale_color_manual(values = c("cornflowerblue", "firebrick", "springgreen4", "cornflowerblue", "firebrick", "springgreen4")) +
    facet_grid(statName ~ Pred) +
    scale_x_continuous(limits = c(-0.025, 1.025), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_y_continuous(limits = c(-0.025, 1.025), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    theme(legend.position = "none") +
    labs(
      title = paste0(
        "Variation of predictive values with\n    1) sensitivity (with ", pvLineSp2$statValue[1],
        ")\n    2) specificity (with ", pvLineSe2$statValue[1], ")"
      ),
      y = "Predictive value",
      x = "Prevalence/Clinical probability"
    ) +
    coord_fixed()

  return(p0)
}





popplot <- function(n, prevalence, sensitivity, specificity, sorted, ciFlag) {
  populationdf <- populationdf(n, prevalence, sensitivity, specificity, sorted)
  linesDf <- linesDf(n, prevalence, sensitivity, specificity)

  p1 <- ggplot(populationdf, aes(x = x, y = y, color = condition, shape = condition)) +
    geom_point(size = 4) +
    # scale_color_manual(values=c("#E69F00", "#999999")) + coord_fixed() +
    scale_color_manual(values = c("cyan3", "firebrick")) +
    coord_fixed() +
    labs("Condition") # , colour="Result")
  if (sorted) {
    p1 <- ggplot(populationdf, aes(x = x, y = y, color = condition, shape = condition)) +
      geom_point(size = 4) + # scale_color_manual(values=c("#999999", "#E69F00"))  +

      ### add line segments (with 95% CI)  to separate Condition present from condition absent
      geom_segment(aes(x = vx, y = 0, xend = vx, yend = 1, colour = NULL, shape = NULL),
        data = linesDf
      ) +


      ### add in scales for x and y axis
      scale_x_continuous(
        breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
        labels = c("0", "25%", "50%", "75%", "100%")
      ) +
      theme(axis.text.x = element_text(size = 15, colour = "azure4")) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      # scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
      #                    labels = c("0","25%","50%","75%","100%")) + theme(axis.text.y = element_text(size = 15,colour = "azure4")) +
      coord_fixed()

    if (ciFlag) {
      p1 <- p1 + annotate("rect",
        xmin = linesDf$vxlci, xmax = linesDf$vxuci, ymin = 0, ymax = 1,
        colour = "deepskyblue", alpha = 0.2
      )
    }
  }
  if (!sorted) {
    p1 <- p1 +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      )
  }
  p1 <- p1 +
    labs(x = "", y = "") # , title="Population: people with and without the condition") +
  #  theme(plot.title = element_text(size = rel(1.5), colour = "dodgerblue3"))
  p1
}


popplot2 <- function(n, prevalence, sensitivity, specificity, sorted, ciFlag) {
  populationdf <- populationdf(n, prevalence, sensitivity, specificity, sorted)
  linesDf <- linesDf(n, prevalence, sensitivity, specificity)
  contingencyM <- contingencyM(n, prevalence, sensitivity, specificity)

  p2 <- ggplot(populationdf, aes(x = x, y = y, color = condition, shape = result)) +
    geom_point(size = 4) +
    coord_fixed() +
    labs(color = "Condition", shape = "Result")

  if (sorted) {
    p2 <- p2 +
      ### line to separate Condition present/absent
      geom_segment(aes(x = vx, y = 0, xend = vx, yend = 1, colour = NULL, shape = NULL),
        data = linesDf
      ) +

      ### line to separate TP from FN
      geom_segment(aes(x = 0, y = hy1, xend = vx, yend = hy1, colour = NULL, shape = NULL),
        data = linesDf
      ) +

      ### line to separate FP from TN
      geom_segment(aes(x = vx, y = hy2, xend = 1, yend = hy2, colour = NULL, shape = NULL),
        data = linesDf
      ) +

      ### label the cells of the contingency matrix
      geom_label_repel(
        data = contingencyM, size = 5, aes(x = cmX, y = cmY, label = labs, colour = NULL, shape = NULL),
        fontface = 2, colour = "gray41", label.padding = unit(0.15, "lines"), label.r = unit(0.2, "lines")
      ) +

      ### add in scales for x and y axis
      scale_x_continuous(
        breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
        labels = c("0", "25%", "50%", "75%", "100%")
      ) + theme(axis.text.x = element_text(size = 15, colour = "azure4")) +
      scale_y_continuous(
        breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
        labels = c("0", "25%", "50%", "75%", "100%")
      ) + theme(axis.text.y = element_text(size = 15, colour = "azure4")) +
      coord_fixed()


    if (ciFlag) {
      p2 <- p2 +
        ### rectangles to show 95% CIs
        annotate("rect",
          xmin = linesDf$vxlci, xmax = linesDf$vxuci, ymin = 0, ymax = 1,
          colour = "deepskyblue", alpha = 0.2
        ) +
        annotate("rect",
          xmin = linesDf$vxlci, xmax = linesDf$vxuci, ymin = 0, ymax = 1,
          colour = "deepskyblue", alpha = 0.2
        ) +
        annotate("rect",
          xmin = 0, xmax = linesDf$vx, ymin = linesDf$hy1lci, ymax = linesDf$hy1uci,
          colour = "darksalmon", alpha = 0.2
        ) +
        annotate("rect",
          xmin = linesDf$vx, xmax = 1, ymin = linesDf$hy2lci, ymax = linesDf$hy2uci,
          colour = "darksalmon", alpha = 0.2
        ) #+
      #  geom_text(data = contingencyM, size = 7, aes(x = cmX, y = cmY, label = labs, colour = NULL, shape = NULL),
      #            fontface = 2, colour = "gray41")
    }
  }
  if (!sorted) {
    p2 <- p2 + theme(
      axis.text.x = element_blank(), # element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  }
  p2 <- p2 +
    labs(x = "", y = "") # ,
  #    title ="Test accuracy: true and false positives; false \n and true negatives.") +
  #   theme(plot.title = element_text(size = rel(1.5), colour = "dodgerblue3"))
  # ggtitle("Test accuracy: true and false positives; \n false and true negatives, sensitivity, specificity, ...")
  p2
}

distributiondf <- function(prevalence, sensitivity, specificity) {
  xdist <- seq(0, 1, length = 1000)
  mean_pos <- 0.5 - sensitivity * 0.25
  ydist <- prevalence * dnorm(xdist, mean = mean_pos, sd = 0.1)
  xdist2 <- seq(0, 1, length = 1000)
  mean_neg <- 0.5 + specificity * 0.25
  ydist2 <- (1 - prevalence) * dnorm(xdist2, mean = mean_neg, sd = 0.1)

  ydist[1] <- 0
  ydist2[1] <- 0
  ydist[1000] <- 0
  ydist2[1000] <- 0

  return({
    data.frame(
      mean_pos = mean_pos,
      mean_neg = mean_neg,
      xdist = xdist,
      ydist = ydist,
      xdist2 = xdist2,
      ydist2 = ydist2
    )
  })
}

### coordinates and labels for distributions graphic
distritext <- function(n, prevalence, sensitivity, specificity) {
  distdf <- distributiondf(prevalence, sensitivity, specificity)
  Dx <- DxStats(n, prevalence, sensitivity, specificity)


  max1 <- 0.25 * max(distdf$ydist, na.rm = TRUE)
  max2 <- 0.25 * max(distdf$ydist2, na.rm = TRUE)

  data.frame(
    cmX = c(
      distdf$mean_pos[1],
      distdf$mean_neg[1] - 0.25,
      distdf$mean_pos[1] + 0.25,
      distdf$mean_neg[1]
    ),
    cmY = c(
      max1,
      max2 * 0.25,
      max1 * 0.25,
      max2
    ),
    labels = c(
      strwrap(paste0("TP = ", Dx$TP)),
      strwrap(paste0("FP = ", Dx$FP)),
      strwrap(paste0("FN = ", Dx$FN)),
      strwrap(paste0("TN = ", Dx$TN))
    )
  )
}


distributionplots <- function(n, prevalence, sensitivity, specificity) {
  distridf <- distributiondf(prevalence, sensitivity, specificity)

  distritext <- distritext(n, prevalence, sensitivity, specificity)
  Dx <- DxStats(n, prevalence, sensitivity, specificity)

  shade <- rbind(c(0.5, 0), subset(distridf, xdist < 0.5), c(distridf[nrow(distridf), "X"], 0))
  shade3 <- rbind(c(0.5, 0), subset(distridf, xdist < 0.5), c(distridf[nrow(distridf), "X"], 0.5))
  shade2 <- rbind(c(0.5, 0), subset(distridf, xdist2 > 0.5), c(distridf[nrow(distridf), "X2"], 0))
  distri <- ggplot(distridf, aes(x = xdist2, y = ydist2)) +
    geom_polygon(data = shade2, aes(xdist2, ydist2), fill = "#999999")
  distri <- distri + geom_line(colour = "#999999")
  distri <- distri + geom_line(aes(x = xdist, y = ydist), colour = "#E69F00") + geom_vline(xintercept = 0.5) +
    geom_polygon(data = shade3, aes(xdist, ydist), fill = "#E69F00") +
    geom_line(data = distridf, aes(x = xdist2, y = ydist2), colour = "#999999") +
    theme(
      axis.title.x = element_text(size = 8),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    scale_x_continuous("Index test result") +
    annotate("text", x = 0.05, y = 3.1, label = "With condition", size = 6, colour = "royalblue3") +
    annotate("text", x = 0.95, y = 3.1, label = "Without condition", size = 6, colour = "royalblue3") +
    geom_text(data = distritext, size = 6, aes(x = cmX, y = cmY, label = labels))
  distri
}
