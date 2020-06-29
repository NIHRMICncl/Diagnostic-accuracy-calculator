source("global.R")


shinyServer(
  function(input, output, session) {
    session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it

    ##### dynamic UI-s

    output$dataEntryTitle <-
      renderUI({
        req(input$studyDesign, input$dataType)
        if (input$studyDesign == "cohort") { # ****Note: ifelse() does not work****
          h4("Enter results from cohort diagnostic accuracy study:")
        } else {
          h4("Enter results from case-control diagnostic accuracy study:")
        }
      })

    output$dataEntry2x2 <- # test accuracy data
      renderUI({ # data input for TP, FP, FN, TN
        req(input$studyDesign, input$dataType)
        if (
          input$dataType == "TP, FP, FN, TN") {
          tagList(
            numericInput("TP", "True positives", min = 1, value = 1),
            numericInput("FP", "False positives", min = 0, value = 0),
            numericInput("FN", "False negatives", min = 0, value = 0),
            numericInput("TN", "True negatives", min = 1, value = 1)
          )
        }
      })

    output$dataEntrySS <- # test accuracy data
      renderUI({ # data input for sensitivity, specificity data
        req(input$studyDesign, input$dataType)
        if (
          input$dataType == "sensitivity & specificity") {
          tagList(
            numericInput("sensitivity", "sensitivity of index test (50% to 100%)", min = 50, max = 100, value = 50),
            numericInput("specificity", "specificity of index test (50% to 100%)", min = 50, max = 100, value = 50)
          )
        }
      })


    output$dataEntryPrevN <- # prevalence and study size for case-control studies
      renderUI({
        req(input$studyDesign, input$dataType)
        if (
          input$dataType == "sensitivity & specificity" | (input$studyDesign == "Case-control/don't know")) {
          tagList(
            hr(),
            h5("Enter parameters to enable calculation of predictive values"),
            numericInput("n", "number in study providing data", min = 5, value = 5),
            numericInput("prevalence", "prevalence/clinical probability of condition (0% to 100%)", min = 0, max = 100, value = 15)
          )
        }
      })

    ##### reactive expressions


    sensitivity <- reactive({
      req(input$dataType)
      if (input$dataType == "sensitivity & specificity") {
        req(input$sensitivity)
        # validate(
        #   need(input$sensitivity >= 50 & input$sensitivity <= 100), "Sensitivity must be between 50% and 100%")
        input$sensitivity / 100
      }
      else {
        req(input$TP, input$FN)
        # validate(
        #   need((input$TP > 0 & input$FN >= 0 ), "TP must be > 0 and FN >= 0"))
        input$TP / (input$TP + input$FN)
      }
    })

    specificity <- reactive({
      req(input$dataType)
      if (input$dataType == "sensitivity & specificity") {
        req(input$specificity)
        # validate(
        #   need(input$specificity >= 50 & input$specificity <= 100), "specificity must be between 50% and 100%")
        input$specificity / 100
      }
      else {
        req(input$TN, input$FP)
        # validate(
        #   need((input$TN + input$FP > 0), "TN + FP must be > 0"))
        input$TN / (input$TN + input$FP)
      }
    })

    n <- reactive({
      req(input$dataType, input$studyDesign)
      if (input$dataType == "sensitivity & specificity" | (input$studyDesign == "Case-control/don't know")) {
        req(input$n)
        # validate(
        #   need(input$n > 5), "n must be at least 5")
        input$n
      }
      else {
        req(input$TP, input$FP, input$FN, input$TN)
        # validate(
        #   need((input$TP > 0 & input$FP >= 0 & input$FN >= 0 & input$TN > 0), "Invalid count for at least 1 of TP, FP, FN, TN"))
        input$TP + input$FP + input$FN + input$TN
      }
    })

    prevalence <- reactive({
      req(input$dataType, input$studyDesign)
      if (input$dataType == "sensitivity & specificity" | (input$studyDesign == "Case-control/don't know")) {
        req(input$prevalence)
        # validate(
        #   need(input$prevalence > 0 & input$prevalence <= 100, "Prevalence must be greater than 0% and less than 100%"))
        input$prevalence / 100
      }
      else {
        req(input$TP, input$FP, input$FN, input$TN)
        validate(
          need((input$TP + input$FP + input$FN + input$TN > 0), "n must be > 0")
        )
        (input$TP + input$FN) / (input$TP + input$FP + input$FN + input$TN)
      }
    })

    TP <- reactive({
      req(input$dataType)
      if (input$dataType == "sensitivity & specificity") {
        req(input$sensitivity, input$specificity, input$n, input$prevalence)
        TPfn(input$sensitivity / 100, input$specificity / 100, input$n, input$prevalence / 100)
      }
      else {
        if (input$studyDesign == "cohort") {
          req(input$TP)
          # not working???
          # validate(
          #   need((input$TP > 0), "TP must be > 0"))
          input$TP
        }
        else { # for a case control study, reverse engineer the TP, FP, FN, TN for given n and prevalence
          req(input$TP, input$FP, input$FN, input$TN, input$n, input$prevalence)
          TPfn(
            input$TP / (input$TP + input$FN), # sensitivity
            input$TN / (input$FP + input$TN), # specificity
            input$n, input$prevalence / 100
          )
        }
      }
    })

    FP <- reactive({
      req(input$dataType)
      if (input$dataType == "sensitivity & specificity") {
        req(input$sensitivity, input$specificity, input$n, input$prevalence)
        FPfn(input$sensitivity / 100, input$specificity / 100, input$n, input$prevalence / 100)
      }
      else {
        if (input$studyDesign == "cohort") {
          req(input$FP)
          input$FP
        }
        else { # for a case control study, reverse engineer the TP, FP, FN, TN for given n and prevalence
          req(input$TP, input$FP, input$FN, input$TN, input$n, input$prevalence / 100)
          FPfn(
            input$TP / (input$TP + input$FN), # sensitivity
            input$TN / (input$FP + input$TN), # specificity
            input$n, input$prevalence / 100
          )
        }
      }
    })

    FN <- reactive({
      req(input$dataType)
      if (input$dataType == "sensitivity & specificity") {
        req(input$sensitivity, input$specificity, input$n, input$prevalence)
        FNfn(input$sensitivity / 100, input$specificity / 100, input$n, input$prevalence / 100)
      }
      else {
        if (input$studyDesign == "cohort") {
          req(input$FN)
          input$FN
        }
        else { # for a case control study, reverse engineer the TP, FP, FN, TN for given n and prevalence
          req(input$TP, input$FP, input$FN, input$TN, input$n, input$prevalence)
          FNfn(
            input$TP / (input$TP + input$FN), # sensitivity
            input$TN / (input$FP + input$TN), # specificity
            input$n, input$prevalence / 100
          )
        }
      }
    })


    TN <- reactive({
      req(input$dataType)
      if (input$dataType == "sensitivity & specificity") {
        req(input$sensitivity, input$specificity, input$n, input$prevalence)
        TNfn(input$sensitivity / 100, input$specificity / 100, input$n, input$prevalence / 100)
      }
      else {
        if (input$studyDesign == "cohort") {
          req(input$TN)
          input$TN
        }
        else { # for a case control study, reverse engineer the TP, FP, FN, TN for given n and prevalence
          req(input$TP, input$FP, input$FN, input$TN, input$n, input$prevalence)
          TNfn(
            input$TP / (input$TP + input$FN), # sensitivity
            input$TN / (input$FP + input$TN), # specificity
            input$n, input$prevalence / 100
          )
        }
      }
    })


    tab2x2Labels <- reactive({
      tibble(
        title = input$customTitle,
        subtitle = input$customSubTitle,
        stubheadLabel = input$indexTest,
        rowgroupLabel = input$indexTest,
        rownames = list(c("test +ve", "test -ve", "totals")),
        summaryLabel = "Totals",
        spannerColumnLabel = input$referenceTest,
        columnLabels = list(c("present", "absent", "totals")),
        footnote = "",
        sourcenotes = ""
      )
    })

    ##### outputs

    observeEvent(input$GoButton,
      {
        req(input$dataType, input$studyDesign)
        if (input$studyDesign == "Case-control/don't know" & input$dataType == "TP, FP, FN, TN") {
          output$table1subtitle <- renderUI(
            tagList(
              p(),
              p("TP, FP, FN, TN in the table and flow chart are calculated from prevalence and n."),
              p('They therefore differ from the entered data -- see the "Guide to using the calculator for more information"')
            )
          )
        }
        else {
          output$table1subtitle <- NULL
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$GoButton,
      {
        output$table1title <- renderText({
          "Summary of data and statistical measures"
        })
      },
      ignoreInit = TRUE
    )


    observeEvent(input$GoButton,
      {
        output$tableStats <- renderText({
          "Diagnostic accuracy statistics"
        })
      },
      ignoreInit = TRUE
    )


    observeEvent(input$GoButton,
      {
        output$dx2x2Table <- renderTable(
          {
            isolate(dx2x2Table(sensitivity(), specificity(), n(), prevalence()))
          },
          digits = 0
        )
      },
      ignoreInit = TRUE
    )


    # observeEvent(input$GoButton, {
    #         output$dxStatsTable <- renderTable({
    #             isolate(dxStatsTable(sensitivity(), specificity(), n(), prevalence()))
    #         }, digits = 2, align = "rllrlrl", na = "")
    # }, ignoreInit = TRUE)
    #

    observeEvent(input$GoButton,
      {
        output$dx2x2tgt <-
          render_gt({
            tab2x2gtStats(sensitivity(), specificity(), n(), prevalence(), tab2x2Labels())
          })
      },
      ignoreInit = TRUE
    )

    observeEvent(input$GoButton,
      {
        # DiagrammeR likes its variables to be in the global environment
        #  assign() used because <<- puts variables in an unpredictable environment
        req(TP(), FP(), FN(), TN(), sensitivity(), specificity(), prevalence())
        assign("TPglobal", round(TP(), digits = 1), envir = .GlobalEnv)
        assign("FPglobal", round(FP(), digits = 1), envir = .GlobalEnv)
        assign("FNglobal", round(FN(), digits = 1), envir = .GlobalEnv)
        assign("TNglobal", round(TN(), digits = 1), envir = .GlobalEnv)
        assign("TestPosGlobal", round((TP() + FP()), digits = 1), envir = .GlobalEnv)
        assign("TestNegGlobal", round((FN() + TN()), digits = 1), envir = .GlobalEnv)
        assign("SensPCTglobal", round(sensitivity() * 100, digits = 1), envir = .GlobalEnv)
        assign("SensPCT1Mglobal", round(100 - sensitivity() * 100, digits = 1), envir = .GlobalEnv)
        assign("SpecPCTglobal", round(specificity() * 100, digits = 1), envir = .GlobalEnv)
        assign("SpecPCT1Mglobal", round(100 - specificity() * 100, digits = 1), envir = .GlobalEnv)
        assign("PrevPCTglobal", round(prevalence() * 100, digits = 1), envir = .GlobalEnv)
        assign("PrevPCT1Mglobal", round(100 - prevalence() * 100, digits = 1), envir = .GlobalEnv)
      },
      ignoreInit = TRUE,
      priority = 10,
      ignoreNULL = TRUE
    )


    observeEvent(input$GoButton,
      {
        req(TP(), FP(), FN(), TN())
        output$naturalFrequencies <- renderGrViz(
          naturalFrequencies(),
          env = .GlobalEnv
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )


    observeEvent(input$GoButton,
      {
        output$customPlotTitle <- renderText({
          input$customPlotTitle
        })

        output$predValuesPlotTitle <- renderText({
          "Positive and negative predictive values versus prevalence/clinical probability"
        })

        output$predValuesPlot1 <- renderPlotly({
          isolate(
            predValuesPlot1(sensitivity(), specificity(), n(), prevalence(), input$ciFlag)
          )
        })
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )


    observeEvent(input$GoButton,
      {
        output$predValuesPlot2 <- renderPlot({
          isolate(
            predValuesPlot2(sensitivity(), specificity(), n(), prevalence())
          )
        })
      },
      ignoreInit = TRUE
    )

    observeEvent(input$GoButton,
      {
        output$populationPlot <- renderPlot({
          isolate(popplot(
            n(), prevalence(), sensitivity(), specificity(),
            input$sorted, input$ciFlag
          ))
        })
      },
      ignoreInit = TRUE
    )


    observeEvent(input$GoButton,
      {
        output$testedPlots <- renderPlot({
          isolate(popplot2(
            n(), prevalence(), sensitivity(), specificity(),
            input$sorted, input$ciFlag
          ))
        })
      },
      ignoreInit = TRUE
    )



    # output$distributionplots <- renderPlot({
    #   isolate(distributionplots(n(), prevalence(), sensitivity(), specificity(),
    #                    input$sorted, input$ciFlag))
    # })


    observeEvent(input$GoButton, {

      ## Thanks to Mark Strong for this code
      # https://github.com/Sheffield-Accelerated-VoI/SAVI/blob/master/server.R

      output$downloadReport <- downloadHandler(
        filename = function() { # "my-report.pdf"
          paste("report", sep = ".", switch(
            input$format, PDF = "pdf", HTML = "html", Word = "docx"
          ))
        },

        content = function(file) {
          src <- normalizePath("report.Rmd")
          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, "report.Rmd", overwrite = TRUE)

          library(rmarkdown)
          out <- render(
            input = "report.Rmd", # pdf_document()
            output_format = switch(
              input$format,
              PDF = pdf_document(), HTML = html_document(),
              Word = word_document()
            )
          )
          file.copy(out, file)
        },
        contentType = "text/plain"
      )
    })
  }
)
