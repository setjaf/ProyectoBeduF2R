library(shiny)

shinyServer(function(input, output) {
    output$df = DT::renderDataTable({
        if(input$Bases == "PREP 2006"){
            datos_06}
        else if(input$Bases == "PREP 2012"){
            e_estados_2012}
        else{e_estados}
    }, options = list(lengthMenu = list(c(5, 15, 32), c("5", "15", "32")),
                      pageLength = 15), filter = "top", selection = "multiple",
    style = "bootstrap")
    
    output$Grafica <- renderPlot({
        if(input$PREP == "PREP 2018"){
            graficas(input$Partcand, input$Estado)
        }else if(input$PREP == "PREP 2012"){
                graficas_2(input$Partcand, input$Estado)
        }else{
            graficas_3(input$Partcand, input$Estado)
            }
    })
    
    output$mat_final = DT::renderDataTable({
        if(input$Transicion == "Transición 2006-2012"){
            aux_0612
        }else{
            aux_1218
        }
    }, options = list(lengthMenu = list(c(5, 15, 32), c("5", "15", "32")),
                      pageLength = 15), filter = "top", selection = "multiple",
    style = "bootstrap")
    
    output$Ind <- DT::renderDataTable({
        if(input$Indicadores == "Indicadores 2009"){
            Ind_2009
        }else{
            Ind_2015
        }
    }, options = list(lengthMenu = list(c(5, 15, 32), c("5", "15", "32")),
                      pageLength = 15), filter = "top", selection = "multiple",
    style = "bootstrap")
    
    output$summary = renderPrint({
        if(input$Indicadores_reg == "Indicadores 2009"){sal_09}
        else {sal_15}
    })
    
    output$Conclusion <- renderText({
        if(input$Indicadores_reg == "Indicadores 2009"){
            "\nNingún indicador parece ser estadisticamente significativo, siendo
            PIB la variable con el menor p-value."
        }else{"\nNingún indicador parece ser estadisticamente significativo, siendo
            prevalencia delictiva la variable con el menor p-value."}
    })
})
