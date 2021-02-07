library(shiny)
library(DT)

slide1 <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("Bases", label = "Seleccione base de datos",
                        choices = c("PREP 2006", "PREP 2012",
                                    "PREP 2018"))
        ),
        mainPanel(
            DT::dataTableOutput("df"),
            width = 12
        )
    )
)

slide2 <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("PREP", label = "Seleccione base del PREP",
                        choices = c("PREP 2006", "PREP 2012",
                                    "PREP 2018"),
                        selected = "PREP 2018"),
            radioButtons("Partcand",
                         label = "Seleccione partido o candidato",
                         choices = c("Partido", "Candidato")),
            selectInput("Estado", label = "Seleccione estado",
                        choices = e_estados[,1])
        ),
        mainPanel(
            plotOutput("Grafica")
        )
    )
)

slide3 <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("Transicion", label = "Seleccione transición",
                        choices = c("Transición 2006-2012",
                                    "Transición 2012-2018"),
                        selected = "Transición 2012-2018")
        ),
        mainPanel(
            DT::dataTableOutput("mat_final")
        )
    )
)

slide4 <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("Indicadores", label = "Seleccione indicadores",
                        choices = c("Indicadores 2009",
                                    "Indicadores 2015"),
                        selected = "Indicadores 2006")
        ),
        mainPanel(
            DT::dataTableOutput("Ind")
        )
    )
)

slide5 <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("Indicadores_reg", label = "Seleccione indicadores",
                        choices = c("Indicadores 2009",
                                    "Indicadores 2015"),
                        selected = "Indicadores 2006")
        ),
        mainPanel(
            verbatimTextOutput("summary"),
            textOutput("Conclusion")
        )
    )
)


shiny::shinyUI(
    htmlTemplate("index1.html",
                 slide1=slide1,
                 slide2=slide2,
                 slide3=slide3,
                 slide4=slide4,
                 slide5=slide5
    )
)
