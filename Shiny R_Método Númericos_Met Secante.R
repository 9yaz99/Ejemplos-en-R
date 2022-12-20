library(shiny)
ui <- fluidPage(
  titlePanel(strong("Método de la secante",align = "center", style = "color:blue")),
  sidebarLayout(
    sidebarPanel( numericInput("x0", "Selecciona x0", min = -100, max = 100, value = 0.1),
                  numericInput("x1", "Selecciona x1", min = -100, max = 100, value = 20),
                  numericInput("es", "Selecciona es", min = 0, max =5, value = 0.5),
                  numericInput("max_iter", "Selecciona el máximo de iteraciones", min = 1, max = 300, value = 20),
                  sliderInput("rango", "Rango de valores de x para graficar", min = -100, max = 100, value =c(-5,5)),
                  textInput("txt","Escribe tu función","sen(x)-x")),
    mainPanel( textOutput("funcion"),
               plotOutput("plot"),
               verbatimTextOutput("x_updated"))
  )
)


server <- function(input, output) {
  f=function(x){sin(x)-x}
  secante =function(f,x0,x1){
    iter=0
    x<-x1-f(x1)*(x0-x1)/(f(x0)-f(x1))
    ea<-100
    es=input$es
    max_iter=input$max_iter
    while(ea>es){
      iter=iter+1
      if(iter>max_iter){
        warning("maximo de iteraciones excedido")
        break
      }
      x<-x1-f(x1)*(x0-x1)/(f(x0)-f(x1))
      x0=x1
      x1=x
      ea<-abs((x-x0)/x)*100 
      cat("iteración",iter,"raiz aproximado",x,"error aproximado",ea,"\n")
    }
  }
  output$funcion<-renderText({input$txt})
  output$plot <- renderPlot({ 
    library(ggplot2)
    dominio=data.frame(x=c(input$rango))
    ggplot(dominio,aes(x=x,color=I("darkblue")))+stat_function(fun=f)+ geom_hline(yintercept = 0)+labs(y=expression(f(x)))
  })
  current_x <- reactive({secante(f,input$x0,input$x1)})
  output$x_updated <- renderPrint({ current_x()})
}


shinyApp(ui = ui, server = server)

