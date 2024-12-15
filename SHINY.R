#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(ggplot2)

# Función cuadrática
f <- function(x) {
  (x - 3)^2
}

# Derivada de la función
f_prime <- function(x) {
  2 * (x - 3)
}

# Descenso de gradiente
gradient_descent <- function(x_start, lr, tol, max_iter) {
  x <- x_start
  path <- data.frame(iter = 0, x = x, f_x = f(x))  # Inicializa el camino
  
  for (i in 1:max_iter) {
    grad <- f_prime(x)
    x <- x - lr * grad  # Actualiza el valor de x
    
    # Guarda el progreso
    path <- rbind(path, data.frame(iter = i, x = x, f_x = f(x)))
    
    # Criterio de parada
    if (abs(grad) < tol) break
  }
  return(path)
}

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Descenso de Gradiente en una Función Cuadrática"),
  sidebarLayout(
    sidebarPanel(
      numericInput("x_start", "Punto inicial (x):", value = 0),
      numericInput("lr", "Tasa de aprendizaje (α):", value = 0.1, step = 0.01),
      numericInput("tol", "Tolerancia:", value = 1e-6),
      numericInput("max_iter", "Máximo de iteraciones:", value = 100),
      actionButton("run", "Ejecutar")
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("results")
    )
  )
)

# Servidor
server <- function(input, output) {
  result <- eventReactive(input$run, {
    gradient_descent(input$x_start, input$lr, input$tol, input$max_iter)
  })
  
  output$results <- renderPrint({
    res <- result()
    last <- tail(res, 1)
    cat("Mínimo encontrado en x =", round(last$x, 6), "\n")
    cat("Valor de f(x):", round(last$f_x, 6), "\n")
    cat("Iteraciones:", nrow(res) - 1)
  })
  
  output$plot <- renderPlot({
    res <- result()
    
    # Gráfica de la función
    x_vals <- seq(-10, 10, length.out = 200)
    y_vals <- f(x_vals)
    
    ggplot() +
      geom_line(aes(x = x_vals, y = y_vals), color = "blue", size = 1) +
      geom_point(data = res, aes(x = x, y = f_x), color = "red", size = 2) +
      labs(title = "Descenso de Gradiente en una Función Cuadrática",
           x = "x", y = "f(x)") +
      theme_minimal()
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)