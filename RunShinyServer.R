setwd("c:/Users/Marco/Desktop/COVID-19-Shiny-Dashboard/COVID-19-Shiny-Dashboard")
load(".RData")
shiny::runApp(appDir = getwd(), port = getOption("shiny.port"),
  launch.browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"), workerId = "",
  quiet = FALSE, display.mode = c("auto", "normal", "showcase"),
  test.mode = getOption("shiny.testmode", FALSE))