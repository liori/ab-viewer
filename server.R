library(shiny)
library(dplyr)
library(ggplot2)
library(lambda.r)

options(shiny.error=browser)

is.error <- function(v) {
  inherits(v, "simpleError")
}

track.errors <- function(expr, on.error=NULL) {
  tryCatch(list(value=expr, error=NULL),
           error=function(e) list(value=on.error, error=e))
}

W.E <- function(v, on.error=NULL) {
  if(is.error(v$error)) {
    if (is.null(on.error)) {
      stop(v$error)
    } else {
      on.error
    }
  } else {
    v$value
  }
}

make.reactive.df <- function(input, filename) {
  reactive({
    track.errors({
      df <- read.delim(paste0("~/tmp/vm/measurements/", input[[filename]], ".csv"))

      if (nrow(df) == 0) {
        stop(sprintf("No rows in input data frame %s.", input.filename))
      }
      
      if (!"ttime" %in% names(df)) {
        stop(sprintf("No `ttime` column in input data frame %s.", input.filename))
      }
      
      if (!"url" %in% names(df)) {
        df <- df %.% mutate(url="not given")
      }
      
      pages <- df %.% group_by(url) %.% summarize(count = n(), mean = mean(ttime))
      
      list(data=df, pages=pages)
    }, on.error=list(data=data.frame(ttime=c(), url=c()),
                     pages=data.frame(url=c(), count=c(), mean=c())))
  })
}


shinyServer(function(input, output) {
  
  df1 <- make.reactive.df(input, "dataset1")
  df2 <- make.reactive.df(input, "dataset2")
  
  available.pages <- reactive({
    pages1 <- df1()$value$pages
    pages2 <- df2()$value$pages

    # TODO: full outer join
    if (nrow(pages1)==0 & nrow(pages2)==0) {
      return(list(url=c(), count=c(), mean=c()))
    } else if (nrow(pages1)==0) {
      pages = pages2
    } else if (nrow(pages2)==0) {
      pages = pages1
    } else {
      pages = merge(pages1, pages2, by="url")
    }

    pages %.% arrange(url)
  })
  
  data <- reactive({
    track.errors({
      if (length(input$pages) == 0) {
        stop("No pages selected.")
      }

      d1 <- W.E(df1())$data
      d2 <- W.E(df2())$data
      
      if (nrow(d1)>0) {
        d1 <- d1 %.% filter(url %in% input$pages) %.% mutate(run=1)
      }
      
      if (nrow(d2)>0) {
        d2 <- d2 %.% filter(url %in% input$pages) %.% mutate(run=2)
      }
      
      if (nrow(d1)==0 & nrow(d2)==0) {
        data.frame(ttime=c(), url=c(), run=c())
      } else if (nrow(d1)==0) {
        d2
      } else if (nrow(d2)==0) {
        d1
      } else {
        rbind(d1, d2)
      }
    }, on.error=data.frame(ttime=c(), url=c(), run=c()))
  })
  
  output$pagesUI <- renderUI({
    pages = available.pages()$url
    if (length(pages) > 0) {
      checkboxGroupInput("pages", "Pages to analyze", pages, pages[c(1, 2)])
    }
  })
  
  output$message <- renderText({
    messages <- c()
    add <- function(obj) {
      name <- deparse(substitute(obj))

      if (is.reactive(obj)) {
        obj <- obj()
      }
      
      if (is.list(obj)) {
        obj <- obj$error
      }
      
      if (is.error(obj)) {
        messages <<- c(messages, "[", name, "]: ", conditionMessage(obj), "\n")
      }
    }
    
    add(df1)
    add(df2)
    add(data)
    
    paste0(messages)
  })
  
  output$distPlot <- renderPlot({
    d <- data()$value

    if(nrow(d)!=0) {
      p <- ggplot(d, aes(x=ttime))
      p <- p + geom_density(data=d %.% filter(run==1), fill="red", alpha=0.5)
      p <- p + geom_density(data=d %.% filter(run==2), fill="green", alpha=0.5)
      p <- p + facet_grid(url ~ ., scales="free_y", shrink=TRUE)
      
      print(p)
    }
  })
})
