
assignNonParamSettings <- function(x,data.encrypted,env = globalenv(), datasources)
{
  server.success <- FALSE
  client.success <- FALSE

  names.var.on.server <-  paste(x, collapse=";")


  # sever call and server assignment of settings.
  expression   <- call("assignNonParamSettingsDS",names.var.on.server, data.encrypted)
  results      <- dsConnectClient::ds.aggregate(expression = expression,
                                                asynchronous = TRUE,
                                                error.stop = TRUE,
                                                datasources = datasources)

  #check all the servers have successfully completed the assignments
  server.success <- lapply(results, function(x){x$outcome})
  server.success <- unlist(server.success)
  server.success <- all(TRUE %in% server.success)

  #assign settings on the client
  if(server.success)
  {

    var.to.share <- extract.param(results)

    assign(".non.param.setting", var.to.share, envir = env)

    client.success <- exists(".non.param.setting", envir = env) &
      all(c("a", "b", 'c', 'columns') %in% names(get(".non.param.setting", envir = env)))

  }

  return(server.success & client.success)
}

extract.param <- function(server.results)
{
  #extract information ... may need to do more checks...
  a       <- levels(factor(lapply(server.results,
                                  function(x){l <- x[[3]]; return(unlist(l[grep("param_a", names(l))]))})))
  b       <- levels(factor(lapply(server.results,
                                  function(x){l <- x[[3]]; return(unlist(l[grep("param_b", names(l))]))})))
  c       <- levels(factor(lapply(server.results,
                                  function(x){l <- x[[3]]; return(unlist(l[grep("param_c", names(l))]))})))
  columns <- levels(factor(lapply(server.results,
                                  function(x){l <- x[[3]]; return(unlist(l[grep("param_d", names(l))]))})))


  # the columns should be different on each server.
  if(length(a) == 1 & length(b) == 1 & length(c) == 1 & length(columns) == 1)
  {
    var.to.share <- list(a = a, b = b, c = c, columns = columns)
  }
  return(var.to.share)
}

occult <- function(encode.only = TRUE, env = globalenv(), datasources)
{
  server.success <- FALSE
  client.success <- FALSE

  #print(dsConnectClient::ds.aggregate(expression = call("danger_settings"),
  #ß                                 datasources = datasources))
  expression   <- call("occultDS",encode.only)
  results      <- dsConnectClient::ds.aggregate(expression = expression,
                                                asynchronous = TRUE,
                                                error.stop = TRUE,
                                                datasources = datasources)
  #check all the servers have successfully completed the assignments
  server.success <- lapply(results, function(x){x$success})
  server.success <- unlist(server.success)
  server.success <- all(TRUE == server.success)

  if(server.success)
  {
    encoded.data          <- sapply(results, function(x){x$encoded.name})
    settings              <- get(".non.param.setting", envir = env)
    settings$encoded.data <- encoded.data
    assign(".non.param.setting", settings, envir = env)
    client.success        <- exists(".non.param.setting", where = env)
  }

  return(server.success & client.success)
}

