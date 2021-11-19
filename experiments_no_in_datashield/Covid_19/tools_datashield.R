
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


client.compute.bin <- function(x = "data.encrypted", env = globalenv(), datasources)
{
  # server  code - compute min and max from encoded data
  expression   <- call("computeExtremesDS")

  results      <- dsConnectClient::ds.aggregate(expression = expression,
                                                asynchronous = TRUE,
                                                error.stop = TRUE,
                                                datasources = datasources)


  # create map values, var.names and server
  # create a matrix, rows represents each variable and each column a server
  values            <- matrix(unlist(results), nrow = length(results[[1]]), ncol = length(results))
  row.names(values) <- names(results[[1]])
  colnames(values)  <- names(results)

  # extract values for bins
  bins   <- sapply(row.names(values),
                   function(var.name, values)
                   {

                     data <- values[var.name,]
                     data <- sapply(1:length(data),
                                    function(col,data){as.numeric(unlist(strsplit(data[col], ";")))},
                                    data = data)
                     return(c(step = median(data[1,]), first = min(data[2,]), last = max(data[3,])))
                   },
                   values = values)
  # save bins definitions in global env

  assign(".bins.definition", bins, envir = env)

  # summary.results
  settings      <- get(".non.param.setting", envir = env)
  settings$bins <- ".bins.definition"

  # edit settings
  assign(".non.param.setting", settings, envir = env)

  # verify process succeeded
  return(exists(".bins.definition",where = env))
}


compute.rank <- function(x = "data.encrypted", env = globalenv(), datasources)
{

  # assign and compute ranges for each bin for server variable listed as x
  ranges          <- compute.sequence(".bins.definition", env)
  indices         <- 1:(length(ranges[,1]) - 1)

  "issue here and rank data - work with columns - i.e. list...."
  name.var.client <- sapply(names(ranges),
                            function(name)
                            {
                              assign(paste0(".ranks_",name), c(), envir = env)
                              assign(paste0(".max_rank_", name), 0, envir = env)
                              return(exist(paste0(".ranks_",name), envir = env) &
                                       exist(paste0(".max_rank_", name), envir = env))
                            })

  # each range is executed
  sapply(indices,
         function(i, x, name.var.client, ranges, env, datasources)
         {
           if((i+1) <= nrow(ranges))
           {
             # format ranges for server call
             min <- paste(ranges[i,], collapse =";")
             max <- paste(ranges[i+1,], collapse = ";")


             #create bins on the server and bring encoded data to the client
             encoded.data <- create.bin(x = x,
                                             newobj = ".bin",
                                             min = min,
                                             max = max,
                                             env = env,
                                             datasources = datasources)
             rank.bin(x = x,
                           encoded.data = encoded.data,
                           env = env)

             print("problem here with rank.bin")
           }


         },
         x = x,
         name.var.client  = name.var.client,
         ranges           = ranges,
         env              = env,
         datasources      = datasources)


  return(exists(".ranks", envir = env))
}

compute.sequence <- function(client.var.name, env)
{
  # compute bins ranges ....
  bin.ranges  <- get(client.var.name, envir = env)
  #print("sequence")
  #print(bin.ranges)
  ranges      <- lapply(colnames(bin.ranges),
                        function(col, bin.ranges)
                        {
                          min  <- bin.ranges[2,col] - bin.ranges[1,col]
                          max  <- bin.ranges[3,col] + bin.ranges[1,col]
                          step <- bin.ranges[1,col]
                          return(seq(from = min, to = max, by = step))
                        },
                        bin.ranges = bin.ranges)

  max.length = max(lengths(ranges))


  # compute ranges as list ....
  ranges <- sapply(1:length(colnames(bin.ranges)),
                   function(index, max.length, data)
                   {
                     values <- data[[index]]
                     no.NAS <- max.length - length(values)
                     return(c(values, rep(NA, times = no.NAS )))
                   },
                   max.length = max.length,
                   data       = ranges)


  #ranges <- matrix(ranges, ncol = length(colnames(bin.ranges)), nrow = max.length)
  colnames(ranges) <- colnames(bin.ranges)

  return(ranges)

}

create.bin <- function(x, newobj,min,max, env, datasources)
{
  expression   <- call("createBinDS", x, newobj, min, max)

  results      <- dsConnectClient::ds.aggregate(expression = expression,
                                                asynchronous = TRUE,
                                                error.stop = TRUE,
                                                datasources = datasources)

  # retrieve bins from each server


  var.names <- unlist(strsplit(x, ";"))
  results <- dsShareClient::ds.read(data.from.server = var.names ,
                                    data.encrypted = ".bin",
                                    client.side.variable = "temp.data",
                                    no.rows = 1000,
                                    datasources = datasources)

  #print("YYYYYYYYYY")
  #print(results)
  #print(get("temp.data", envir = env))
  # return encoded data in bin data
  if (exists("temp.data", where = env))
  {
    return(get("temp.data", envir = env))
  }
  else
  {
    return(data)
  }
}


rank.bin <- function(x, encoded.data, env)
{
  if(ncol(encoded.data) > 0)
  {
    # compute rank - problem in this funciton .....

    print(1)
    max.rank  <- get(".max_rank", envir = env)
    print(2)
    ranks     <- get(".ranks" , envir = env) #here first time ranks do not exists ...
    print(3)

    bin.ranks <- rank(encoded.data[,1])
    bin.ranks <- bin.ranks + max.rank


    assign(".ranks", c(ranks, bin.ranks), envir = env)
    assign(".max_rank", max(bin.ranks), envir = env)
  }
}
