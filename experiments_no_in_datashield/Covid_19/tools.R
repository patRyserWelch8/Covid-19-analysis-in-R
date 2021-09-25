
get_country_covid_19 <- function(country = "")
{
    # get the data
    data <- covid19(country = country,
                   level = 1,
                   start = "2010-01-01",
                   end = Sys.Date(),
                   raw = TRUE,
                   vintage = FALSE,
                   verbose = FALSE,
                   ca = TRUE,
                   wb = NULL,
                   gmr = NULL,
                   amr = NULL)

    # data
    data <- as.data.frame(data[,c('date',
                                  'vaccines',
                                  'tests',
                                  'confirmed',
                                  'recovered',
                                  'deaths',
                                  'population',
                                  'latitude',
                                  'longitude')])

    # clean
    data[is.na(data)] = 0
    data[is.null(data)] = 0
    print(head(data))

    # computes ratio in relation to polulation and rate of changes
    recovered     <- data['recovered']/data[1:nrow(data),"population"]
    deaths        <- data['deaths']/data[1:nrow(data),"population"]
    confirmed     <- data['confirmed']/data[1:nrow(data),"population"]
    rates         <- as.data.frame(apply(data[-1],2,diff))

    # extract other data
    latitude      <- data[1:nrow(confirmed),"latitude"]
    longitude     <- data[1:nrow(confirmed),"longitude"]
    dates         <- data[1:nrow(confirmed),"date"]

    # return combined dataset for a country
    assign("data.rates",
            data.frame(confirmed = confirmed,
                      deaths    = deaths,
                      recovered = recovered,
                      latitude  = latitude,
                      longitude = longitude,
                      date      = dates),
           envir = globalenv()
          )
}

combine <- function()
{
  if(!exists("all.rates", where = globalenv()))
  {
    countries <- NULL
  }
  else
  {
    countries <- get("all.rates", envir = globalenv())
  }

  country   <- get("data.rates", envir = globalenv())

  stopifnot(!is.null(country))

  if(is.null(countries))
  {
    assign("all.rates", country, envir = globalenv())
  }
  else
  {
    assign("all.rates", rbind(countries,country), envir = globalenv())
  }
}

get_data <- function(country = "")
{
  get_country_covid_19(country)
  combine()
  return(exists("all.rates", where = globalenv()))
}
