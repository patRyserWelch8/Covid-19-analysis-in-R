
get_country_covid_19 <- function(country = "")
{
    # get the data
    data <- covid19(country = country,
                   level = 1,
                   start = "2010-01-01",
                   end = "2021-10-31",
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

combine <- function(obj.name)
{
  if(!exists(obj.name, where = globalenv()))
  {
    countries <- NULL
  }
  else
  {
    countries <- get(obj.name, envir = globalenv())
  }

  country   <- get("data.rates", envir = globalenv())

  stopifnot(!is.null(country))

  if(is.null(countries))
  {
    assign(obj.name, country, envir = globalenv())
  }
  else
  {
    assign(obj.name, rbind(countries,country), envir = globalenv())
  }
}

get_data <- function(country = "", obj.name = "all.rates")
{
  get_country_covid_19(country)
  combine(obj.name)
  return(exists(obj.name, where = globalenv()))
}
