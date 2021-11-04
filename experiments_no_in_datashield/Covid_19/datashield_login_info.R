# build connections information

if(IN_SCRIPT)
{

    server.names   <- c("africa", "asia", "oceania", "europe","north america", "south america")

    # Data computers url
    url_africa        <- 'https://192.168.56.100:8443'
    url_asia          <- 'https://192.168.56.100:8443'
    url_oceania       <- 'https://192.168.56.100:8443'
    url_europe        <- 'https://192.168.56.100:8443'
    url_north_america <- 'https://192.168.56.100:8443'
    url_south_america <- 'https://192.168.56.100:8443'
    server.urls       <- c(url_africa,url_asia,url_oceania,url_europe,url_north_america, url_south_america)

    # Assign datasets
    table_africa        <- "COVID19.AFRICA"
    table_asia          <- "COVID19.ASIA"
    table_europe        <- "COVID19.EUROPE"
    table_north_america <- "COVID19.NORTH_AMERICA"
    table_south_america <- "COVID19.SOUTH_AMERICA"
    table_oceania       <- "COVID19.OCEANIA"


    server.tables   <- c(table_africa, table_asia, table_oceania,
                         table_europe, table_north_america, table_south_america)

    # Set user and password to access the DataSHIELD servers
    user_africa         <-  "administrator"
    user_asia           <-  "administrator"
    user_ocenania       <-  "administrator"
    user_europe         <-  "administrator"
    user_north_america  <-  "administrator"
    user_south_america  <-  "administrator"

    server.users.id <- c(user_africa, user_asia, user_ocenania, user_europe,
                         user_north_america, user_south_america)

    password_africa            <-  "datashield_test&"
    password_asia              <-  "datashield_test&"
    password_oceania           <-  "datashield_test&"
    password_europe            <-  "datashield_test&"
    password_north_america     <-  "datashield_test&"
    password_south_america     <-  "datashield_test&"
    server.users.pwd           <-  c(password_africa, password_asia, password_oceania,
                                     password_europe, password_north_america, password_south_america)

    # Set drivers
    driver_africa           <- "OpalDriver"
    driver_asia             <- "OpalDriver"
    driver_oceania          <- "OpalDriver"
    driver_europe           <- "OpalDriver"
    driver_north_america    <- "OpalDriver"
    driver_south_america    <- "OpalDriver"
    server.drivers   <- c(driver_africa,driver_asia,driver_oceania,
                          driver_europe, driver_north_america, driver_south_america)

    # Set SSL drivers
    ssl_options_africa          <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
    ssl_options_asia            <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
    ssl_options_oceania         <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
    ssl_options_europe          <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
    ssl_options_north_america   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
    ssl_options_south_america   <- "list(ssl_verifyhost=0,ssl_verifypeer=0)"
    server.ssl.options          <- c(ssl_options_africa,ssl_options_asia,ssl_options_oceania,
                                     ssl_options_europe, ssl_options_north_america,
                                     ssl_options_south_america)


    builder <- DSI::newDSLoginBuilder()
    builder$append(server = "africa",
                   url = "https://192.168.56.100:8443",
                   user = "administrator",
                   password = "datashield_test&",
                   driver = "OpalDriver")

    # Create login data frame

    login_data <- ds.build.login.data.frame(server.names,
                                            server.urls,
                                            server.tables,
                                            server.users.id,
                                            server.users.pwd,
                                            server.ssl.options,
                                            server.drivers)


    connections <- dsConnectClient::ds.login(login.data.frame = login_data , assign = TRUE, symbol = SERVERS_SYMBOL)


    if(FALSE)
    {
    # Log in to DataSHIELD server did not work
    connections <- ds.login(login.data.frame = login.data, assign = TRUE, symbol = SERVERS_SYMBOL)


    builder$append(server = "asia",
                   url = "https://192.168.56.100:443/",
                   user = "administrator",
                   password = "datashield_test&",
                   driver = "OpalDriver")
    builder$append(server = "europe",
                   url = "https://192.168.56.100:443/",
                   user = "administrator",
                   password = "datashield_test&",
                   driver = "OpalDriver")
    builder$append(server = "oceania",
                   url = "https://192.168.56.100:443/",
                   user = "administrator",
                   password = "datashield_test&",
                   driver = "OpalDriver")
    builder$append(server = "north_america",
                   url = "https://192.168.56.100:443/",
                   user = "administrator",
                   password = "datashield_test&",
                   driver = "OpalDriver")
    builder$append(server = "south_america",
                   url = "https://192.168.56.100:443/",
                   user = "administrator",
                   password = "datashield_test&",
                   driver = "OpalDriver")



    connections <- dsConnectClient::ds.login(login.data.frame = login_data , assign = TRUE, symbol = "D")
    connections
   }

}
