stack build && strip $(stack exec which codeparty-server) && scp $(stack exec which codeparty-server) codeparty:codeparty-new
