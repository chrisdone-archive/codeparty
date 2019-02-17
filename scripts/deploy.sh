stack build && strip $(stack exec which codeparty) && scp $(stack exec which codeparty) codeparty:codeparty-new
