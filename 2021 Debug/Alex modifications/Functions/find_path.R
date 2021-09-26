path_to_root <- function(label){
  
  # Create empty result container
  path <- vector("character", nchar(label))
  
  # Get path to root
  for(i in 1:nchar(label)){
    path[i] <- substr(label, 1, nchar(label) - (i-1))
  }
  
  # Return result
  return(path)
  
}

# BUG this function has returned duplicate IDs
path_between <- function(s1, s2){
  
  # Get segment to root paths
  start.path <- path_to_root(s1)
  end.path <- path_to_root(s2)
  
  # Match vectors to determine index of common ancestor
  start.in <- which(start.path %in% end.path)[1]
  end.in <- which(end.path %in% start.path)[1]
  
  # Combine path vectors and reduce by 1 to exclude repeating common ancestor
  full.path <- c(start.path[1:start.in], 
                 rev(end.path[1:end.in-1]))
  
  # Return full path
  return(full.path)
  
}
