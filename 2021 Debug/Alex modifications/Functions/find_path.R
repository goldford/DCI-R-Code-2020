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

path_between <- function(s1, s2){
  
  # Set start and end
  start <- s1
  end <- s2
  
  # Get segment to root paths
  start.path <- path_to_root(s1)
  end.path <- path_to_root(s2)
  
  # Find common ancestor of both paths
  path.ca <- match(start.path, end.path)
  ca.position.end <- (min(path.ca, na.rm = T))
  ca.position.start <- match(ca.position.end, path.ca)
  
  # Get path, reducing by 1 to exclude the common ancestor
  full.path <- start.path[1:ca.position.start]
  #full.path <- append(full.path, end.path[1:ca.position.end - 1])
  
  # Return full path
  return(full.path)
  
}

gather_property <- function(nodes = NULL, property = NULL){
  
  
  
}