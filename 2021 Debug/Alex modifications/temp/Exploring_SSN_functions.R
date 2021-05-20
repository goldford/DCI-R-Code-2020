function (ssn, predpts = NULL, o.write = FALSE, amongpreds = FALSE) 
{
  
  if (is.null(predpts)) {
    pred.num <- 0
  }
  
  # Connect to binary db, each network table holds rID and binaryID
  driver <- RSQLite::SQLite()
  connect.name <- file.path(ssn@path, "binaryID.db")
  connect <- dbConnect(SQLite(), connect.name)
  on.exit({
    dbDisconnect(connect)
  })
  
  # Convert networkID to factor variable
  ssn@obspoints@SSNPoints[[1]]@network.point.coords$NetworkID <- as.factor(ssn@obspoints@SSNPoints[[1]]@network.point.coords$NetworkID)
  # count number of networks
  net.count <- length(levels(ssn@network.line.coords$NetworkID))
  warned.overwrite <- FALSE
  
  # For each individual network
  for (i in 1:net.count) {
    # Get netID
    net.num <- levels(ssn@network.line.coords$NetworkID)[i]
    # Match observations to current network by netID
    ind.obs <- ssn@obspoints@SSNPoints[[1]]@network.point.coords$NetworkID == 
      as.numeric(net.num)
    # Count number of observation in current network
    site.no <- nrow(ssn@obspoints@SSNPoints[[1]]@network.point.coords[ind.obs, 
                                                                      ])
    
    # If perdiction points exist
    if (pred.num > 0) {
      # Match predictions to current network by netID
      ind.preds <- ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords$NetworkID == 
        as.numeric(net.num)
      # Count number of predictions in current network
      pred.site.no <- nrow(ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords[ind.preds, 
                                                                                     ])
    }
    # If no predicitons are supplied
    else {
      pred.site.no <- 0
    }
    
    # If there are some observations
    if (site.no > 0) {
      # Get the pointIDs of each observation in the network (pid)
      obs.pids <- sort(as.numeric(rownames(ssn@obspoints@SSNPoints[[1]]@network.point.coords[ind.obs, 
                                                                                             ])))
      # If prediciton points are supplied do the same with those pids
      if (!is.null(predpts)) {
        pred.pids <- sort(as.numeric(rownames(ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords[ind.preds, 
                                                                                                        ])))
        # If prediction points exist create the empty matrices for distance between observations and predictions
        if (pred.site.no > 0) {
          current_distance_matrix_a <- matrix(NA, nrow = site.no, 
                                              ncol = pred.site.no, dimnames = list(obs.pids, 
                                                                                   pred.pids))
          current_distance_matrix_b <- matrix(NA, nrow = pred.site.no, 
                                              ncol = site.no, dimnames = list(pred.pids, 
                                                                              obs.pids))
        }
      }
      
      # Create network name
      net.name <- paste("net", net.num, sep = "")
      # Create RData files to save workspaces
      workspace.name.a <- paste("dist.net", net.num, 
                                ".a.RData", sep = "")
      workspace.name.b <- paste("dist.net", net.num, 
                                ".b.RData", sep = "")
      # Read in binary table
      bin.table <- dbReadTable(connect, net.name)
      # another RData workspace file name
      workspace.name <- paste("dist.net", net.num, 
                              ".RData", sep = "")
      
      # File write permissions check
      if (!o.write) {
        exists <- file.exists(file.path(ssn@path, "distance", 
                                        "obs", workspace.name))
        if (!missing(predpts) && !is.null(predpts)) {
          exists <- c(exists, file.exists(file.path(ssn@path, 
                                                    "distance", predpts, workspace.name.a)), 
                      file.exists(file.path(ssn@path, "distance", 
                                            predpts, workspace.name.b)))
        }
        if (all(exists)) {
          if (!warned.overwrite) {
            warned.overwrite <- TRUE
            cat("Distance matrices already existed while o.write was set to FALSE. Not overwriting existing matrices\n")
          }
          next
        }
        else if (any(exists) && any(!exists)) {
          stop("o.write was set to FALSE and some (but not all) distance matrices already existed")
        }
      }
      
      # Create empty matrix, rows and cols = pids of observations
      current_distance_matrix <- matrix(NA, nrow = site.no, 
                                        ncol = site.no, dimnames = list(obs.pids, obs.pids))
      # Set diagonal to 0
      diag(current_distance_matrix) <- 0
      # Set row and column names
      rownames(current_distance_matrix) <- obs.pids
      colnames(current_distance_matrix) <- obs.pids
      # Get rowID attribute from network.point.coords list object
      locID.obi <- attributes(ssn@obspoints@SSNPoints[[1]]@network.point.coords[ind.obs, 
                                                                                ])$locID
      # Bind pointID, reachID, and locationID into one dataframe
      ob.i <- as.data.frame(cbind(as.numeric(rownames(ssn@obspoints@SSNPoints[[1]]@network.point.coords[ind.obs, 
                                                                                                        ])), 
                                  as.numeric(levels(ssn@obspoints@SSNPoints[[1]]@network.point.coords$SegmentID[ind.obs]))[ssn@obspoints@SSNPoints[[1]]@network.point.coords$SegmentID[ind.obs]],
                                  locID.obi[ind.obs]))
      colnames(ob.i) <- c("pid", "rid", "locID")
      ob.i$locID <- as.factor(ob.i$locID)
      # Merge in binaryIDs
      ob.i$binaryID <- bin.table$binaryID[match(ob.i$rid, 
                                                bin.table$rid)]
      # Reorder table by pointID and use as row names
      ob.i <- ob.i[order(ob.i[, "pid"]), ]
      rownames(ob.i) <- ob.i$pid
      # Create copy sorted by locID and set all variables numeric
      ob.i_by_locID <- ob.i[order(ob.i[, "locID"]), 
                            ]
      ob.i_by_locID$pid <- as.numeric(ob.i_by_locID$pid)
      ob.i_by_locID$locID <- as.numeric(ob.i_by_locID$locID)
      # Get order by pID value
      ob.j_reordering <- order(ob.i_by_locID$pid)
      # Setup for loop comparisons
      locID.old <- -1
      # Check for locID duplicates
      ind.dup <- !duplicated(ob.i_by_locID$locID)
      
      # For each prediction point
      for (j in 1:nrow(ob.i)) {
        # Get current pID
        pid.i <- ob.i[j, "pid"]
        # Get current locID
        locID.i <- ob.i[j, "locID"]
        # If locID is new
        if (locID.i != locID.old) {
          # Use a pre-compiled DLL file to to find points that lie on the path
          junk <- SSN:::get.rid.fc(ob.i_by_locID[ind.dup, "binaryID"], 
                             ob.i$binaryID[j])
          # Calculate distances
          ob.j <- SSN:::getObsRelationshipsDF(ssn, pid.i, junk, 
                                        ind.dup, ob.i, ob.i_by_locID, bin.table)
          upDist.i <- ssn@obspoints@SSNPoints[[1]]@network.point.coords[paste(pid.i), 
                                                                        "DistanceUpstream"]
          ob.j <- ob.j[ob.j_reordering, ]
          ind.fc <- ob.j$fc == 1
          dist.obs <- ifelse(ind.fc, upDist.i - ob.j$upDist.j, 
                             upDist.i - ob.j$juncDist)
          current_distance_matrix[, paste(pid.i)] <- ifelse(dist.obs < 
                                                              0, 0, dist.obs)
        }
        else {
          current_distance_matrix[, paste(pid.i)] <- current_distance_matrix[, 
                                                                             paste(pid.old)]
        }
        if (locID.i != locID.old) {
          if (!is.null(predpts) && pred.site.no > 0) {
            ob.j <- getPredRelationshipsDF(ssn, pred.num, 
                                           ind.preds, bin.table, ob.i, j)
            ob.j <- ob.j[order(ob.j[, "pid"]), 
                         ]
            ind.fc <- ob.j$fc == 1
            dist.a <- ifelse(ind.fc, ob.j$upDist.j - 
                               upDist.i, ob.j$upDist.j - ob.j$juncDist)
            current_distance_matrix_a[paste(pid.i), ] <- ifelse(dist.a < 
                                                                  0, 0, dist.a)
            dist.b <- ifelse(ind.fc, upDist.i - ob.j$upDist.j, 
                             upDist.i - ob.j$juncDist)
            current_distance_matrix_b[, paste(pid.i)] <- ifelse(dist.b < 
                                                                  0, 0, dist.b)
          }
        }
        else {
          if (!is.null(predpts) && pred.site.no > 0) {
            current_distance_matrix_a[paste(pid.i), ] <- current_distance_matrix_a[paste(pid.old), 
                                                                                   ]
            current_distance_matrix_b[, paste(pid.i)] <- current_distance_matrix_b[, 
                                                                                   paste(pid.old)]
          }
        }
        pid.old <- pid.i
        locID.old <- locID.i
      }
      file_handle = file(file.path(ssn@path, "distance", 
                                   "obs", workspace.name), open = "wb")
      serialize(current_distance_matrix, file_handle, ascii = FALSE)
      close(file_handle)
      if (pred.site.no > 0) {
        file_handle = file(file.path(ssn@path, "distance", 
                                     predpts, workspace.name.a), open = "wb")
        serialize(current_distance_matrix_a, file_handle, 
                  ascii = FALSE)
        close(file_handle)
        file_handle = file(file.path(ssn@path, "distance", 
                                     predpts, workspace.name.b), open = "wb")
        serialize(current_distance_matrix_b, file_handle, 
                  ascii = FALSE)
        close(file_handle)
      }
    }
    if (amongpreds & pred.site.no > 0) {
      workspace.name <- paste("dist.net", net.num, 
                              ".RData", sep = "")
      pred.pids <- sort(as.numeric(rownames(ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords[ind.preds, 
                                                                                                      ])))
      net.name <- paste("net", net.num, sep = "")
      bin.table <- dbReadTable(connect, net.name)
      among_distance_matrix <- amongPredsDistMat(ssn, pred.pids, 
                                                 pred.num, bin.table)
      file_handle = file(file.path(ssn@path, "distance", 
                                   predpts, workspace.name), open = "wb")
      serialize(among_distance_matrix, file_handle, ascii = FALSE)
      close(file_handle)
    }
  }
}

##### Hidden helper functions #####

# get.rid.fc
function (binIDs, referenceBinID) 
{
  ind.match <- .Call("test_fc", binIDs, referenceBinID)
  data.frame(fc = ind.match < 0, binaryID = substr(binIDs, 
                                                   1, abs(ind.match)), stringsAsFactors = FALSE)
}

# test_fc
# This file is a pre-compiled C++ pr C# DLL file. Need to decompile to read code

# getObsRelationshipDF
function (ssn, pid, junk, ind, ob, ob_by_locID, bin) 
{
  ob.j.r <- data.frame(ob_by_locID[ind, c("pid", "locID")], 
                       junk, stringsAsFactors = FALSE)
  ob.j.r$fc <- as.logical(ob.j.r$fc)
  rownames(ob.j.r) <- ob.j.r$pid
  ob.j.r$junc.rid <- bin$rid[match(ob.j.r$binaryID, bin$binaryID)]
  reps <- as.numeric(ob_by_locID$locID)
  ob.j <- ob.j.r[reps, ]
  rownames(ob.j) <- paste(rownames(ob), ".fc", sep = "")
  ob.j$pid <- ob_by_locID$pid
  ob.j$juncDist <- ssn@network.line.coords$DistanceUpstream[match(ob.j$junc.rid, 
                                                                  ssn@network.line.coords$SegmentID)]
  ob.j$upDist.j <- ssn@obspoints@SSNPoints[[1]]@network.point.coords$DistanceUpstream[match(ob.j$pid, 
                                                                                            as.numeric(rownames(ssn@obspoints@SSNPoints[[1]]@network.point.coords)))]
  ob.j
}