#last updated Feb 2022 - G Oldford
################################################################
# CREATE GRAPH OBJECT for DCI w/ Distance Decay
# edge weights are used for distance calculations
# edge data / attributes used for habitat quantity calculations
# #https://www.rdocumentation.org/packages/graph/versions/1.50.0/topics/graphAM-class
create_graph_dd_2020 <- function(adj_matrix_edgelengths=0.0,FIPEX_table=NULL){

    # Create graph object
    # 2020 - different way to call the graphAM function 
    # vs pre-2020
    g_dd <- graphAM(adjMat=adj_matrix_edgelengths,  edgemode="directed", values=list(weight=1))

    # associate passabilities with nodes using NodeData slot
    # e.g. nodeData(g,n=c("b", "c"), attr ="color") <- "red"
    nodeDataDefaults(g_dd, attr ="pass") <- 1.0
    nodeData(g_dd,n=as.character(FIPEX_table$NodeEID), attr="pass") <- as.double(FIPEX_table$BarrierPerm)
    #nd <- nodes(g_dd)

    nodeDataDefaults(g_dd, attr ="nodelabel") <- "none"
    nodeData(g_dd,n=as.character(FIPEX_table$NodeEID), attr="nodelabel") <- as.character(FIPEX_table$NodeLabel)
    nodeData(g_dd,n="sink", attr="nodelabel") <- "sink"
    #nd <- nodes(g_dd)

    nodeDataDefaults(g_dd, attr ="downnodelabel") <- "none"
    nodeData(g_dd,n=as.character(FIPEX_table$NodeEID), attr="downnodelabel") <- as.character(FIPEX_table$DownstreamNodeLabel)
    #nd <- nodes(g_dd)

    nodeDataDefaults(g_dd, attr ="natural") <- "none"
    nodeData(g_dd,n=as.character(FIPEX_table$NodeEID), attr="natural") <- FIPEX_table$NaturalTF
    nodeData(g_dd,n="sink", attr="natural") <- FALSE
    #nd <- nodes(g_dd)

    # optionally can give edges attributes
    #edgeDataDefaults(g_dd, attr="name")<-"noname"
    #edgeData(self, from, to, attr)
    #edgeData(self, from, to, attr) <- value
    edgeDataDefaults(g_dd, attr="HabitatQuan")<-0.0
    edgeData(g_dd,from=as.character(FIPEX_table$NodeEID), 
         to=as.character(FIPEX_table$DownstreamEID), 
         attr="HabitatQuan")<-as.double(FIPEX_table$HabQuantity)
    # reverse - attr associated with each direction along one edge
    edgeData(g_dd,from=as.character(FIPEX_table$DownstreamEID), 
         to=as.character(FIPEX_table$NodeEID), 
         attr="HabitatQuan")<-as.double(FIPEX_table$HabQuantity)


    # give edges an easy-to-access name insensitive to direction
    # this is done to quickly identify duplicates later
    # there may be alternatives such as accessing edgeNames but I suspect
    # they are slower than this
    edgeDataDefaults(g_dd, attr="EdgeNameGO")<-"init"
    edgeData(g_dd,from=as.character(FIPEX_table$NodeEID), 
         to=as.character(FIPEX_table$DownstreamEID), 
         attr="EdgeNameGO")<-paste(as.character(FIPEX_table$DownstreamEID),
                                   as.character(FIPEX_table$NodeEID),
                                   sep="-")
    # reverse - attr associated with each direction along one edge
    edgeData(g_dd,from=as.character(FIPEX_table$DownstreamEID), 
         to=as.character(FIPEX_table$NodeEID), 
         attr="EdgeNameGO")<-paste(as.character(FIPEX_table$DownstreamEID),
                                   as.character(FIPEX_table$NodeEID),
                                   sep="-")
    return(g_dd)
}

################################################################################
# get all distances and paths (from penult) to all nodes from Sink / all nodes
# https://www.rdocumentation.org/packages/RBGL/versions/1.48.1/topics/dijkstra.sp
# note it must be node-node - no edge-edge possible
# note using this function repeatedly during DCIp is inefficient 
# !!! (should use BFS w/ LCA i.e., custom algorithm) !!!
#   (cannot edit the source for Djikstra.sp because it's actually an 
#   interface to C++ 'Boost'library for graphs - can't get edge and node attributes 
#   during net traversal)

get_paths_distances <- function(g=NULL,fromnode="sink"){
    dijkstra.sp(g,fromnode,eW=unlist(edgeWeights(g)))
    
    # TO DO: ALTERNATIVES FOR BENCHMARKING
}

##############################################################################
##### SUMMARY TABLE 2020 #####

# replaces similar pre-2020 function to create a table for each edge-edge pair
# Includes options for alternative data management for benchmarking
# (code could be trimmed).
# this function could be sped up with custom algorithm that can find path 
# while also grabbing attribute data (BFS w/ LCA). 
# - G Oldford, 2020

# gets cumulative passability each pair using path info
# and get other attributes

# pseudocode:
# for each 'from node' (e.g., sink in DCId, and all nodes in DCIp)
#  get paths between node and all other node
#
#  for each 'to node' in 'all paths' results
#   get the first edge len and hab traversed from node to sink
#
#   store length and hab of the edge between 'to node' and first node encountered
#   in path back to 'from node' (i.e., the 'to edge')

#   do while next node name <> "from node"
#     pass = nodeData(g_dd, nextnode, "pass")
#     cumulativepass =  cumulativepass * pass
#     nextnode = the next node in path towards 'from node'
#     if last edge traversed on the way to 'from node'
#       store the length and habitat of this edge which is the 'from edge'
#     if there is a maxdistance set for distance decay, 
#       add a TRUE/FALSE column to indicate this
#   
#   
#   add various other attributes to master table (attr's from g object)

# requires library(data.table)
# data.table vs other options likely to speed things up for large networks
#https://rstudio-pubs-static.s3.amazonaws.com/406521_7fc7b6c1dc374e9b8860e15a699d8bb0.html
#https://www.rdocumentation.org/packages/data.table/versions/1.13.0/topics/rbindlist

get_summary_tab_2020 <- function(option="dt-lists",
                                 naturalonly=FALSE,
                                 g = NULL,
                                 DCIp=FALSE,
                                 bDistanceLim=FALSE,
                                 dMaxDist=0.0){
    
    # funciton params:
    # option - for benchmarking speed of appending to table
    # naturalonly - will calculate pass-weighted path distances
    #  while ignoring non-natural barriers
    # g - the graph object (rbgl GraphAM in BioconductR)
    # DCIp - TRUE / FALSE will trigger loop that finds path
    #       between all nodes, node just sink
    # initialize empty data object in different ways
    
    # for different options and benchmarking:
    DT2 = data.table(FromNode="init",
                 ToNode="init",
                 FromNodeLabel="init",
                 ToNodeLabel="init",
                 CumulativePass=0.0,
                 FromEdgeLen=0.0,
                 ToEdgeLen=0.0,
                 TotalDist=0.0,
                 DistMinusStartEndLen=0.0,
                 DistMinusSEExceedsThreshold=FALSE,
                 FromEdgeHab=0.0,
                 ToEdgeHab=0.0,
                 ToEdgeName="init",
                 FromEdgeName="init",
                 ToFromEdgeNameCombo="init")
    
    DF2 = data.frame(FromNode="init",
                 ToNode="init",
                 FromNodeLabel="init",
                 ToNodeLabel="init",
                 CumulativePass=0.0,
                 FromEdgeLen=0.0,
                 ToEdgeLen=0.0,
                 TotalDist=0.0,
                 DistMinusStartEndLen=0.0,
                 DistMinusSEExceedsThreshold=FALSE,
                 FromEdgeHab=0.0,
                 ToEdgeHab=0.0,
                 ToEdgeName="init",
                 FromEdgeName="init",
                 ToFromEdgeNameCombo="init", stringsAsFactors=F)
    
    # lists in R must have size pre-allocated 
    # size of our table is almost n^2 - n*(n-1) 
    # (less than n^2 since not getting distance from node to itself)
    if(DCIp==FALSE){
        outlist <- vector("list", length(numNodes(g_dd)))
    }else{
        outlist <- vector("list", length(numNodes(g_dd)*(numNodes(g_dd)-1)))   
    }
    
    outlist[[numNodes(g_dd)]] <- list(FromNode="init",
                 ToNode="init",
                 FromNodeLabel="init",
                 ToNodeLabel="init",
                 CumulativePass=0.0,
                 FromEdgeLen=0.0,
                 ToEdgeLen=0.0,
                 TotalDist=0.0,
                 DistMinusStartEndLen=0.0,
                 DistMinusSEExceedsThreshold=FALSE,
                 FromEdgeHab=0.0,
                 ToEdgeHab=0.0,
                 FromEdgeName="init",
                 ToEdgeName="init",
                 ToFromEdgeNameCombo="init")

    # from = sink / start node
    # to = other nodes
    if(DCIp==FALSE){
        fromnodecount=1
    }else{
        fromnodecount=numNodes(g_dd)
    }
    
    bDistMinusSEExceedsThreshold = FALSE
    count = 0
    for (j in 1:fromnodecount){
        
        if(DCIp==FALSE){
            fromnode_name = "sink"
            fromnode_label = "sink" 
        }else{
            fromnode_name = nodes(g_dd)[j]
             if (fromnode_name=="sink"){
                fromnode_label = "sink"
            }else{
                fromnode_label = nodeData(g_dd, fromnode_name, "nodelabel")[[1]]  
            }
        }
        
        ###########################################################
        #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # get path & distances between 'fromnode' and all other nodes
        paths_distances <- get_paths_distances(g,fromnode_name)
        # this can be time consuming
        
    for (k in 1:length(paths_distances$penult)) {
        
        tonode <- paths_distances$penult[k]     
        tonode_name = names(tonode)
        tonode_name <- tonode_name[[1]]
        tonode_label = nodeData(g_dd, tonode_name, "nodelabel")[[1]]
    
        if (tonode_name == fromnode_name){
            # not interested in distance from one node to itself
            next
        }
        count = count+1
        # initialize
        cumulativepass = 1.0
        pass = 1.0 # watch not to take pass from to/from end nodes since traversal starts at edge
        totaldistance = paths_distances$distances[tonode_name]
        totaldistance <- totaldistance[[1]]     
        
        # get length of edge 
        nextnode = paths_distances$penult[tonode]
        nextnode_name = names(nextnode)
        lastnode_name = tonode_name
    
        # get the last edge length traversed on the way to 'to node'
        # alternatively could grab the weight for this edge instead of subtraction
        toedgelen = totaldistance - paths_distances$distances[nextnode_name]
        toedgelen <- toedgelen[[1]]
        toedgedata =edgeData(g_dd, tonode_name,nextnode_name)
        toedgehab = toedgedata[[1]]$HabitatQuan
        toedgename = toedgedata[[1]]$EdgeNameGO
        
        exitvar = "go"
        while (exitvar != "stop"){
        
            if(nextnode_name != fromnode_name){
                pass = nodeData(g_dd, nextnode_name, "pass")
                if(naturalonly==FALSE){
                    cumulativepass = cumulativepass * pass[[1]]
                }else{
                    natural = nodeData(g_dd,nextnode_name,"natural")
                    if(natural[[1]]==TRUE){
                        cumulativepass = cumulativepass * pass[[1]]
                    }
                }
            }else{
                fromedgelen = paths_distances$distances[lastnode_name]
                fromedgelen <- fromedgelen[[1]]
                fromedgedata = edgeData(g_dd, lastnode_name,fromnode_name)
                fromedgehab = fromedgedata[[1]]$HabitatQuan
                fromedgename = fromedgedata[[1]]$EdgeNameGO
                
                exitvar="stop"
            }
            
            lastnode_name = nextnode_name
            nextnode = paths_distances$penult[nextnode]
            nextnode_name = names(nextnode)
        }
        
       distminusstartendlen = totaldistance - toedgelen - fromedgelen
       # less than zero distance indicates it's an edge-to-itself distance
       # correct for this
       if (distminusstartendlen<0){
          distminusstartendlen = 0 
       }
        
       tofromedgename_combo = paste(toedgename,fromedgename,sep="|")
       
       if (bDistanceLim == TRUE){
           if (distminusstartendlen > dMaxDist){
               bDistMinusSEExceedsThreshold=TRUE
           }else{
               bDistMinusSEExceedsThreshold=FALSE
           }
       }else{
           bDistMinusSEExceedsThreshold=FALSE
       }
       
        if (option=="dt"){
            #print(cumulativepass)
            #https://www.rdocumentation.org/packages/data.table/versions/1.13.0/topics/rbindlist
            DT1 = data.table(FromNode=fromnode_name,
                     ToNode=tonode_name,
                     FromNodeLabel=fromnode_label,
                     ToNodeLabel=tonode_label,
                     CumulativePass=cumulativepass, 
                     FromEdgeLen=fromedgelen,
                     ToEdgeLen=toedgelen,
                     TotalDist=totaldistance,
                     DistMinusStartEndLen=distminusstartendlen,
                     DistMinusSEExceedsThreshold = as.logical(bDistMinusSEExceedsThreshold),
                     FromEdgeHabLen=fromedgehablen,
                     ToEdgeHab=toedgehab,
                     FromEdgeHab=fromedgehab,
                     FromEdgeName=fromedgename,
                     ToEdgeName=toedgename,
                     ToFromEdgeNameCombo=tofromedgename_combo)
            l = list(DT1,DT2)
            
            DT2 = rbindlist(l, use.names=TRUE)
        }else if(option=="dt-lists"){
            # append lists to list rather than work yet with tables
            DL1 = list(FromNode=fromnode_name,
                     ToNode=tonode_name,
                     FromNodeLabel=fromnode_label,
                     ToNodeLabel=tonode_label,
                     CumulativePass=cumulativepass, 
                     FromEdgeLen=fromedgelen,
                     ToEdgeLen=toedgelen,
                     TotalDist=totaldistance,
                     DistMinusStartEndLen=distminusstartendlen,
                     DistMinusSEExceedsThreshold = as.logical(bDistMinusSEExceedsThreshold),
                     FromEdgeHab=fromedgehab,
                     ToEdgeHab=toedgehab,
                     FromEdgeName=fromedgename,
                     ToEdgeName=toedgename,
                     ToFromEdgeNameCombo=tofromedgename_combo)
            #print("Length DL1: ")
            #print(length(DL1))
            outlist[[count]] <- (DL1)
            
        }else if(option=="df"){
            DF1 = data.frame(FromNode=fromnode_name,
                     ToNode=tonode_name,
                     FromNodeLabel=fromnode_label,
                     ToNodeLabel=tonode_label,
                     CumulativePass=cumulativepass, 
                     FromEdgeLen=fromedgelen,
                     ToEdgeLen=toedgelen,
                     TotalDist=totaldistance,
                     DistMinusStartEndLen=distminusstartendlen,
                     DistMinusSEExceedsThreshold = as.logical(bDistMinusSEExceedsThreshold),
                     FromEdgeHab=fromedgehab,
                     ToEdgeHab=toedgehab,
                     FromEdgeName=fromedgename,
                     ToEdgeName=toedgename,
                     ToFromEdgeNameCombo=tofromedgename_combo)
            #print(fromedgehabarea)
            #print(DF1)
            DF2 <- rbind(DF2, DF1)
          }else if(option=="df-lists"){
            DL1 = list(FromNode=as.character(fromnode_name),
                     ToNode=as.character(tonode_name),
                     FromNodeLabel=as.character(fromnode_label),
                     ToNodeLabel=as.character(tonode_label),
                     CumulativePass=as.numeric(cumulativepass), 
                     FromEdgeLen=as.numeric(fromedgelen),
                     ToEdgeLen=as.numeric(toedgelen),
                     TotalDist=as.numeric(totaldistance),
                     DistMinusStartEndLen=as.numeric(distminusstartendlen),
                     DistMinusSEExceedsThreshold = as.logical(bDistMinusSEExceedsThreshold),
                     FromEdgeHab=fromedgehab,
                     ToEdgeHab=toedgehab,
                     FromEdgeName=as.character(fromedgename),
                     ToEdgeName=as.character(toedgename),
                     ToFromEdgeNameCombo=as.character(tofromedgename_combo))
            outlist[[count]] <- (DL1)
        }else if(option=="dplyr"){
             DL1 = list(FromNode=as.character(fromnode_name),
                     ToNode=as.character(tonode_name),
                     FromNodeLabel=as.character(fromnode_label),
                     ToNodeLabel=as.character(tonode_label),
                     CumulativePass=as.numeric(cumulativepass), 
                     FromEdgeLen=as.numeric(fromedgelen),
                     ToEdgeLen=as.numeric(toedgelen),
                     TotalDist=as.numeric(totaldistance),
                     DistMinusStartEndLen=as.numeric(distminusstartendlen),
                     DistMinusSEExceedsThreshold = as.logical(bDistMinusSEExceedsThreshold),
                     FromEdgeHab=as.numeric(fromedgehab),
                     ToEdgeHab=as.numeric(toedgehab),
                     FromEdgeName=fromedgename,
                     ToEdgeName=toedgename,
                     ToFromEdgeNameCombo=tofromedgename_combo)
            #print(row1)
            DF2 <- bind_rows(DF2,DL1)
        }
    } #k
    } #j
        
    if(option=="dt"){
        sum_tab <- DT2
    }else if(option=="dt-lists"){
        DT2 <- data.table(rbindlist(outlist))
        
        sum_tab <- DT2
    }else if(option=="df"){
        #DF2 <- DF2[!duplicated(DF2$ToFromEdgeNameCombo), ]
        sum_tab <- DF2
    }else if(option=="df-lists"){
        DF2 <- data.frame(do.call(rbind, outlist))
        #DF2 <- DF2[!duplicated(DF2$ToFromEdgeNameCombo), ]
        sum_tab <- DF2
    }else if(option=="dplyr"){
        sum_tab <- DF2
    }
        
    # if a distance limit, eliminate rows
    if(bDistanceLim == TRUE){
        return(sum_tab[DistMinusSEExceedsThreshold==FALSE])
    }else{
        return(sum_tab) 
    } 
          
}

#######################################################
###### Calculate DCI #####
#dci_calc_2020_dd <- function(){}
# warning: the sum_tab_2020 must be a data.table
#          only some data frames work ('dplyr' is ok not
#          the 'df-lists' option)


######### (1) DCId - calc_DCId #########
calc_DCId_2020 <- function(sum_tab_2020=NULL,
                           totalhabitat=0.0,
                           FromNode="sink",
                           bDistanceLim=FALSE){
    
    # filter table
    DCId_data<-subset(sum_tab_2020,FromNode=="sink")
    
    # Credit to Chris Edge Code for avoiding loops in R here: 
    if(bDistanceLim==FALSE){
        DCId_data$temp <- DCId_data$CumulativePass * (DCId_data$ToEdgeHab/totalhabitat)
    }else{
        DCId_data$temp <- DCId_data$CumulativePass * (DCId_data$ToEdgeHabMaxAccessible/DCId_data$MaxTotalAccessHabFromEdge)
    }
    DCId <- sum(DCId_data$temp)
    return(round(DCId*100,2))
} # DCId

######### (2) DCIp - calc_DCIp #########
# Credit to C Edge for shorter code
calc_DCIp_2020 <- function(sum_tab_2020=NULL,
                           totalhabitat=0.0,
                           option="unique",
                           bDistanceLim=FALSE){
    
    # 'option' = unique / distinct for benchmarking speeds
    # unique = data.table / base r
    # distinct = dplyr
    # to do: sometimes sum_tab may be a data.table sometimes data.frame
    if(option=="unique"){
        sum_tab_2020 <- unique(sum_tab_2020, by = "ToFromEdgeNameCombo")
    }else{
        sum_tab_2020 <- distinct(sum_tab_2020, ToFromEdgeNameCombo, .keep_all = TRUE)
    }

    DCIp <- 0
    if(bDistanceLim==FALSE){
        for (i in 1:nrow(sum_tab_2020)) {
            DCIp <- DCIp + (sum_tab_2020$CumulativePass[i] * (sum_tab_2020$FromEdgeHab[i]/totalhabitat)) * (sum_tab_2020$ToEdgeHab[i]/totalhabitat)
        }
    }else{
        for (i in 1:nrow(sum_tab_2020)) {
            DCIp <- DCIp + (sum_tab_2020$CumulativePass[i] * (sum_tab_2020$FromEdgeHab[i]/totalhabitat))* (sum_tab_2020$ToEdgeHabMaxAccessible[i]/sum_tab_2020$MaxTotalAccessHabFromEdge[i]) 
        }
    }
    return(round(DCIp*100,2))
}

######### (3) DCIs - calc_DCIs #########
# can be added into (2) as option see below
calc_DCIs_2020 <- function (sum_tab_2020=NULL,
                            totalhabitat=0.0,
                            option="dt",
                            bDistanceLim=FALSE){
    
    # option: "dt","dplyr","old"
    # alternative methods for benchmarking
    if(option=="dt"){
        
        DCIs <- sum_tab_2020
        # remove duplicates
        DCIs <- unique(sum_tab_2020, by = "ToFromEdgeNameCombo")
        
        if(bDistanceLim==FALSE){
            # select only a required columns
            cols = c("FromEdgeName","ToEdgeHab","CumulativePass")
            DCIs <- DCIs[,..cols]
            # first step to DCIs
            DCIs[, DCIs_i := round(ToEdgeHab/totalhabitat * CumulativePass * 100,2)]
        }else{
            # select only a required columns
            cols = c("FromEdgeName","ToEdgeHabMaxAccessible","CumulativePass","MaxTotalAccessHabFromEdge")
            DCIs <- DCIs[,..cols]
            # first step to DCIs
            DCIs[, DCIs_i := round(ToEdgeHabMaxAccessible/MaxTotalAccessHabFromEdge * CumulativePass * 100,2)]
        }
        
        cols = c("FromEdgeName","DCIs_i")
        DCIs <- DCIs[,..cols]
        # second step to DCIs
        DCIs <- DCIs[, lapply(.SD,sum), by=.(FromEdgeName)]
        
    }else if(option=="dplyr"){
        
        if(bDistanceLim==FALSE){
            DCIs <- sum_tab_2020 %>%
            distinct(ToFromEdgeNameCombo, .keep_all = TRUE) %>%
            mutate(DCIs_i = CumulativePass * ToEdgeHab/totalhabitat * 100) %>%
            select(DCIs_i,FromEdgeName) %>%
            group_by(FromEdgeName) %>%
            summarise(DCIs = sum(DCIs_i))
        }else{
            DCIs <- sum_tab_2020 %>%
            distinct(ToFromEdgeNameCombo, .keep_all = TRUE) %>%
            mutate(DCIs_i = CumulativePass * ToEdgeHabMaxAccessible/MaxTotalAccessHabFromEdge)*100 %>%
            select(DCIs_i,FromEdgeName) %>%
            group_by(FromEdgeName) %>%
            summarise(DCIs = sum(DCIs_i))
        }
        
        
    }else if(option=="old"){
        sum_tab_2020 <- unique(sum_tab_2020, by = "ToFromEdgeNameCombo")
        sections<-as.vector(unique(sum_tab_2020$FromEdgeName))
        # store the all section results in DCI.as
        DCI_as<-NULL
        
        for(s in 1:length(sections)){
            DCI_s<-0
            # Old notes:
            # select out only the data that corresponds to pathways from one sectino 
            # to all other sections
            d_nrows<-subset(sum_tab_2020, FromEdgeName==sections[s])
            d_sum_table<-d_nrows
            
            if(bDistanceLim==FALSE){
                for (a in 1:dim(d_nrows)[1]){
                    # Old note:
                    #to get the DCI for diadromous fish, use the following formula: 
                    # DCId= li/L*Cj (where j= the product of the passability in the pathway)
                    la<-d_sum_table$ToEdgeHab[a]/sum(FIPEX_table$HabQuantity)
                    pass_d<-d_sum_table$CumulativePass[a]
                    DCI_s<-round(DCI_s+la*pass_d*100, digits=2)
                } # end loop over sections for dci calc
            }else{
                for (a in 1:dim(d_nrows)[1]){
                    # Old note:
                    #to get the DCI for diadromous fish, use the following formula: 
                    # DCId= li/L*Cj (where j= the product of the passability in the pathway)
                    la<-d_sum_table$ToEdgeHabMaxAccessible[a]/d_sum_table$MaxTotalAccessHabFromEdge
                    pass_d<-d_sum_table$CumulativePass[a]
                    DCI_s<-round(DCI_s+la*pass_d*100, digits=2)
                } # end loop over sections for dci calc
            }
            DCI_as[s]<-round(DCI_s*100,2)
        } # end loop over "first" sections	

        # STORE RESULTS IN .CSV file
        DCIs<-data.frame(sections,DCI_as)
    }else{
        print("error in options passed to calc_DCIs")
        DCIs <- 0.0
    }
    return(DCIs)
}

apply_distance_limits <- function(sum_tab_2020 = NULL, 
                                bDistanceLim=FALSE, 
                                dMaxDist=0.0,
                                bDistanceDecay=FALSE,
                                sDDFunction="none"){
    
    ### Calculate proportion of start-end distances for segments < dmax
    ### 'a','b' = max,min dist possibly travelled
    ### is destination segment reachable? based on min dist 'b'
    
    
    #sum_tab_2020 <- sum_tab_2020 %>%
    #mutate(ToEdgeHabProp = (DistMinusStartEndLen+ToEdgeLen-dMaxDist)/ToEdgeLen) %>%
    #mutate(ToEdgeHabProp = ifelse(ToEdgeHabProp>=0,1-ToEdgeHabProp,1)) %>%
    #mutate(ToEdgeHabMaxAccessible = ToEdgeHabProp * ToEdgeHab) %>%
    #select(-ToEdgeHabProp)
    
    # function changes in 2022
    get_max_accessible <- function(DistMinusStartEndLen,ToEdgeLen,
                                   ToEdgeHab,dMaxDist,FromEdgeLen, 
                                   FromEdgeName, ToEdgeName){

        reachable_shortest_b = (dMaxDist-DistMinusStartEndLen)/ToEdgeLen
        reachable_shortest_b = ifelse(reachable_shortest_b>0,1,reachable_shortest_b)
        
        prop_reachable_a = ifelse(FromEdgeName==ToEdgeName,
                                  (dMaxDist)/ToEdgeLen,
                                  (dMaxDist-DistMinusStartEndLen-FromEdgeLen)/ToEdgeLen)
        prop_reachable_a = ifelse(prop_reachable_a>0,prop_reachable_a,0)
        prop_reachable_a = ifelse(prop_reachable_a>1,1,prop_reachable_a)

        
        #ifelse statements for vectors, not testing a single val
        # numerator is avg of prop_reachable_b (always 1) so removed and prop_reachable_a 
        toedge_maxaccessible = ifelse(reachable_shortest_b==0,0,((prop_reachable_a * ToEdgeHab)+ ToEdgeHab)/ 2)
        print(reachable_shortest_b)
        print(prop_reachable_a)
        toedge_maxaccessible
        
        
        # old code from 2021
#         prop_accessible = (dMaxDist-DistMinusStartEndLen)/ToEdgeLen
#         prop_accessible = ifelse(prop_accessible>0,prop_accessible,0)
#         prop_accessible = ifelse(prop_accessible>1,1,prop_accessible)
#         toedge_maxaccessible = prop_accessible * ToEdgeHab

        toedge_maxaccessible
    }

    sum_tab_2020[, ToEdgeHabMaxAccessible := get_max_accessible(DistMinusStartEndLen,
                                                                ToEdgeLen,ToEdgeHab,
                                                                dMaxDist,FromEdgeLen,
                                                                FromEdgeName, ToEdgeName)]
    
    if(bDistanceDecay==TRUE & sDDFunction!="none"){
        sum_tab_2020 <- apply_distance_decay(sum_tab_2020,sDDFunction,dMaxDist)
        # overwriting here because totals below should be based on weighted hab
        sum_tab_2020$ToEdgeHabMaxAccessible = sum_tab_2020$toedgehabaccessible_dd
    }
    
    # sum habitat accessible from edge and add as attribute
    # in new column 
    # remove duplicates first...
        # to do: dplyr is slower than data.table - re-code and benchmark
    sum_tab_hab <- sum_tab_2020 %>% 
    distinct(ToFromEdgeNameCombo, .keep_all = TRUE) %>%
    group_by(FromEdgeName) %>%
    summarise(MaxTotalAccessHabFromEdge = sum(ToEdgeHabMaxAccessible))

    sum_tab_2020 <- sum_tab_2020 %>%
    left_join(sum_tab_hab, by = "FromEdgeName", copy=FALSE)
    
    return(data.table(sum_tab_2020))
}

apply_distance_decay <- function(sum_tab_2020=NULL,
                              sDDFunction="none",
                              dMaxDist = 0.0){

# distance decay options: "linear" - linear (1-x), 
#                         "natexp1"- natural exponential #1 (general form: e^x), 
#                         "natexp2" - natural exponential #2 (general form: e^x^2), 
#                         "circle" - based on equation of circle ((1-x^2)^0.5)
#                         "sigmoid" - sigmoid (general form:1/(1+e^x)
# functions chosen because they can be integrated analytically.
# for analytical solutions see documentation. 
# general form modified so intercepts are (0,1),(1,0) - sometimes approximate
# G Oldford, 2020

# to do: data.tables with many columns don't perform well and melt() may 
#        help

########## General Formulas ##########
#  multiplies the 'maximum accessible habitat' at edge j by f_avg(a,b)
# (max accessible is pre-calculated earlier using cut-off value and distance of edge j from edge i)
# ===== GO Feb 2022 - changed 'b' to add in the start edge len i (see write-up)
# 
# avg value of a dd function:
# f_avg(a,b,f(x)) = 1/(b-a)*integral_a_to_b(f(x)dx)
#           represents average value of distance decay function f(x) between 
#           two positions, a and b, along total distance from end of edge i to maxdist
#           where a and b are positions from 0 to maxdist (maxdist is always
#           rescaled to 1. 
# 'a' - proportion of maxdist reached at start of edge j 
# 'b' - proportion of maxdist reached at end of edge j 
    e = exp(1)
    
########## LINEAR distance decay function ##########
# function: (1-x) where x is proportion of maxdist
# f_avg = ((1-b)+(1-a))/2 
# toedgehabaccessible_dd = f_avg(a,b)*ToEdgeHabMaxAccessible
    f_avg_linear <- function(a,b) {((1-b)+(1-a))/2}

########## natural exponential DD Function #1 ########## 
########## general form: (-e^x) ##########
# parameterized function: f(x) = 1-e^(5(x-5)) with zero intercepts (0,1),(1,0)
# integral of f(x) = F_int = x - e^(5(x-1))/5)
# f_avg(a,b,f(x)) = 1/(b-a)*(b - e^(5(b-1))/5 -(a - e^(5(a-1))/5))
    integral_fx_natexp1 <- function(x){x-e^(5*(x-1))/5}
    f_avg_natexp1 <- function(a,b) {(1/(b-a)*(integral_fx_natexp1(b)-integral_fx_natexp1(a)))}

########## natural exponential DD Function #2 ########## 
########## general form: -e^x^2 ##########
# parameterized function: f(x) = 2 - e^((x^2)*1/1.44)
# integral of f(x) = F_int = ((pi*i)^1/3 * erf(5*i*x/6)) / 5 + 2*x
# f_avg(a,b,f(x)) = to do
    # Note complete - can't be done without special erfi function 
    erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
    integral_fx_natexp2 <- function(x){((pi*i)^1/3 * erf(5*i*x/6)) / 5 + 2*x}

########## CIRCULAR DD Function ########## 
########## general form: (1-x^2)^0.5  ##########
# parameterized function: f(x) = (1-x^2)^0.5
# integral of f(x) = F_int = (asin(x)+x*(1-x^2)^0.5)/2
# f_avg(a,b,f(x)) = (asin(b)+b*(1-b^2)^0.5)/2 - (asin(a)+a*(1-a^2)^0.5)/2
    integral_fx_circle <- function(x){(asin(x)+x*(1-x^2)^0.5)/2}
    f_avg_circle <- function(a,b){((asin(b)+b*(1-b^2)^0.5)/2 - (asin(a)+a*(1-a^2)^0.5)/2)}


###### SIGMOID DD function ###### 
#      general form: 1/(e^x+1))  #
# parameterized function: f(x) = 1/(e^(10(x*0.5)))+1
# integral f(x) = F_int = x - ln(e^10x+e^5)/10
# f_avg(a,b,f(x)) = 1/(b-a)*(b - ln(e^10b+e^5)/10- a - ln(e^10a+e^5)/10)
    integral_fx_sigmoid <- function(x){x - log(e^10*x+e^5)/10}
    f_avg_sigmoid <- function(a,b) {(1/(b-a)*(integral_fx_sigmoid(b)-integral_fx_sigmoid(a)))}

##### Calculate distance-decay weighted habitat accessible ######
##### (at each edge j from each each i)
# ========= GO Feb 2022 - modified a to be from node at start of line ====
    #get_dd_habitat <- function(DistMinusStartEndLen,ToEdgeHabMaxAccessible,dMaxDist,sDDFunction){
    get_dd_habitat <- function(DistMinusStartEndLen,ToEdgeHabMaxAccessible,dMaxDist,sDDFunction, FromEdgeLen){
        
        a = round(DistMinusStartEndLen/dMaxDist,3)
        b = round((FromEdgeLen+DistMinusStartEndLen+ToEdgeHabMaxAccessible)/dMaxDist,3)
        
        a = ifelse(a<0,0,a)
        a = ifelse(a>1,1,a)
        b = ifelse(b<0,0,b)
        b = ifelse(b>1,1,b)
   
        if(sDDFunction=="linear"){
            toedgehab_dd <- ToEdgeHabMaxAccessible * f_avg_linear(a,b)
        }else if(sDDFunction=="natexp1"){
            toedgehab_dd <- ToEdgeHabMaxAccessible*f_avg_natexp1(a,b)
        }else if(sDDFunction=="natexp2"){
            # to do
        }else if(sDDFunction=="circle"){
            toedgehab_dd <- ToEdgeHabMaxAccessible*f_avg_circle(a,b)
        }else if(sDDFunction=="sigmoid"){
            toedgehab_dd <- ToEdgeHabMaxAccessible*f_avg_sigmoid(a,b)
        }
      
        # if segment << cutoff distance this can result in NaN's which cause issues.
        toedgehab_dd[ is.nan(toedgehab_dd) ] <- 0

        toedgehab_dd
    }

    sum_tab_2020[, toedgehabaccessible_dd := round(get_dd_habitat(DistMinusStartEndLen,
                                                                   ToEdgeHabMaxAccessible,
                                                                   dMaxDist,
                                                                   sDDFunction,
                                                                   FromEdgeLen),2)]
    return(sum_tab_2020)
    
}

# ########################################
# ########## MAIN CODE SECTION ############

# intialize file with error code
write("ERROR",file='out_dd.txt')

# required libraries
library(RBGL) 
library(data.table) 
library(tidyverse)
# RBGL and Rgraphviz must be installed via BioconductR
#install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
#BiocManager::install("RBGL")

# optional libraries
#library(Rgraphviz) # only needed for visuals
#library(rbenchmark) 

########## 1) DATA PREP #########

# 'Advanced' table includes habitat, length, connectivity info in one table
FIPEX_table=read.csv("FIPEX_Advanced_DD_2020.csv")
# ensure it's "sink" not "Sink"
FIPEX_table <- FIPEX_table %>%
mutate(DownstreamEID = ifelse(DownstreamEID == "Sink","sink",as.character(DownstreamEID)))

# for testing only: natural TF set node 84 to natural barrier
#FIPEX_table[2,]$NaturalTF <- TRUE
#FIPEX_table

# The params file allows users to pass param settings
# to R from within the ArcMap software (new in 2020)
FIPEX_params=read.csv("FIPEX_2020_params.csv")

###### 2) PARAMETERIZATION #######

bDCISectional <- as.logical(FIPEX_params$bDCISectional)
bDistanceLim <- as.logical(FIPEX_params$bDistanceLim)
dMaxDist <- as.double(FIPEX_params$dMaxDist)
bDistanceDecay <- as.logical(FIPEX_params$bDistanceDecay)
sDDFunction <- as.character(FIPEX_params$sDDFunction)
# for testing:
#sDDFunction = "linear"
#sDDFunction = "natexp1"
#sDDFunction = "circle"
#sDDFunction = "sigmoid"
#sDDFunction = "none"
#bDistanceLim = FALSE
#dMaxDist = 1000
#bDistanceDecay = FALSE
naturalonly = FALSE
#bDCISectional = TRUE
bDCIp = TRUE

totalhabitat = sum(FIPEX_table$HabQuantity)
totallength = sum(FIPEX_table$DownstreamNeighDistance)

######### 3) NETWORK ANALYSIS #########

# build adjacency matrix
edgeweighted_adj_matrix <- create_advanced_adjmatrix_2020(FIPEX_table)

# build graph object
g_dd <- create_graph_dd_2020(edgeweighted_adj_matrix,FIPEX_table)

# build summary table (analyses to determine paths, pass, etc between edges)
sum_tab_2020 <- get_summary_tab_2020(option="dt-lists",
                                     naturalonly,
                                     g_dd,
                                     bDCIp,
                                     bDistanceLim,
                                     dMaxDist)

if(bDistanceLim==TRUE){
    sum_tab_2020 <- apply_distance_limits(sum_tab_2020, bDistanceLim, dMaxDist, bDistanceDecay, sDDFunction)
}

# for testing: turn all pass = 1 and DCI should = 1
#sum_tab_2020<- sum_tab_2020 %>% mutate(CumulativePass=1)
#sum_tab_2020 = as.data.table(sum_tab_2020)

######## 4) DCI CALC ########
DCId = 0.00
DCIp = 0.00
DCIs = 0.00

DCId <- calc_DCId_2020(sum_tab_2020,totalhabitat,"sink",bDistanceLim)
if(bDCIp==TRUE){
    DCIp <- calc_DCIp_2020(sum_tab_2020,totalhabitat,"unique",bDistanceLim)
	# sectional can only be run if DCIp has been selected
    if(bDCISectional==TRUE){
        DCIs <- calc_DCIs_2020(sum_tab_2020,totalhabitat,"dt",bDistanceLim)
    }
}

######## 5) Write to Files ######

# following previous output format
res<- data.frame(c(DCIp,DCId))
names(res)<-"value"
row.names(res)<-c("DCIp","DCId")
write.table(res,file='out_dd.txt')

# transform data to match what FIPEX expects
# DCIs 'FromEdgeName' 100-101 has first numbers as downstream node
# to align in FIPEX it can be adjusted to e.g., 100_s
# however, this results in more than one segment since, say, node
# 100 can have multiple upstream edges and nodes.
# To address this the _downstream_ segment DCI_s can be reported
# for each node in the system. This change will have to be carefully reported
# to the user!
if(bDCISectional==TRUE & bDCIp==TRUE){
    names(DCIs)[names(DCIs) == 'DCIs_i'] <- 'DCI_as'
    names(DCIs)[names(DCIs) == 'FromEdgeName'] <- 'section'
    
    #DCIs$sections <- paste(sub("\\-.*", "", DCIs$section),"_s",sep="")
    # there was a problem encountered with the above because it resulted
    # in more than one upstream segment associated with each node. I 
    # reversed this but now need to be careful that the segmental DCI
    # is now reported as associated with the immediate _downstream_
    # segement from each node!
    DCIs$sections <- paste(sub(".*\\-", "", DCIs$section),"_s",sep="")

    DCIs$sections[DCIs$sections == "sink_s"] <- "sink"
    DCIs <- DCIs %>% select(sections,DCI_as)
    res<-data.frame(DCIs)
    write.table(x=res,
                file="DCI_all_sections_dd.csv",
                sep=",",
                row.names=F)
}




