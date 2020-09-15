#Debugging done here
#Code changes to functions below must also be made in the .r files
#modifications to uncomment calls to source() must be made to .r files
# G Oldford, 2020

#install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
#BiocManager::install("RBGL")

# new in 2020:
# (others may be used in experiemnts below but these are required as of 2020)
#install.packages("rbenchmark", repos='http://cran.us.r-project.org')
#install.packages("data.table", repos='http://cran.us.r-project.org')
#install.packages("tidyverse", repos='http://cran.us.r-project.org')

# Note 2020: 
# Some original files were modified to upgrade to newer version of R (3.6.1)
# variable names and functions with . were replaced with _ to avoid confusion 
# the visualization function 

# don't use source() in notebook here because instead of debugging using functions
# defined here it will look to the functions in the R files

# to test that it works via command line go to the R model folder and run
# "C:/Program Files/R/R-3.6.1/bin/r.exe" CMD BATCH FIPEX_run_DCI.r

##############################################################################
# 
# FIPEX_run_DCI.r
#source("FIPEX_output_to_R_input.r")
FIPEX_output_to_R_input()
#source("dci_fxs.r")
x = try(dci_fxs(),silent=FALSE)

if(class(x)=='data.frame' | class(x)=='list'){
  write.table(x,file='out.txt')
} else{
  write("ERROR",file='out.txt')
}

##############################################################################
# FIPEX_run_DCI_Sectional.r
#source("FIPEX_output_to_R_input.r")
FIPEX_output_to_R_input()
#source("dci_fxs.r")
x = try(dci_fxs(all_sections=T),silent=FALSE)
if(class(x)=='data.frame' | class(x)=='list'){
  write.table(x,file='out.txt')
} else{
  write("ERROR",file='out.txt')
}

##############################################################################
# FIPEX_output_to_R_input.r


##############################################################################
##############################################################################
    # Last modified August, 2020 by G Oldford
    # Inputs: 
    #   FIPEX_connectivity.csv
    #   FIPEX_BarrierHabitatLine.csv
    # Outputs: 
    #   segment_matrix.csv
    #   barrier.csv
    # Description:
    # 1) Creates segment_matrix.csv with columns: "Seg_ID","Seg"
    #   (connectivity table, "seg" = downstream segment)
    # 2) Creates barrier.csv that has columns: "Pass", "Bar_ID", "Seg_ID", 
    #  "nat_barrier"
    # 3) Creates length.csv with columns: "Seg_ID","Shape_Length"
    # 

FIPEX_output_to_R_input<-function(){
    ######################################################
    #### 1. PREPARE / CONVERT CONNECTIVITY TABLE ####
    connectivity_data<-read.csv("FIPEX_connectivity.csv")

    barrier<- as.vector(connectivity_data$BarrierOrFlagID)
    down_barrier<- as.vector(connectivity_data$Downstream_Barrier)

    # turn the barrier names into segment names by adding _s
    segment<-paste(as.vector(connectivity_data$BarrierOrFlagID),"_s",sep="")
    down_segment<-paste(as.vector(connectivity_data$Downstream_Barrier),"_s",sep="")

    # maintian the name of the closest segment to the start point of the riverscape as "sink"
    down_segment[down_segment=="Sink_s"]<-"sink"

    res<-NULL

    for(i in 1:length(segment)){
        # get the segment downstream	
        down<-down_segment[segment==segment[i]]
        
        # look for any matching segments in the downstream dataset
        additional_connected_segments<-segment[!is.na(match(down_segment,segment[i]))]
        connected_segments<-c(segment[i],down,additional_connected_segments)
        
        num_segments<-length(connected_segments)
        # rep - replicates values, second arg = x times
        newdata<-data.frame(rep(segment[i],num_segments),connected_segments)
        
        # make the names match what the R DCI functions expect
        names(newdata)<-c("Seg_ID","Seg")
        # rbind essentially appends rows
        res<-rbind(res,newdata)
        }

    # add in the origin, which is connected to itself and other segments next to it. 
    section_name<-"sink"
    additional_connected_segments<-segment[!is.na(match(down_segment,section_name))]
    connected_segments<-c(section_name,additional_connected_segments)
    num_segments<-length(connected_segments)
    newdata<-data.frame(rep(section_name,num_segments),connected_segments)
    names(newdata)<-c("Seg_ID","Seg")
    
    res<-rbind(res,newdata)

    write.table(x=res,
                file="segment_matrix.csv",
                row.names=F,
                sep=",")
    
    ######################################################
    #### 2. PREPARE / CONVERT BARRIER ATTRIBUTE TABLE ####
    # create the file barrier.csv that has columns: Pass	Bar_ID	Seg_ID	nat_barrier
    # this requires knowing the upstream and downstream segments of each barrier. 

    # read in the barrier passabilities
    barrier_info<-read.csv("FIPEX_BarrierHabitatLine.csv")
    #barrier.info$BarrierID[barrier.info$BarrierID=="Sink"]<-"1"

    # change the col names  
    barrier_info$barrier<-barrier_info$BarrierID
    barrier_info$pass<-barrier_info$BarrierPerm

    res2<-NULL

    for(i in 1:length(barrier))
        {
        upstream_segment<-paste(barrier[i],"_s",sep="")
        if(down_barrier[i]=="Sink") 
            downstream_segment <- "sink"
        else 
            downstream_segment<-paste(down_barrier[i],"_s",sep="")
    
        pass<-barrier_info$BarrierPerm[barrier_info$BarrierID==barrier[i]]
        nat_barrier<-barrier_info$NaturalYN[barrier_info$BarrierID==barrier[i]]
        #nat_barrier<-F
        
        newdata<-data.frame(rep(pass,2),
                            rep(barrier[i],2),
                            c(upstream_segment,downstream_segment),
                            rep(nat_barrier,2))
        
        names(newdata)<-c("Pass",
                          "Bar_ID",
                          "Seg_ID",
                          "nat_barrier")
        res2<-rbind(res2,newdata)
        }

    write.table(file="barrier.csv",
                res2,
                row.names=F,
                sep=",") 
    
    ######################################################
    #### 3. PREPARE / CONVERT SEGMENT LENGTH TABLE ####

    data<-read.csv("FIPEX_BarrierHabitatLine.csv")
    # the column names in the input file assumed to be: 
    # ObID, BarrierID, HabClass, Shape_Length, BarrierPerm, NaturalYN
    # the column names in the destination file are: Seg_ID, Shape_Length

    # changed by Greig (Oct 7,2010) - lengths<-data$Shape_Length
    lengths<-data$Quantity
    segment<-paste(as.vector(data$BarrierID),"_s",sep="")
    
    # maintain the name of the closest segment to the start point of the riverscape as "sink"
    segment[segment=="Sink_s"]<-"sink"
    newdat<-data.frame(segment,lengths)
    names(newdat)<-c("Seg_ID","Shape_Length")
    write.table(file="length.csv",
                newdat,
                row.names=F,
                sep=",")

    #print("Barrier.csv")
    #print(res2)
    #print("Segment_matrix.csv")
    #print(res)
    #print("length.csv")
    #print(newdat)
}


##############################################################################
# DCI_fxs.r

dci_fxs<-function(all_sections=F, dist_decay=F){

    #source in the 7 functions in R
    #see each function for an explanation of inputs and outputs

    #source("convert_gis_output_to_r_format.r")
    #source("get_adj_matrix_from_gis.r")
    #source("graph_fx.r")
    #source("sum_fx.r")
    #source("graph_and_data_setup_for_DCI.r")
    #source("dci_calc_fx.r")
    #source("dci_calc.r")

    convert_gis_output_to_r_format()
    adj_matrix<-get_adj_matrix_from_gis()
    
    #print(adj_matrix)
    #print(as.matrix(adj_matrix))

    #OLD note:
    #have to assign it an object name as this function gets called in 
    #in "graph.and.data.setup.for.DCI.r" and "sum.fx.r"
    
    # Note GLO - graph_fx actually returns an object
    graph_fx(plot_it=F, adj_matrix=adj_matrix)
    passability<-read.csv("segments_and_barriers.csv")

    NB<-graph_and_data_setup_for_DCI(passability=passability, adj_matrix=adj_matrix)
   
    dci_calc(NB,all_sections=all_sections)
}

##############################################################################
# convert_gis_output_to_r_format


##############################################################################
##############################################################################
    # Edited by: G Oldford
    # Last modified: August, 2020 (minor changes, no changes to logic)
    # Input: barrier.csv with columns: "Pass", "Bar_ID", "Seg_ID", 
    #  "nat_barrier"
    #
    # Input example: 
    # "Pass","Bar_ID","Seg_ID","nat_barrier"
    # 0.5,55152,"55152_s",FALSE

    # Output: segments_and_barriers.csv with colu,ns: "passability", 
    #   "barrier id", "segment 1 (start)", "segment 2 (end)", "nat_barrier",
    #   "section 1.2" (binding segment 1 and segment 2 - for use in another function)
    #
    # Output example: 
    # "Bar_ID","Seg_1","Seg_2","Pass","nat_barrier","section1_2"
    # 55152,"55152_s","sink",0.5,FALSE,"55152_s,sink"

    # Notes from previous coding work: 
    #  barrier.csv contains barrier ID, the passability for each barrier and 
    # the segments that border the barrier
   
convert_gis_output_to_r_format<-function(){
    
    barrier=read.csv("barrier.csv")
    
    # if the barrier.csv file actually has no barriers in it (i.e. barrier$Pass 
    # has 1's down the entire column),
    # then we want to stop the algorithm and let the user know that their riverscape 
    # is one without natural or artificial barriers. 
    if (length(barrier$Pass)== sum(barrier$Pass)) 
        {
        stop("
        *** There are no artificial or natural barriers in the riverscape. ***
        *** All passability values in your barrier.csv file are 1 ***
        *** Analysis will not proceed past this point. ***")
    }else{

        unique_barriers<-with(barrier, unique(Bar_ID))
        #extract unique barrier IDs to later match them up with Bar_ID
        #and get the 2 sections/segments that neighbour the barrier

        segments_and_barriers<-NULL

        for (i in 1:length(unique_barriers)){
            #find in which *position* there is a match between barrier$Bar_ID and unique barriers
            index<-match(barrier$Bar_ID,unique_barriers[i])
        
            section_pair<-barrier$Seg_ID[!is.na(index)]
            
            #obtain the passability value by matching the barrier ID
            passability<-barrier$Pass[match(unique_barriers[i],barrier$Bar_ID)]

            #determine whether it is a natural or artificial barrier
            barrier_type<-barrier$nat_barrier[match(unique_barriers[i],barrier$Bar_ID)]

            # get barrier id (old note: you can't concatenate a factor and numbers 
            # (i.e. section name (a letter) with barrier ID and passability (numbers)), 
            #so you have to turn it into a list)
            barrier_id<-list(unique_barriers[i])
            sections_and_barriers<-data.frame(barrier_id,
                                              section_pair[1],
                                              section_pair[2],
                                              passability,
                                              barrier_type)
            names(sections_and_barriers)<-c("Bar_ID","Seg_1","Seg_2","Pass","nat_barrier")
        
            # Old Note
            #use these column headings as these are the headings that Christina uses in 
            # her ArcGIS output files
            segments_and_barriers<-rbind(segments_and_barriers, sections_and_barriers)
        }

        # Old Note
        #we want to get information for both directions between segments e.g. 1 to 2 as well as the 2 to 1
        #create a column where we paste the start and end segments (separated by a comma)
        rev_segments_and_barrier<-segments_and_barriers
        
        # Below does not work in newer versions of R - GO, 2020
        #names(rev_segments_and_barriers)<-c("Bar_ID","Seg_2","Seg_1","Pass","nat_barrier")
        #rev_segments_and_barriers<-rev_segments_and_barriers[,c(1,3,2,4,5)]
        # Recoded temporarily (should be done using dplyr)
        # ***********************************************************
        names(rev_segments_and_barrier)[names(rev_segments_and_barrier) == "Seg_1"] <- "Seg_2a"
        names(rev_segments_and_barrier)[names(rev_segments_and_barrier) == "Seg_2"] <- "Seg_1a"

        names(rev_segments_and_barrier)[names(rev_segments_and_barrier) == "Seg_2a"] <- "Seg_2"
        names(rev_segments_and_barrier)[names(rev_segments_and_barrier) == "Seg_1a"] <- "Seg_1"
        
        segments_and_barriers<-rbind(segments_and_barriers,rev_segments_and_barrier)
        segments_and_barriers$section1_2<-with(segments_and_barriers,paste(Seg_1,Seg_2, sep=","))

        write.table(segments_and_barriers, 
                    "segments_and_barriers.csv", 
                    row.names=F, 
                    sep=",")
    } 
}

##############################################################################
# get_adj_matrix_from_gis

    # purpose: produce an adjancy matrix
    # Last modified: August, 2020 by G Oldford
    # Input: segment_matrix.csv (connectivity table) with columns: "Seg_ID","Seg"
    #         (seg and seg_id are segments, seg is downstream of seg_id, segment 
    #          names are based on downstream neighbour node)
    # Output: adjacency_matrix.csv - binary matrix with 1's and 0's indicating 
    #         segment neighbours
    #
    # Input example: 
    # "Seg_ID","Seg"
    # "55152_s","55152_s"
    # "55152_s","sink"

    #
    # Output example: 
    #  matrix object of 
    #  0's and 1's indicating neighbours (directed?)
    #  (note, aug 2020: the CSV was not being used - see below)
    #
    # Notes from previous coding work: 
    #The input is the segment matrix, which is really two vectors. For each 
    #segment in the first column (Seg_ID), the second column (Seg) gives the id 
    #of other sections it touches, including itself.

get_adj_matrix_from_gis<-function(){

    segment_matrix=read.csv("segment_matrix.csv")
    # obtain the segments in whatever order they are in the file
    segments<-with(segment_matrix, unique(Seg_ID))

    #create a matrix with only 0's in them
    adj_matrix<-matrix(nrow=length(segments), 
                       ncol=length(segments), 
                       rep(0,length(segments)*length(segments)))
    segment_length<-length(segments)
    print(adj_matrix)
    rownames(adj_matrix)<-colnames(adj_matrix)<-segments

    for (i in 1:segment_length){
        # find the segments in segment.matrix$Seg where segment.matrix$Seg_ID matches segments[i]
        # index of matching positions - this will be a vector of 1's and NA's
        pos_match<-	match(segment_matrix$Seg_ID,segments[i])
        
        # keep only the positions where pos.match==1
        adj_segments<-segment_matrix$Seg[!is.na(pos_match)]

        # find the column positions that correspond to the adjacaent segments
        col<-match(adj_segments,segments)

        # the row number should correspond to i
        row<-i

        # assign a value of 1 for all values of row and col
        adj_matrix[row,col]<-1
    }

    write.table(adj_matrix,
                "adjacency_matrix.csv",
                row.names=F, sep=",")

    # Old Note
    #write.table seems to give problems when reading in "adjacency matrix.csv" - 
    # it doesn't recognize that it's a matrix so it creates column rows with headings "X1,X2,X3,..."
    # instead of "1,2,3,..."
    #in order to avoid the problem above return the object:
    
    
    return(adj_matrix)

}

##############################################################################
# graph_fx

# Edited by: G Oldford
    # Last modified: August, 2020
    # Inputs: 
    #   adjacency_matrix (from object or file?)
    #   segments_and_barriers.csv
    #   length.csv
    # Output:
    #   directed graph object for further analysis
    #
    #
    # Notes:
    #  this mainly does visuals
    #  rbgl and rgraphviz are both part of the bioconductor r package
    #  rbgl is a connector to the BOOST graph visualization library
    # the new("graphAM") is calling the graph library (also part of bioconductor)
    #   note AM = adjacency matrix
    # I believe adding the rbgl library also adds the graph library
    #
    # to-do: the function is reading an adjacency matrix from CSV
    # but in the master function it passes an adj_matrix object
    # with note that this is to avoid some issue of writing to CSV with 
    # headings. Should fix / sort this out. -GO, 2020
    
    #
    # Notes from previous coding work: 
    #  Problem with adjusting node.size.  If edge.size is changed, the font 
    #  size on the graph changes, if the node.size is changed, nothing is 
    #  changed on the graph.  
    #  Note: the larger the edge.size number, the smaller the font.


graph_fx<-function(edge_size=75,
                   node_size=5,
                   #adj_matrix=read.csv("adjacency_matrix.csv"),
                   adj_matrix=adj_matrix,
                   plot_it=F){
    
    # debug (comment below out in functions)
    #adj_matrix=read.csv("adjacency_matrix.csv")
    #edge_size=75
    #node_size=5
    #plot_it=F
    
    # to do: should not be embedded in function
    library(Rgraphviz)
    library(RBGL)
    
    #print(adj_matrix)
    #print("check")
    #rownames(adj_matrix)<-1:length(adj_matrix)
    #colnames(adj_matrix)<-1:length(adj_matrix)
    #print("check")

    #need to convert the dataframe into a matrix
    adj_matrix<-as.matrix(adj_matrix)
    
    passability<-read.csv("segments_and_barriers.csv")
    #this is the output from the "convert.gis.output.to.r.format.r" Crimson Editor file
    #data contained in this file: "Bar_ID","Seg_1","Seg_2","Pass","nat_barrier", "section1_2" (pasting Seg_1 and Seg_2 together)

    lengths<-read.csv("length.csv")
    #contains the total length of each segment (i.e. segment id and segment length)
    sections<-rownames(adj_matrix)

    #to create a graph without self-pointing edges going from 1 to 1, or 2 to 2, or etc... 
    #need to put zeros along the diagonal
    adj_matrix_zeros_on_diag<-adj_matrix

    # note diag is a core R function - GO
    diag(adj_matrix_zeros_on_diag)<-0


    if(plot_it==T){
        #### PLOT VISUAL GRAPH ####

        g1<-new("graphAM",
                adjMat=adj_matrix_zeros_on_diag, 
                edgemode="directed")
        #for the graph, note that the adj.matrix.zeros.on.diag must have 0s across the diagonal or else it will give you more edges, because it would include a-a, b-b, c-c, etc...

        #we want to label edges of the graph with the Barrier letter and the passability value
        #need to create a new column in passability that gives you: Bar_ID (Passability)
        #round the passability values to 2 decimal places (so it fits better on the page)
        pass_barrier<-passability
        pass_barrier$bar_pass<-with(pass_barrier,paste(Bar_ID," (",round(Pass,2),")",sep=""))

        #deal with EDGES
        eAttrs<-list()

        #need to assign correct labeling in Graph
        #create new labels in pass.barrier that match graph object labels 
        # (edgeNames(g1)/names(eAttrs$labels))
        pass_barrier$names.eAttrs<-with(pass_barrier, paste(Seg_1, Seg_2, sep="~"))

        #determine the right order:
        #the "match" must be done in this order so that we know the position of where the 
        # 1st element of edgeNames(g1) matches with pass.barrier$names.eAttrs
        ord<-match(edgeNames(g1),pass_barrier$names.eAttrs)
        
        # order the dataframe appropriately wrt the ord and pass.barrier dataframes
        pass_barrier<-data.frame(pass_barrier)[ord,]

        #assign the text we want to appear with each edge
        ew<-pass_barrier$bar_pass
        
        # get the labels of the edges
        names(ew)<-edgeNames(g1)
        
        # assign graph object labels so it it knows where to put the text
        eAttrs$label<-ew

        #deal with NODES
        nAttrs<-list()

        #get segment names
        n<-nAttrs$label
        n<-row.names(adj_matrix)
        names(n)<-nodes(g1)

        # fontsize for edges 
        a<-rep(edge_size,length(ew))
        names(a)<-edgeNames(g1)
        eAttrs$fontsize<-a

        # fontsize for nodes 
        b<-rep(node_size, length(nodes(g1)))
        names(b)<-nodes(g1)
        nAttrs$fontsize<-b

        #overall it looks like font size of the nodes and edges don't work independently from eachother

        #node shape
        node_shape<-rep("ellipse",length(nodes(g1)))
        names(node_shape)<-nodes(g1)
        nAttrs$shape<-node_shape

        #node height
        node_height<-rep(4,length(nodes(g1)))
        names(node_height)<-nodes(g1)
        nAttrs$height<-node_height

        #node width
        node_width<-rep(1.1,length(nodes(g1)))
        names(node_width)<-nodes(g1)
        nAttrs$width<-node_width
        
        #edge color
        edge_color<-rep("grey",length(ew))
        names(edge_color)<-edgeNames(g1)
        eAttrs$color<-edge_color

        plot(g1,edgeAttrs=eAttrs, nodeAttrs=nAttrs, main="_______ Watershed")
        #gives a graph with a 2-way arrow
    }

    #print(adj_matrix)
    g2<-new("graphAM",
            adjMat=adj_matrix, 
            edgemode="directed")
    #use g2 for the graph.and.data.setup.for DCI function - you need 1's along the diagonal

    return(c(sections,g2))

}

##############################################################################
# graph_and_data_setup_for_DCI.r

# Edited by: G Oldford
    # Last modified: August, 2020
    # Inputs: 
    #  adj_matrix, passability, lengths
    # Output:
    #  "summary_table_natural.csv" with passability values of each pathway 
    #   if there were no artificial barriers (the natural passability of the 
    #   riverscape)
    # "summary_table_all.csv" with passability values 
    #   of the pathways in the given riverscape (artificial and natural barriers 
    #   included).  This way we can have an idea of how much the artificial barriers
    #   are really affecting the DCI of the riverscape.
    #
    # Output example:
    #  "start","end","path2","barrier_id","pathway_pass","start_section_length","finish_section_length"
    #  "55152_s","55152_s","55152_s","NA",1,120.1,120.1
    #  "55152_s","62689_s","55152_s,sink,62689_s","55152,62689",0.25,120.1,452.2
    #
    # Notes:
    #  this function calls sum_fx for natural only and natural / anthro barriers
    #
    # to-do: 
    
    #
    # Notes from previous coding work: 
    #dci.fxs.r assigns adj.matrix and passability before it runs the graph.and.data.setup.for.DCI.r	
    #output: get the summary tables of all possible pathways with their passability values, 
    # start and end sections, barrier id, length of start and end segments
    #calls on "sum.fx.r" function

graph_and_data_setup_for_DCI<-function(adj_matrix,
                                       passability,
                                       lengths){
    # checks if any barriers are flagged as natural - GO, 2020
    NB<-sum(passability$nat_barrier==TRUE)>0
    
    #NB = natural barriers
    if (NB==TRUE){
        # Old note:
        # if there are natural barriers in the system, we want to know what the 
        # overall DCInp and DCInd (n= natural) is for the system so we can compare 
        # it to the DCIap and DCIad (a= all (anthropogenic + natural barriers))
        natural_passability<-passability
        # Old note:
        # only want to take into consideration passability values for natural barriers.  
        # so we want to change the passability values for anthropogenic barriers to 1
        natural_passability$Pass[natural_passability$nat_barrier==FALSE]<-1

        sum_table_n<-sum_fx(adj_matrix=adj_matrix, 
                            passability=natural_passability,
                            lengths=lengths)
        write.table(sum_table_n,
                    "summary_table_natural.csv",
                    row.names=F,
                    sep=",")
    }
    # Old note:
    #if you put the else statement here, then it will only do the below commands 
    # only if the above statement is false.  if you take the else statement out, 
    # it will do the above commands only if it's true, but it will ALWAYS do the 
    # commands below.

    sum_table<-sum_fx(adj_matrix=adj_matrix, 
                      passability=passability,
                      lengths=lengths)

    write.table(sum_table,
                "summary_table_all.csv",
                row.names=F,
                sep=",")

    return(NB)
}

##############################################################################
# sum_fx.r

# Edited by: G Oldford
    # Last modified: August, 2020
    # purpose: calculate paths between segments and produce sum_tab
    # Inputs: 
    #   adj_matrix
    #   passability
    #   lengths (opening the csv and replacing param)
    # Output:
    #   sum_table
    #
    # Output example: 
    #  "start","end","path2","barrier_id","pathway_pass","start_section_length","finish_section_length"
    #  "55152_s","55152_s","55152_s","NA",1,120.1,120.1
    #  "55152_s","62689_s","55152_s,sink,62689_s","55152,62689",0.25,120.1,452.2
    #
    #
    # Notes 2020:
    #  this is the only function other than the visualization function graph_fx() 
    # that uses the graphing library (rbgl)
    #
    # to-do: 
    
    #
    # Notes from previous coding work: 
    #function(adj.matrix, passability) calls on "adj.matrix" and "passability" that were 
    #created in previous functions.  When you call a function, it creates a bunch of 
    # objects, but these objects disappear when you open a new function.  In order to 
    # be able to use these objects, you can either "assign(object.name)", or you can do 
    # what was done above function(object.names).  Because all of these functions are 
    # called in "dci.fxs.r" and "dci.fxs.theo.restoration.r", the information is passed 
    # on from one function to another.
    # WHAT THIS FUNCTION DOES: it creates all the data for a summary table (start and end 
    # segments, pathway, barriers it goes through, passability, length of start and end
    # segments)
    # BUT this dataframe is NOT outputted into Excel in this function.  It's returned and 
    # then called in by graph.and.data.setup.for.DCI.r.

sum_fx<- function(adj_matrix,
                  passability,
                  lengths) {



    # why is 'lengths' used as a function param if it's declared again here -GO, 2020
    lengths<-read.csv("length.csv")

    x2<-NULL #section start
    y2<-NULL #section end
    start_section_length<-NULL
    finish_section_length<-NULL
    path2<-NULL
    pathway_pass<-NULL
    barrier_id<-NULL

    # obtain the section names from the adj matrix
    sections<-rownames(adj_matrix)
    
    # code useful later?
    # added by GO to fix issue with adj_matrix
    #rownames(adj_matrix)<-1:length(adj_matrix)
    #colnames(adj_matrix)<-1:length(adj_matrix)
    #convert the dataframe into a matrix
    #adj_matrix<-as.matrix(adj_matrix)
    
    # Old note:
    # there is a problem with only one barrier, since sp.between requires a list, 
    # and we provide a vector.
    # A quick work-aroud is to manually calcualte the DCI with only one barrier 
    # and skip the more complicated steps below
    if(length(sections)>2){
        
        # this object is re-created here when it was created in previous function
        # - GO, 2020
        g2<-new("graphAM",
                adjMat=adj_matrix, 
                edgemode="directed")
        
        # Old note:
        # get a list of adjacent sections (i.e. find all possible pathways that 
        # exist in the riverscape)
        for (i in 1:length(sections)){
            
            # Old note:
            #need it to look through the matrix, "i" cycles down the columns and 
            #"j" cycles across the rows
            for (j in 1:length(sections)){

                x<-sections[i]
                y<-sections[j]
                
                # sp.between is a RBGL function for shortest path (Dijkstra's) -GO
                #sp.between = shortest path between 2 nodes
                path_all<-sp.between(g=g2,start=x,finish=y)
                
                #extract every possible pair that exists in the matrix
                #pulls out the path information for each pair of sections
                #e.g. [1] "1" "2" "3" - to go from 1 to 3 you must go through 2
                path<-path_all[[1]]$path_detail
                
                path_length<-length(path)
                # we need to get the length so that the k-loop (below) can pull out 
                # the appropriate barrier information 

                #we have all of the possible combinations that exist between 
                # sections, we now need to find the barriers that exist bewteen these sections

                x2<-c(x2,x)
                y2<-c(y2,y)
                
                #grab the 1st segment of the path
                start_section<-path[1]
                                
                #grab the last segment of the path
                finish_section<-path[path_length]

                #get the length of the start and finish segments of the pathway
                new_start_length<-lengths$Shape_Length[match(start_section, lengths$Seg_ID)]
                new_finish_length<-lengths$Shape_Length[match(finish_section, lengths$Seg_ID)]

                start_section_length<-c(start_section_length, new_start_length)
                finish_section_length<-c(finish_section_length, new_finish_length)

                section1_2<-NULL
                section_1<-NULL
                section_2<-NULL

                if (path_length<2){
                    section1_2<-NA
                }else{
                    for(k in 1:(path_length-1)){
                        #use "-1" because we are looking at the number of barriers between 
                        # each section.  number of barriers is = to number of sections - 1.
                        section1<-path[k]
                        section2<-path[k+1]
                        #k+1 is used so that R knows to go to the 2nd element in path

                        section_1<-c(section_1, section1)
                        section_2<-c(section_2, section2)

                        section_paste<-paste(section1,section2,sep=",")
                        section1_2<-c(section1_2, section_paste)
                        #need to create this new column, section1_2, in order to match it to 
                        #passability$section1_2 - see function "graph.fx.r ### INPUT DATA ###"
                    }
                }
    
                #find the barriers that are between each pair of sections
                all_barriers<-passability$Bar_ID[match(section1_2,passability$section1_2)]
                
                #give a list of all of the passability values from start to end of the path
                new_barrier_pass2<-passability$Pass[match(all_barriers,passability$Bar_ID)]

                #need to get the product for each new.barrier.pass$all.passabilities
                #add a new column to the new.barrier.pass dataframe
                new_barrier_pass<-prod(new_barrier_pass2)
                pathway_pass<-c(pathway_pass,new_barrier_pass)

                #now we need to turn these objects into 1 vector in order to use cbind to make 
                # these objects into 1 dataframe
                new_barrier_id<-paste(all_barriers,sep="",collapse=",")
                barrier_id<-c(barrier_id, new_barrier_id)

                path1<-paste(path,collapse=",")
                #we want to get "a" "b" "c" to be 1 character (i.e. "a,b,c")
                #this allows it to be recognized as 1 element in the dataframe
                path2<-c(path2,path1)
            }

            sum_table<-cbind(x2, 
                             y2, 
                             path2, 
                             barrier_id, 
                             pathway_pass, 
                             start_section_length, 
                             finish_section_length)
            sum_table<-as.data.frame(sum_table)
            sum_table$pathway_pass<-as.numeric(as.character(sum_table$pathway_pass))
            sum_table$start_section_length<-as.numeric(as.character(sum_table$start_section_length))
            sum_table$finish_section_length<-as.numeric(as.character(sum_table$finish_section_length))
            sum_table$pathway_pass[is.na(sum_table$pathway)]<-1
            names(sum_table)[1:2]<-c("start","end")	
        }
    # close if loop to check to see if ther are more than one barriers        
    }else{

        sum_table<-data.frame
        # old note:
        # "sink" has to appear twice in the start column, 
        # and the second section has to appear twice in the end
 
        start<-c(sections[1],
                 sections[1],
                 sections[2],
                 sections[2])
        end<-c(sections[1],
               sections[2],
               sections[1],
               sections[2])
        path2<-c(sections[1],
                 paste(sections,collapse=","),
                 paste(rev(sections),collapse=","),
                 sections[2])
        barrier_id<-c("NA",
                      passability$Bar_ID[1],
                      passability$Bar_ID[1],
                      "NA")
        pathway_pass<-c(1,
                        passability$Pass[1],
                        passability$Pass[1],
                        1)
        start_section_length<-lengths$Shape_Length[match(start, lengths$Seg_ID)]
        finish_section_length<-lengths$Shape_Length[match(end, lengths$Seg_ID)]
        sum_table<-data.frame(start,
                              end,
                              path2,
                              barrier_id,
                              pathway_pass,
                              start_section_length,
                              finish_section_length)
    }

#print("sum table")
#print(sum_table)
    return(sum_table)
}




##############################################################################
# dci_calc.r

# 
    # Last modified:
    # Inputs: 
    #   summary_table_all.csv
    #   length.csv
    #   summary_table_natural.csv
    #   NB (true/false)
    #
    # Output:
    #   DCIn.csv
    #   DCIa.csv
    #   prop_of_DCI_n.csv
    #
    # Output example: 
    #
    #
    # Notes:
    #  
    #
    # to-do: 
    
    #
    # Notes from previous coding work: 

dci_calc<-function(NB,
                   lengths=read.csv("length.csv"),
                   sum_table_all=read.csv("summary_table_all.csv"),
                   all_sections=F){
    #calls upon the "dci.calc.fx.r function"
    #NB=F
    #lengths=read.csv("length.csv")
    #sum_table_all=read.csv("summary_table_all.csv")
    #all_sections=F
    
    
    if (NB==T){
        sum_table_nat<-read.csv("summary_table_natural.csv")
        # Old note:
        #this dataframe was created in the "graph.and.data.setup.for.DCI.r" 
        # Crimson Editor file dataframe includes start and end segments, 
        # pathway, barriers in pathway, passability for the pathway, and the 
        # length of the start and end segments
        DCI_n<-dci_calc_fx(sum_table=sum_table_nat,
                           lengths=lengths)
        write.table(DCI_n,
                    "DCIn.csv", 
                    row.names=F, 
                    sep=",")
    }

    # old note:
    #the summary table all.csv dataframe was created in the 
    # "graph.and.data.setup.for.DCI.r" Crimson Editor file
    #it includes start and end segments, pathway, barriers 
    # in pathway, passability for the pathway, and the length 
    # of the start and end segments

    DCI_a<-dci_calc_fx(sum_table=sum_table_all,
                       lengths=lengths,
                       all_sections=all_sections)
    write.table(DCI_a,"DCIa.csv", 
                row.names=F, 
                sep=",")

    if (NB==T){
    #returns the results (but you can't do anything after this, so "return" 
    # must always be at the end of a function)
        prop_of_DCI_n<-round(DCI_a/DCI_n,3)
        write.table(prop_of_DCI_n,
                    "prop_of_DCI_n.csv",
                    row.names=F, 
                    sep=",")

        res<-data.frame(unlist(c(DCI_a,
                                 DCI_n,
                                 prop_of_DCI_n)))
        row.names(res)<-c("DCI_P (Total)",
                          "DCI_D (Total)",
                          "DCI_P (nat. barriers only)",
                          "DCI_D (nat. barriers only)",
                          "DCI_P (prop.of natural)",
                          "DCI_D (prop.of natural)")
        names(res)<-"value"
        return(res)
    }else{
        res<- data.frame(t(DCI_a))
        names(res)<-"value"
        return(res)
    }

}


##############################################################################
# dci_calc_fx.r

# 
    # Last modified: 
    # Inputs: 
    #   sum_table  
    #   lengths
    #   all_sections (t/f)
    #
    # Output:
    #   DCI
    #
    # Output example: 
    #   DCIp     DCId
    #   30.03119 44.29823
    #
    # Notes:
    #  
    #
    # to-do: 
    
    #
    # Notes from previous coding work: 

dci_calc_fx<-function(sum_table,
                      lengths,
                      all_sections=F){

    # Old Notes
    #sum.table is a variable that is used in the dci.calc.r function, 
    #where we describe sum.table as sum.table.all or sum.table.nat
    #where: sum.table.nat<-read.csv("summary table natural.csv") and 
    #sum.table.all<-read.csv("summary table all.csv") - from the 
    #graph.and.data.setup.for.DCI.r function

    #WHAT THIS FUNCTION DOES: it calculates the DCIp and DCId 
    #values for the riverscape (includes both natural and 
    #artificial barriers)

    #this contains the length of each section

    #use this number for the DCIp calculation - i
    # interested in movements in all directions from all segments
    #print("check")
    p_nrows<-dim(sum_table)[1]
    #print("check")
    d_nrows<-subset(sum_table, 
                    start=="sink")
    #for diadromous fish we are only interested in the movement 
    # from the segment which is closest to the ocean
    d_sum_table<-d_nrows

    DCIp<-0
    DCId<-0

    #DCIp calculation
    for (k in 1:p_nrows){
        # Old notes: 
        #to get the riverscape connectivity index for potadromous fish, 
        #use the given formula: DCIp= Cij*(li/L)*(lj/L)
        #Cij = passability for pathway (product of all barrier passabilities 
        #in the pathway), li & lj = length of start and finish sections, 
        #L = total length of all sections

        lj<-sum_table$start_section_length[k]/sum(lengths$Shape_Length)
        lk<-sum_table$finish_section_length[k]/sum(lengths$Shape_Length)
        pass<-sum_table$pathway_pass[k]
        DCIp<-DCIp+lj*lk*pass*100
    
        #add DCIp at the beginning to keep a running total of DCIp values
    }

    #DCId calculation
    for (a in 1:dim(d_nrows)[1]){
        # Old notes:
        #to get the DCI for diadromous fish, use the following formula: 
        # DCId= li/L*Cj (where j= the product of the passability in the pathway)
        
        la<-d_sum_table$finish_section_length[a]/sum(lengths$Shape_Length)
        pass_d<-d_sum_table$pathway_pass[a]
        DCId<-DCId+la*pass_d*100
    }

    DCI<-t(c(DCIp,DCId))
    DCI<-as.data.frame(DCI)	

    names(DCI)<-c("DCIp","DCId")
    
    # Old notes
    #########  ALL SECTION ANLAYSIS  ######
    ## if desired, one can calculate the DCI_d starting with every section.  This
    ## gives a "section-level" DCI score for each section in the watershed
    if(all_sections==T){
        sections<-as.vector(unique(sum_table$start))
        # store the all section results in DCI.as
        DCI_as<-NULL
        
        for(s in 1:length(sections)){
            DCI_s<-0
            # Old notes:
            # select out only the data that corresponds to pathways from one sectino 
            # to all other sections
            d_nrows<-subset(sum_table, start==sections[s])
            d_sum_table<-d_nrows
            
            for (a in 1:dim(d_nrows)[1]){
                # Old note:
                #to get the DCI for diadromous fish, use the following formula: 
                # DCId= li/L*Cj (where j= the product of the passability in the pathway)
                la<-d_sum_table$finish_section_length[a]/sum(lengths$Shape_Length)
                pass_d<-d_sum_table$pathway_pass[a]
                DCI_s<-round(DCI_s+la*pass_d*100, digits=2)
            } # end loop over sections for dci calc
        
            DCI_as[s]<-DCI_s
        } # end loop over "first" sections	

        # STORE RESULTS IN .CSV file
        res<-data.frame(sections,DCI_as)
        write.table(x=res,
                file="DCI_all_sections.csv",
                sep=",",
                row.names=F)

    } # end if statement over all.sections

    print(DCI)

    #write.table(DCI,"DCI.csv", row.names=F, sep=",")

    return(DCI)
    # Old note:
    #returns the results (but you can't do anything after this, so "return" 
    # must always be at the end of a function)

}

dci_calc(NB="F")
