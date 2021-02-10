##############################################################################
# graph_and_data_setup_for_DCI.r

# Edited by: G Oldford
    # Last modified: August, 2020
    # Inputs: 
    #  adj_matrix, passability, lengths
    # Output:
    #  "summary_table_natural.csv" with passability values of each pathway 
    #   if there were NO artificial barriers (the natural passability of the 
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
    #  other than graph.fx
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