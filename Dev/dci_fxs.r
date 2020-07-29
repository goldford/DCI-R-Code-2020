##############################################################################
# DCI_fxs.r

dci_fxs<-function(all_sections=F){

    #source in the 7 functions in R
    #see each function for an explanation of inputs and outputs

    source("convert_gis_output_to_r_format.r")
    source("get_adj_matrix_from_gis.r")
    source("graph_fx.r")
    source("sum_fx.r")
    source("graph_and_data_setup_for_DCI.r")
    source("dci_calc_fx.r")
    source("dci_calc.r")

    convert_gis_output_to_r_format()
    adj_matrix<-get_adj_matrix_from_gis()
    
    #print(adj_matrix)
    #print(as.matrix(adj_matrix))

    #OLD note:
    #have to assign it an object name as this function gets called in 
    #in "graph.and.data.setup.for.DCI.r" and "sum.fx.r"
    
    graph_fx(plot_it=F, adj_matrix=adj_matrix)
    passability<-read.csv("segments_and_barriers.csv")

    NB<-graph_and_data_setup_for_DCI(passability=passability, adj_matrix=adj_matrix)
   
    dci_calc(NB,all_sections=all_sections)
}