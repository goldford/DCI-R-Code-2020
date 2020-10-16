##############################################################################
# dci_calc.r

# Edited by: G Oldford
    # Last modified: August, 2020
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
