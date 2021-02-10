##############################################################################
# dci_calc_fx.r

# Edited by: G Oldford
    # Last modified: August, 2020
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
    p_nrows<-dim(sum_table)[1]
    
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
    ## if desired, one can calculate the DCI_d starting with every sections.  This
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