##############################################################################
# convert_gis_output_to_r_format


##############################################################################
##############################################################################
    # Edited by: G Oldford
    # Last modified: August, 2020
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