##############################################################################
# FIPEX_output_to_R_input.r


##############################################################################
##############################################################################
    # Created by: G Oldford
    # Last modified: August, 2020
    # Inputs: 
    #   FIPEX_connectivity.csv
    #   FIPEX_BarrierHabitatLine.csv
    # Outputs: 
    #   segment_matrix.csv
    #   barrier.csv
    # Description:
    # 1) Creates segment_matrix.csv that has columns: "Seg_ID","Seg"
    #   which is connectivity table
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
        # rbind essentially appends row
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
