##############################################################################
# get_adj_matrix_from_gis

    # Edited by: G Oldford
    # purpose: produce an adjancy matrix
    # Last modified: August, 2020
    # Input: segment_matrix.csv with columns: "Seg_ID","Seg"
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
    #  (note, aug 2020: the CSV is not being used - see below)
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