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
    # Notes:
    #  this is the only function that uses the graphing library (rbgl)
    #
    # to-do: 
    
    #
    # Notes from previous coding work: 

sum_fx<- function(adj_matrix,
                  passability,
                  lengths) {

# Old Note:
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
                path_all<-sp.between(g=g2,start=x,finish=y)
                
                #extract every possible pair that exists in the matrix
                #sp.between = shortest path between 2 pairs
                path<-path_all[[1]]$path_detail
                
                #this pulls out the path information for each pair of sections
                #e.g. [1] "1" "2" "3" - to go from 1 to 3 you must go through 2
                
                path_length<-length(path)
                # we need to get the length so that the k-loop (below) can pull out 
                # the appropriate barrier information 

                #we have all of the possible combinations that exist between 
                # sections, we now need to find the barriers that exist bewteen these sections

                x2<-c(x2,x)
                y2<-c(y2,y)

                start_section<-path[1]
                #this should grab the 1st segment of the path
                
                finish_section<-path[path_length]
                #this should grab the last segment of the path

                #need to get the length of the start and finish segments of the pathway
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