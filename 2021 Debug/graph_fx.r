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

    #to create a graph without arrows going from 1 to 1, or 2 to 2, or etc... 
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