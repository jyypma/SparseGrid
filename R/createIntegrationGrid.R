# Copyright (C) 2011 Jelmer Ypma. All Rights Reserved.
# This code is published under the GPL.
#
# File:   createIntegrationGrid.R
# Author: Jelmer Ypma
# Date:   17 May 2011
#
# Input: 
# Output: 

# return grid with the least number of nodes, either sparse grid or product rule grid
createIntegrationGrid <- function( type, dimension, k, sym = FALSE ) {
    builtinfct <- type %in% c('GQU', 'GQN', 'KPU', 'KPN' )
    stopifnot( builtinfct )
    
    integration.grid <- createSparseGrid( 
                                type=type, 
                                dimension=dimension, 
                                k=k )
    if ( length( integration.grid$weights ) > k^dimension ) {
        integration.grid <- createProductRuleGrid( type=type, dimension=dimension, k=k )
    }
    
    return( integration.grid )
}
