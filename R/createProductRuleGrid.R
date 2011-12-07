# Copyright (C) 2011 Jelmer Ypma. All Rights Reserved.
# This code is published under the GPL.
#
# File:   createProductRuleGrid.R
# Author: Jelmer Ypma
# Date:   17 May 2011
#
# Input: 
# Output: 
# TODO: check what happens when we have 1 dimension, should not return anything?
# TODO: works only on built-in functions

# create integration grid according to product rule
createProductRuleGrid <- function( type, dimension, k, sym = FALSE ) {

    # assertions
	# TODO: check for sum(weights)==1 and length(weights)==nrow(nodes)
	if (! type %in% c('GQU', 'GQN', 'KPU', 'KPN' ) | is.function(type) ) {
		stop( paste("createProductRuleGrid expects type to be a string with values 'GQU', 'GQN', 'KPU', 'KPN', or a function, user supplied type = ", type, sep='') )
	}
	if ( is.function(type) ) {
		if (! formals( type ) == 1 ) {
			stop( "User supplied function (argument type) to createProductRuleGrid needs to have one argument." )
		}
		tmp.grid <- type( dimension )
		if (! is.list(tmp.grid) ) {
			stop( "User supplied function (argument type) to createProductRuleGrid needs to return a list." )
		}
		if (! is.list(tmp.grid) ) {
			stop( "User supplied function (argument type) to createProductRuleGrid needs to return a list." )
		}
		if (! "nodes" %in% names(tmp.grid) ) {
			stop( "Elemenet 'nodes' not found in list returned by user supplied function (argument type) to createProductRuleGrid." )	
		}
		if (! "weights" %in% names(tmp.grid) ) {
			stop( "Elemenet 'weights' not found in list returned by user supplied function (argument type) to createProductRuleGrid." )	
		}
	}
	if (! as.integer(dimension) == dimension) {
		stop( paste("createProductRuleGrid expects dimension to be integer, user supplied dimension = ", dimension, sep='') )
	}
	if (! as.integer(k) == k) {
		stop( paste("createProductRuleGrid expects k to be integer, user supplied k = ", k, sep='') )
	}
	if (! is.logical(sym) | is.na(sym)) {
		stop( paste("createProductRuleGrid expects sym to be logical, user supplied sym = ", sym, sep='') )
	}
	
	builtinfct <- type %in% c('GQU', 'GQN', 'KPU', 'KPN' )
    stopifnot( builtinfct )
    
    grid1d <- createSparseGrid( type, 1, k )

    num.nodes <- length(grid1d$nodes)

    tmp.grid <- grid1d

	if ( num.nodes > 1 ) {
		if ( dimension > 1 ) {
			for (i in 2:dimension) {
				tmp.grid$nodes <- cbind( tmp.grid$nodes[ rep(1:nrow(tmp.grid$nodes), each=num.nodes ), ], rep( grid1d$nodes, times=num.nodes ) )
				tmp.grid$weights <- rep( tmp.grid$weights, each=num.nodes ) * rep( grid1d$weights, times=num.nodes )
			}
		}
		tmp.grid$weights <- tmp.grid$weights / sum( tmp.grid$weights )
	}
	else {
		tmp.grid$nodes <- matrix( rep( grid1d$nodes, dimension ), nrow=1, ncol=dimension )
		tmp.grid$weights <- 1
	}
    
    
    return( tmp.grid )
}
