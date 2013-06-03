#A simple program to making human based coding eaiser
#easycode_string(dataframe,string,string)
easycode <- function(X,data_column,coding_column,auto_fill){

	keep_going=TRUE #Stops the program when you are done coding, another way to stop is the ctrl+c
	while(keep_going){
	
	
		for( i in 1:length( X[,coding_column ] ) ) {
		
			if( is.na( X[i, coding_column ] ) ){
				x=readline(paste(X[i, data_column ]," ",sep=""))
				
				X[i,coding_column]=x
			}
				if(auto_fill){
					other_matches=grep(X[i, data_column ],X[,data_column]) #search within the data
					X[other_matches,coding_column]=x #replaces NA with answer
				}
			
				
		}
		
		#Section where I ask things to be parsed#
		parser=readline("Is there a specific format you would like the data 
		you just coded to be in? type int for integers. If not, just press enter ")
		#if yes#
		if(parser == "int"){
			X[,coding_column]=as.integer(X[,coding_column])
			}
		
		print(colnames(X)) #prints column names to make it eaiser to choose which one you want to switch too
		change=readline("Change coding column? type in the column name or N for no ") 
			if(change != "N"){
				coding_column=change
			}
		print(colnames(X))	#prints column names to make it eaiser to choose which one you want to switch too
		change_data=readline("Change data column to be read? type in the column name or N for no ")
			if(change_data != "N"){
				data_column=change_data
			}
		if((change == "N") & (change_data == "N")){
			keep_going=FALSE
			}
			
		
		}
	
	return(X)
}