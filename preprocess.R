library(XML)
require(plyr)




####### PREPROCESSING FUNCTIONS #######

# requires an index file, with the name of one file to be loaded per line
# might be replaced -- R functions for reading the contents of the working directory?
getIndex = function(index_file){
	strsplit(readLines(index_file), " ")[[1]]
}


getDocument = function(file){
	xmlRoot(xmlTreeParse(file, useInternal = TRUE))
} # useInternal maintains the xml document, for use with XPATH searches


getDocuments = function(index){
	n = length(index)
	x = vector("list",n)
	if(n>0){
		for(i in 1:n){
			x[[i]] = getDocument(index[[i]])
			id = paste(c("d",i), collapse="")
			xmlAttrs(x[[i]]) <- c(DocID=id)
		}
	return(x)
	}
}


UniqueXID = function(timeml_node, id_name){
	dID = xmlGetAttr(timeml_node,"DocID")
	search = paste("//*[@", id_name, "]", sep="")
	hits = getNodeSet(timeml_node,search)
	n = length(hits)
	if(n>0){
	for(i in 1:n){
		value = xmlGetAttr(hits[[i]],id_name)
		insert = paste(dID, value, sep="")
		xmlAttrs(hits[[i]])[[id_name]] = insert
	}}
}


makeUnique = function(doc){
	dID = xmlGetAttr(doc,"DocID")
	ids = c("eid", "eiid", "eventID", "tid", "sid", "lid", "timeID", "signalID", "eventInstanceID", "subordinatedEventInstance", "relatedToTime", "relatedToEventInstance")
	n = length(ids)
	for(i in 1:n){
		UniqueXID(doc,ids[[i]])
	}
}


makeAllDocsUnique = function(corpus){
	n = length(corpus)
	for(i in 1:n){
		makeUnique(corpus[[i]])
	}
}




########## Loading the files, creating the xml tree ############

cat("\n", file="index.txt", sep="", append=TRUE)
index = getIndex("index.txt")
documents = getDocuments(index)
makeAllDocsUnique(documents)
docs = newXMLNode("documents")
addChildren(docs,documents)




############## Additional Functions #############

getSentences = function(doc){
	getNodeSet(doc,"//s")
}

printSentence = function(sentence){
	gsub(" +"," ",gsub("\\n"," ",xmlValue(sentence)))
}

printDocument = function(document){
	paste(lapply(getSentences(document),printSentence), collapse=" | ")
}

getEventInstances = function(top,event){ # Takes an input of type EVENT
	eid = xmlGetAttr(event,"eid")
	# builds a search for MAKEINSTANCE objects with attribute eventID equal to input
	search = paste("//MAKEINSTANCE[@eventID='", eid, "']",sep="") 
	getNodeSet(top,search) # executes search, returning hits
}

getSentenceEventInstances = function(sentence){
	x = getNodeSet(sentence,"//EVENT")
	y = length(x)
	if(y>0){
		z <- vector("list", y)
		for(i in 1:y){
			a = getEventInstances(sentence,x[[i]])
			z[[i]] = c(x[[i]],a)
		}
		return(z)
	}
}

frameInstances = function(event_instance_list){
	x = length(event_instance_list)
	y = vector("list",x)
	for(i in 1:x){
		y[[i]] = event_instance_list[[i]][[2]]
	}
	sapply(y,xmlAttrs)
}

getSentenceEntities = function(sentences){
	if(length(sentences) > 0){
		for(i in 1:length(sentences)){ 
			sentence = sentences[[i]]
			print(sentence)
			events = getNodeSet(sentence,"EVENT")
			if(length(events) > 0){
				for(j in 1:length(events)){
					print(events[[j]])
					eid = xmlGetAttr(events[[j]],"eid")
					print(eid)
					getEventInstances(eid)
				}
			}
		}
	}
	else print("No sentences")
}

getValue = function(nodes){lapply(nodes, function (x) xmlSApply(x,xmlValue))}

			  
makeDataFrame = function(nodeSet){
	nodeList = lapply(nodeSet,xmlAttrs)
	n = length(nodeList)
	d = data.frame()
	for(i in 1:n){
		d = rbind.fill(
			d,as.data.frame(t(nodeList[[i]]))
		)
	}
	return(d)
}
# xmlToDataFrame				  

####################################################


