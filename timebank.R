library(XML)
require(plyr)

####### FUNCTIONS #######

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
	x = getEvents(sentence)
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


####################################################

cat("\n", file="index.txt", sep="", append=TRUE)
index = getIndex("index.txt")
documents = getDocuments(index)
makeAllDocsUnique(documents)
docs = newXMLNode("documents")
addChildren(docs,documents)


####################################################

sentences = getNodeSet(docs,"//s")
events = getNodeSet(docs,"//EVENT")
instances = getNodeSet(docs,"//MAKEINSTANCE")
times = getNodeSet(docs,"//TIMEX3")
signals = getNodeSet(docs,"//SIGNAL")
cardinality = getNodeSet(docs,"//MAKEINSTANCE[@cardinality]")
modality = getNodeSet(docs,"//MAKEINSTANCE[@modality]")
plurals = getNodeSet(docs,"//MAKEINSTANCE[@cardinality='PLURAL']")

# > length(sentences)
# [1] 2624
# > length(events)
# [1] 7935
# > length(instances)
# [1] 7940
# > length(times)
# [1] 1414
# > length(signals)
# [1] 688

instanceList = lapply(instances,xmlAttrs) #this is just a list
n = length(instanceList) # 7940
#names(instanceList)<-c(1:length(instanceList))

d = as.data.frame(t(instanceList[[1]]))
d = data.frame()
for(i in 1:n){
	d = rbind.fill(
		d,as.data.frame(t(instanceList[[i]]))
		)
} # Now we have a data frame

summary(d)
#     eventID          eiid             tense                         aspect    
#  d4e30  :   2   d1ei392:   1   PAST      :2153   NONE                  :7315  
#  d8e229 :   2   d1ei418:   1   PRESENT   :1415   PROGRESSIVE           : 190  
#  d8e12  :   2   d1ei375:   1   NONE      :2793   PERFECTIVE            : 415  
#  d11e2  :   2   d1ei402:   1   FUTURE    : 284   PERFECTIVE_PROGRESSIVE:  20  
#  d14e26 :   2   d1ei382:   1   INFINITIVE: 782                                
#  d1e20  :   1   d1ei429:   1   PRESPART  : 361                                
#  (Other):7929   (Other):7934   PASTPART  : 152                                
#  polarity            pos          modality     cardinality  
#  POS:7651   VERB       :5122   would  : 127   PLURAL :   5  
#  NEG: 289   NOUN       :2225   could  :  49   2      :   4  
#             ADJECTIVE  : 266   may    :  31   4      :   3  
#             PREPOSITION:  28   can    :  26   EVERY  :   2  
#             OTHER      : 299   none   :  21   7      :   2  
#                               (Other):  66   (Other):  14  
#                               NA's   :7620   NA's   :7910 


nameList = list()
for(i in 1:n){
	nameList[[i]] <- names(instanceList[[i]])
	}
table(unlist(nameList))
     #aspect cardinality        eiid     eventID    modality    polarity 
     #  7940          30        7940        7940         320        7940 
     #   pos       tense 
     #  7940        7940 

xtabs(~tense + aspect, data=d)
#             aspect
# tense        NONE PROGRESSIVE PERFECTIVE PERFECTIVE_PROGRESSIVE
#   PAST       2039          19         94                      1
#   PRESENT     946         163        289                     17
#   NONE       2764           3         24                      2
#   FUTURE      275           5          4                      0
#   INFINITIVE  781           0          1                      0
#   PRESPART    360           0          1                      0
#   PASTPART    150           0          2                      0




# xmlName([node]) # self
# names([node]) # children
