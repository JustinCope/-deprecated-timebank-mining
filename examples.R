source(preprocessing.R)

####################################################

sentences = getNodeSet(docs,"//s")
events = getNodeSet(docs,"//EVENT")
instances = getNodeSet(docs,"//MAKEINSTANCE")
times = getNodeSet(docs,"//TIMEX3")
signals = getNodeSet(docs,"//SIGNAL")
tlinks = getNodeSet(docs,"//TLINK")
slinks = getNodeSet(docs,"//SLINK")
alinks = getNodeSet(docs,"//ALINK")

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
# > length(tlinks)
# [1] 6418
# > length(slinks)
# [1] 2932
# > length(alinks)
# [1] 265

# MAKEINSTANCE attributes
cardinality = getNodeSet(docs,"//MAKEINSTANCE[@cardinality]")
# length(cardinality)
# 30                 
plurals = getNodeSet(docs,"//MAKEINSTANCE[@cardinality='PLURAL']")
# length(plurals)
# 5   	
                                  
modality = getNodeSet(docs,"//MAKEINSTANCE[@modality]")
# length(modality)
# 320
                                  
# Converting a nodeset (instances) into a list as prerequisite for creating data frame			  
instanceList = lapply(instances,xmlAttrs) #this is just a list
n = length(instanceList) # 7940
#names(instanceList)<-c(1:length(instanceList))
                                  
# This is one way to determine how many event instances include the optional "cardinality" and "modality" attributes.
nameList = list()
for(i in 1:n){
	nameList[[i]] <- names(instanceList[[i]])
	}
table(unlist(nameList))
     #aspect cardinality        eiid     eventID    modality    polarity 
     #  7940          30        7940        7940         320        7940 
     #   pos       tense 
     #  7940        7940 


# Create dataframe; rbind.fill inserts the value "NA" into a cell (x,y) if the instance in row x no attribute corresponding to the name of column y
# d = as.data.frame(t(instanceList[[1]]))
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
