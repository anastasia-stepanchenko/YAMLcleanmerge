
# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
options(stringsAsFactors=FALSE)
obj.names  = load("yamlQN.RData", .GlobalEnv)

# remove empty fields
yaml.removeNulls = function(yaml_dset) {
  yaml_dset_clean = yaml_dset
  for (i in 1:length(yaml_dset)){
    ind_remove  = c()
    for (j in 1:length(yaml_dset[[i]]))
    {
      if(is.null(yaml_dset[[i]][[j]])) {
        ind_remove = c(ind_remove,j)}
    }
    for (m in sort(ind_remove, decreasing = TRUE)){ 
      yaml_dset_clean[[i]][m]  = NULL
    }
  }
  return (yaml_dset_clean)
}

corpus = yaml.removeNulls(yaml_list)

# merge fields, only those in fields_standard are left: 

yaml.mergeFields = function(yaml_dset, fields_to_merge, fields_standard) {
  yaml_dset_merged = yaml_dset
  for (i in 1:length(yaml_dset)){
    field_names = names(yaml_dset[[i]])
    ind_remove  = c()
    for (j in 1:length(field_names)){
      for (k in 1:length(fields_to_merge)){
        if (field_names[j] %in% fields_to_merge[[k]]){
          if (fields_standard[k] %in% field_names){
            app_to  = which(field_names %in% fields_standard[k])
            yaml_dset_merged[[i]][[app_to]] = paste(yaml_dset_merged[[i]][[app_to]],", ",yaml_dset_merged[[i]][[j]], sep="")
            ind_remove = c(ind_remove,j)}
          else{names(yaml_dset_merged[[i]])[j] = fields_standard[k]}}
      }
    }
    for (m in sort(ind_remove, decreasing = TRUE)){ 
      yaml_dset_merged[[i]][m]  = NULL
    }
  }
  return(yaml_dset_merged)
}

fields_to_merge = list(c("Author [New]", "Author[Update]"), c("Datafiles", "Datafile[example]"), 
                         c("Keywords[new]"), c("Name of QuantLet"), c("Subfunctions"))
fields_standard = c("Author", "Datafile", "Keywords", "Name of Quantlet", "Subfunction")
  
corpus = yaml.mergeFields(corpus, fields_to_merge, fields_standard)

# number of occurrences of all different fields
yaml.FieldNumDocs = function(yaml_dset) {
  field_names = c()
  field_ndocs = c()
  
  for (i in 1:length(yaml_dset)){
    row_names = names(yaml_dset[[i]])
    for (j in 1:length(row_names)){
      if (row_names[j] %in% field_names){
        m = match(row_names[j], field_names)
        field_ndocs[[m]] = field_ndocs[[m]]+1}
      else {
        field_names = c(field_names, row_names[j])
        field_ndocs = c(field_ndocs, 1)}
    }
  }
  Field_nDocs = data.frame(fields = field_names, ndocs  = field_ndocs)
  Field_nDocs = Field_nDocs[order(-Field_nDocs$ndocs),]
  return (Field_nDocs)
}

# plot
yaml.plotFieldNumDocs = function(yaml_dset) {
  Field_nDocs = yaml.FieldNumDocs(yaml_dset)
  Field_nDocs = transform(Field_nDocs, fields = factor(fields, levels = Field_nDocs$fields))

  plot = ggplot(data=Field_nDocs, aes(x=fields, y=ndocs, fill=fields)) + 
    geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") + 
    guides(fill=FALSE) + xlab("Fields") + ylab("Number of documents") +
    ggtitle("Number of documents containing a given field") + theme(axis.text.x = element_text(angle = 50, hjust = 1))
  
  return(plot)
}

yaml.plotFieldNumDocs(corpus)
