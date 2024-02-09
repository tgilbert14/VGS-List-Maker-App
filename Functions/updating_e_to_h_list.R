## update Exclusive list to H list ----

## Must link list as exclusive in Survey first - 
## then export it
## then run this script ->

## find out which list chosen was being used - get i value
new_i<- grep(selected_list, lists_being_used)
## find the PK_SpList in 2 columns above each ListName found - row_list
pk_look<- t[row_list[new_i]-2,]

pk_info<- gregexec("PK_SpList",pk_look)
s<- pk_info[[1]][1]
e<- pk_info[[1]][2]
pk_spList<- substr(pk_look,s+13,e-6)

## find location in survey where Fk_Items are using the selected PK_SpList - pk_sp
row_attributes_stored<- grep("CDATA", x = t$V1)
og_survey_att<- t[row_attributes_stored,]
## find template type below it
find_att_for_list<- gregexec(pk_spList, og_survey_att)
s1<- find_att_for_list[[1]][1]

temp_attributes<- substr(og_survey_att, s1, nchar(og_survey_att))

## sub out Exclusive for Hierarchical -> sub only does 1st instance
temp_attributes<- sub("Exclusive","Hierarchical",temp_attributes)

## put together temp attributes with original
final_attributes<- paste0(substr(og_survey_att,0,s1-1),substr(temp_attributes,0,nchar(temp_attributes)))

## re-attach final attributes with survey protocol
t[row_attributes_stored,] <- final_attributes

## Now need to update List IsHierarchical to True
## find all lists in survey uploaded - 4 rows down from ListName
t[row_list[new_i]+4,] <- sub("false", "true", t[row_list[new_i]+4,])

## update H status in VGS -> 
update_h_status <- paste0(
  "UPDATE spList SET IsHierarchical = 1 WHERE ListName ='",selected_list,"'"
)
## link species to list
dbExecute(mydb, update_h_status)


## then save as a survey to upload...
write_delim(t, "test_final.vgsp", quote = "none", escape = "none", col_names = FALSE)

## pop up survey location??
