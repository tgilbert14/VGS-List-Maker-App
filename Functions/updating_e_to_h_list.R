## update Exclusive list to H list ----
## pop up to upload survey protocol
survey<- choose.files(".vgsp", caption = "Upload VGS survey to update", multi = F)

t<- read.delim(survey, header = F)

## find all lists in survey uploaded
row_list<- grep(";ListName", x = t$V1)
lists_being_used<- t[row_list,]

def_info<- gregexec("ListName",lists_being_used)
s<- def_info[[1]][1]
e<- def_info[[1]][2]
def_list<- substr(lists_being_used,s+12,e-6)

## then would have to select lists that are H to update

## this is for testing -> selected list
selected_list<- def_list

## find the PK_SpList in 2 columns above each ListName found - row_list
pk_look<- t[row_list-2,]

pk_info<- gregexec("PK_SpList",pk_look)
s<- pk_info[[1]][1]
e<- pk_info[[1]][2]
pk_sp<- substr(pk_look,s+13,e-6)

## then find location where Fk_Items are using the selected PK_SpList - pk_sp
row_att<- grep("CDATA", x = t$V1)
og_survey_att<- t[row_att,]
## find template type below it
find_at.1<- gregexec(pk_sp, og_survey_att)
s1<- find_at.1[[1]][1]

temp_att<- substr(og_survey_att, s1, nchar(og_survey_att))
## find next TemplateType:
temp_loc<- gregexec("TemplateType:",temp_att)
t1<- temp_loc[[1]][1]
substr(temp_att, t1+13, t1+100)
## then update E list to H list

## then save as a survey to upload...

write_delim(t, "test.vgsp", quote = "none", escape = "none", col_names = FALSE)


