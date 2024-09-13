# aws_workspace_scripts  


In this repository, AMP SCZ investigators can share scripts that they use on the AWS workspace  

As an example, we have created a first folder called: npenzel_derive_clinical_outcomes_nda2.  
In this folder, we include 3 scripts:  

1.) create_auxiliary_df.R (extracts key variables for all subjects included in NDA2)  
2.) create_clinical_df.R (extracts outcome variables defined by experts from Team B/I for thte different clinical questionnaires)  
3.) load_aux.R (function used by create_clinical_df.R to load auxiliaries for combining with the clinical df).  

When you add scripts to this repository, please be aware that they are modifiable and accessible by all AMP SCZ investigators.  
Thus, make sure that you have a security copy and further, saving files would not overwrite crucial files.  
Following the naming convention of the first folder, please name your folders accordingly, i.e., githubhandle_descriptivename.  
Add first your githubhandle and then use a folder-name that is descriptive of its content.
