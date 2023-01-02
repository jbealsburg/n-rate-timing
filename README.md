# n-rate-timing
nitrogen rate timing experiment

nrate_eda_9dec2022.R is my first shot at exploring data. It is a supportive file used in report.Rmd

report.Rmd is where I am compiling data insights into a singular document that can be shared with other collaborators. 

Today I looked at the height data and I think there is an issue when I converted it to a factor and then back to numeric. I needed to make changes to code in the original eda R file, which then is messed up some code later on in the file because it was poorly written to redefine an already defined object, resulting in downstream issues with binding datasets together. I do not want to push any of the changes I made since it broke everything

To move forward, I need to not be defining height data as a factor, this was the original sin. I may need to remake a new import R script based on the nrate_eda now that I have a better idea of where I'm going with analysis
