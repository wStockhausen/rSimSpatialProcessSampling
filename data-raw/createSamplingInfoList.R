#--create data/lstSamplingInfo.rda object

load(file.path(rstudioapi::getActiveProject(),"data-raw/rda_SBSCharacteristics.RData"));
lstSamplingInfo=list(tdBSFRF=td.BSFRF,nwBSFRF=nw.BSFRF,asBSFRF=as.BSFRF,
                     tdNMFS =td.NMFS, nwNMFS =nw.NMFS, asNMFS =as.NMFS,
                     offset=offset);
usethis::use_data(lstSamplingInfo,internal=FALSE,overwrite=TRUE);
