% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/excelling.R
\name{rename_columns}
\alias{rename_columns}
\title{Rename generic column names for export}
\usage{
rename_columns(prep_l)
}
\arguments{
\item{prep_l}{list of prepared data, which must include elements `data_points`, `sub_title`
or `legend_labels` and `transf_txt` if a transformation was made}
}
\value{
same list with updated column names in the dataframe(s) in `data_points`
}
\description{
From the prepared list used to plot the charts, this funciton renames the columns
with the raw and transformed data with the series names, preparing them
for export into an excel spreadsheet with the function \link[UMARvisualisR]{write_to_sheet}.
}
