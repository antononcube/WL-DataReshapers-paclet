(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["AntonAntonov`DataReshapers`"];

CrossTensorate::usage = "Finds the contingency co-occurrence values for multiple columns of a matrix \
using a formula specification. The first argument is the formula with the form \
Count == cn1 + cn2 + ... or cn0 == cn1 + cn2 + ...";

CrossTensorateSplit::usage = "Splits the result of CrossTensorate along a variable. The result can be \
shown with MatrixForm.";

CrossTabulate::usage = "CrossTabulate[mat] finds the contingency table (of co-occurrence values) \
for the matrix argument mat that has two or three columns. \
If mat has three columns then the third column is expected to be a numerical vector. \
The result is an association by default; with the option setting \"Sparse\"->False the result a dataset. \
The result can be shown with MatrixPlot.";

CrossTabulationMatrixQ::usage = "Gives True if the argument is an Association with keys \
\"SparseMatrix\", \"RowNames\", and \"ColumnNames\".";

TypeOfDataToBeReshaped::usage = "TypeOfDataToBeReshaped[data] gives association with data type elements.";

LongFormDataset::usage = "LongFormDataset[ds_Dataset, idColumns_, variableColumns_, opts___] \
converts the dataset ds into long form.";

PivotLonger::usage = "PivotLonger[ds_Dataset, columns_, opts___] \
\"lengthens\" data, increasing the number of rows and decreasing the number of columns.";

WideFormDataset::usage = "WideFormDataset[ds_Dataset, idColumn_, variableColumn_, valueColumn_] \
converts the dataset ds into wide form. The result dataset has columns that are unique values of \
variableColumn. The cell values of the result dataset are derived by applying a specified \
aggregation function over each of the lists of valueColumn values that correspond \
to unique pairs of {idColumn, variableColumn}. \
The aggregation function is specified with the option \"AggregationFunction\".";

RecordsToLongFormDataset::usage = "RecordsToLongFormDataset[records: Association[(_ -> _Association) ..]] \
converts an association of associations into a long form dataset.";

RecordsToWideFormDataset::usage = "RecordsToWideFormDataset[records: { (_Association) ..}, aggrFunc_] \
converts a list of associations into a wide form dataset using a specified aggregation function.";

SeparateColumn::usage = "Separates the values of string column and makes new columns with them.";

RecordsSummary::usage = "Summarizes datasets, lists, or associations that can be transformed into \
full two dimensional arrays. (I.e. lists of records.)";

GridTableForm::usage="GridTableForm[arg] arranges the elements of arg in a two-dimensional grid.";

Begin["`Private`"];

Needs["AntonAntonov`DataReshapers`CrossTabulate`"];
Needs["AntonAntonov`DataReshapers`GridTableForm`"];
Needs["AntonAntonov`DataReshapers`LongAndWideForm`"];
Needs["AntonAntonov`DataReshapers`RecordsSummary`"];


End[];
EndPackage[];