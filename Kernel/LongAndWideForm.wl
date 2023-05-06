(* :Title: DataReshape *)
(* :Context: DataReshape` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-09-07 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: long form, wide form, dataset, reshape *)
(* :Discussion:

    # In brief

    Functions for conversion of Dataset objects and matrices into long form or wide form.

    The original version of this paclet file is [AAp1].

    # Rationale

    Obviously inspired from R's package "reshape2", [HW1].


    # Usage examples



    # References

    [HW1] Hadley Wickham, reshape2: Flexibly Reshape Data: A Reboot of the Reshape Package, (2017), cran.r-project.org.
        URL: https://cran.r-project.org/web/packages/reshape2/index.html .

    [AAp1] Anton Antonov, Data Reshape Mathematica package, MathematicaForPrediction at GitHub.
        URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/DataReshape.m .

    Anton Antonov
    Windermere, FL, USA
    2018-09-07

*)

BeginPackage["AntonAntonov`DataReshapers`LongAndWideForm`"];

Begin["`Private`"];

Needs["AntonAntonov`DataReshapers`"];

(***********************************************************)
(* Column specs                                            *)
(***********************************************************)

Clear[ColumnSpecQ, ColumnSpecsListQ];

ColumnSpecQ[x_] := IntegerQ[x] || StringQ[x] || MatchQ[x, Key[__]];

ColumnSpecsListQ[x_] := VectorQ[x, ColumnSpecQ];


(***********************************************************)
(* TypeOfDataToBeReshaped                                  *)
(***********************************************************)

Clear[TypeOfDataToBeReshaped];

TypeOfDataToBeReshaped[ data_?MatrixQ ] := { "Type" -> "Matrix", "ColumnNames" -> None, "RowNames" -> None };

TypeOfDataToBeReshaped[ data : { _Association .. } ] :=
    If[ MatrixQ[Values /@ data],
      { "Type" -> "ListOfAssociations", "ColumnNames" -> Union[Keys /@ data], "RowNames" -> None },
      (*ELSE*)
      Echo[ "Unhandled dataset case!", "TypeOfDataToBeReshaped:" ];
      Return[$Failed]
    ];

TypeOfDataToBeReshaped[ data_Association ] := { "Type" -> "Association", "ColumnNames" -> None, "RowNames" -> None };

TypeOfDataToBeReshaped[ data_Dataset ] :=
    Block[{namedRowsQ = False, firstRecord, colNames, resType},

      If[ AssociationQ[Normal[data]],
        namedRowsQ = True;
      ];

      firstRecord = Normal[data[1, All]];
      colNames = If[ AssociationQ[firstRecord], Keys[firstRecord], None ];

      resType =
          Which[
            TrueQ[colNames === None] && !namedRowsQ,
            "Dataset-NoColumnNames-NoRowNames",

            TrueQ[colNames === None] && namedRowsQ,
            "Dataset-NoColumnNames-RowNames",

            ListQ[colNames] && namedRowsQ,
            "Dataset-ColumnNames-RowNames",

            ListQ[colNames] && !namedRowsQ,
            "Dataset-ColumnNames-NoRowNames",

            True,
            Echo[ "Unhandled dataset case!", "TypeOfDataToBeReshaped:" ];
            Return[$Failed]
          ];

      <| "Type" -> resType, "ColumnNames" -> colNames, "RowNames" -> If[ namedRowsQ, Keys @ Normal @data, None ] |>

    ];


(***********************************************************)
(* LongFormDataset                                              *)
(***********************************************************)

Clear[LongFormDataset];

SyntaxInformation[LongFormDataset] = { "ArgumentsPattern" -> { _, _., _., OptionsPattern[] } };

Options[LongFormDataset] = {
  "IdentifierColumns" -> Automatic,
  "VariableColumns" -> Automatic,
  "AutomaticKeysTo" -> "AutomaticKey",
  "VariablesTo" -> "Variable",
  "ValuesTo" -> "Value" };

LongFormDataset[mat_?MatrixQ, args___ ] := LongFormDataset[ Dataset[mat], args];

LongFormDataset[ds : { _Association .. }, args___ ] := LongFormDataset[ Dataset[ds], args] /; MatrixQ[ Values /@ ds ];

LongFormDataset[ds_Association, opts : OptionsPattern[] ] := RecordsToLongFormDataset[ds, opts];

LongFormDataset[ds_Dataset, opts : OptionsPattern[] ] :=
    Block[{ idCols, varCols },
      idCols = OptionValue[LongFormDataset, "IdentifierColumns"];
      varCols = OptionValue[LongFormDataset, "VariableColumns"];
      LongFormDataset[ ds, idCols, varCols, opts ]
    ];

LongFormDataset[ds_Dataset, idColumnsSpec_, opts : OptionsPattern[] ] :=
    LongFormDataset[ds, idColumnsSpec, Automatic, opts];

LongFormDataset[ds_Dataset, Automatic, Automatic, opts : OptionsPattern[] ] :=
    LongFormDataset[ds, {0}, Range[ Length[ ds[1] ] ], opts];

LongFormDataset[ds_Dataset, idColumn_?ColumnSpecQ, valueColumns_, opts : OptionsPattern[] ] :=
    LongFormDataset[ds, {idColumn}, valueColumns, opts];

LongFormDataset[ds_Dataset, idColumns_, valueColumn_?ColumnSpecQ, opts : OptionsPattern[] ] :=
    LongFormDataset[ds, idColumns, {valueColumn}, opts];

LongFormDataset[ds_Dataset, Automatic, valueColumns : {_Integer ..}, opts : OptionsPattern[] ] :=
    Block[{idColumns},

      idColumns = Complement[ Range @ Length[ds[1]], valueColumns ];

      If[ Length[idColumns] == 0, idColumns = {0} ];

      LongFormDataset[ds, idColumns, valueColumns, opts]
    ];

LongFormDataset[ds_Dataset, idColumns : {_Integer ..}, Automatic, opts : OptionsPattern[] ] :=
    Block[{valueColumns},

      valueColumns = Complement[ Range @ Length[ds[1]], idColumns ];

      If[ Length[valueColumns] == 0,
        ds,
        (*ELSE*)
        LongFormDataset[ds, idColumns, valueColumns, opts]
      ]
    ];

LongFormDataset[ds_Dataset, idColumns : {_Integer ..}, valueColumns : {_Integer ..}, opts : OptionsPattern[] ] :=
    Block[{records = Normal[ds], cnAuto},

      cnAuto = OptionValue[LongFormDataset, "AutomaticKeysTo"];

      records =
          Which[
            TrueQ[idColumns == {0}] && MatchQ[records, Association[(_ -> _Association) ..]],
            Association@
                KeyValueMap[<|cnAuto -> #1|> -> KeyTake[#2, Keys[#2][[valueColumns]]] &, records],

            ! TrueQ[idColumns == {0}] && MatchQ[records, Association[(_ -> _Association) ..]],
            Association@
                Map[KeyTake[#, Keys[#][[idColumns]]] ->
                    KeyTake[#, Keys[#][[valueColumns]]] &, Values[records]],

            TrueQ[idColumns == {0}] && MatchQ[records, List[_Association ..]],
            Association@
                MapIndexed[<|cnAuto -> #2[[1]]|> -> KeyTake[#1, Keys[#1][[valueColumns]]] &, records],

            MatchQ[records, List[(_Association) ..]],
            Association@
                Map[KeyTake[#, Keys[#][[idColumns]]] ->
                    KeyTake[#, Keys[#][[valueColumns]]] &, records],

            TrueQ[idColumns == {0}] && MatchQ[records, List[(_List) ..]],
            Association@
                MapIndexed[
                  <| cnAuto -> #2[[1]] |> -> AssociationThread[ToString /@ valueColumns, #1[[valueColumns]]] &,
                  records],

            MatchQ[records, List[(_List) ..]],
            Association@
                Map[AssociationThread[ToString /@ idColumns, #[[idColumns]]] ->
                    AssociationThread[ToString /@ valueColumns, #[[valueColumns]]] &,
                  records],

            True,
            Return[$Failed]
          ];

      RecordsToLongFormDataset[records, opts]

    ] /; (TrueQ[idColumns == {0}] ||
        Apply[And, Map[1 <= # <= Dimensions[ds][[2]] &, idColumns]]) &&
        Apply[And, Map[1 <= # <= Dimensions[ds][[2]] &, valueColumns]] &&
        Length[Intersection[idColumns, valueColumns]] == 0;


LongFormDataset::nocolkeys = "If the second and third arguments are not column indices the dataset should have named columns.";

LongFormDataset::colkeys = "If the second and third arguments are not Automatic or column indices \
then they are expected to be columns names of the dataset.";

LongFormDataset[ds_Dataset, idColumnsArg : ( Automatic | _?ColumnSpecsListQ ), valueColumnsArg : ( Automatic | _?ColumnSpecsListQ ), opts : OptionsPattern[] ] :=
    Block[{idColumns = idColumnsArg, valueColumns = valueColumnsArg, keys},

      keys = Normal[ds[1]];

      If[!AssociationQ[keys],
        Message[LongFormDataset::nocolkeys];
        Return[$Failed]
      ];

      keys = Keys[keys];

      If[
        !( TrueQ[ idColumns === Automatic ] || Length[idColumns] == 0 || Apply[And, Map[ MemberQ[keys, #]&, idColumns ] ] ) ||
            !( TrueQ[ valueColumns === Automatic ] || Apply[And, Map[ MemberQ[keys, #]&, valueColumns ] ] ),

        Message[LongFormDataset::colkeys];
        Return[$Failed]
      ];

      If[ TrueQ[ idColumns === Automatic ],
        idColumns = Complement[keys, valueColumns];
      ];

      If[ TrueQ[ valueColumns === Automatic ],
        valueColumns = Complement[keys, idColumns];
      ];

      Which[
        Length[valueColumns] == 0,
        ds,

        Length[idColumns] == 0,
        LongFormDataset[ds, {0}, Flatten[Position[keys, #]& /@ valueColumns], opts ],

        True,
        LongFormDataset[ds, Flatten[Position[keys, #]& /@ idColumns], Flatten[Position[keys, #]& /@ valueColumns], opts ]
      ]

    ];

LongFormDataset[ds_Dataset, "AutomaticKey", valueColumns_?ColumnSpecsListQ, opts : OptionsPattern[] ] :=
    Block[{keys},
      keys = Normal[ds[1]];

      If[!AssociationQ[keys],
        Message[LongFormDataset::nocolkeys];
        Return[$Failed]
      ];

      keys = Keys[keys];

      If[ ! Apply[And, Map[ MemberQ[keys, #]&, valueColumns ] ],
        Message[LongFormDataset::colkeys];
        Return[$Failed]
      ];

      LongFormDataset[ds, 0, Flatten[Position[keys, #]& /@ valueColumns], opts ]
    ];

LongFormDataset::args = "The first argument is expected to be an association or a dataset. \
If the first argument is an association then no other arguments are expected. \
If the first argument is a dataset then the rest of the arguments are expected to be columns specifications or Automatic.";

LongFormDataset[___] :=
    Block[{},
      Message[LongFormDataset::args];
      $Failed
    ];


(***********************************************************)
(* RecordsToLongFormDataset                                       *)
(***********************************************************)

(* RecordsToLongFormDataset is an "internal" function. It is assumed that all records have the same keys. *)
(* valueColumns is expected to be a list of keys that is a subset of the records keys. *)

Clear[NotAssociationQ];
NotAssociationQ[x_] := Not[AssociationQ[x]];

Clear[RecordsToLongFormDataset];

Options[RecordsToLongFormDataset] = Options[LongFormDataset];

RecordsToLongFormDataset[records : Association[( _?NotAssociationQ -> _Association) ..], opts : OptionsPattern[]] :=
    Block[{cnAuto},
      cnAuto = OptionValue[RecordsToLongFormDataset, "AutomaticKeysTo"];
      RecordsToLongFormDataset[ KeyMap[ <|cnAuto -> #|>&, records ], opts ]
    ];

RecordsToLongFormDataset[records : Association[(_Association -> _Association) ..], opts : OptionsPattern[] ] :=
    Block[{cnVariables, cnValues, res},

      cnVariables = OptionValue[RecordsToLongFormDataset, "VariablesTo"];
      cnValues = OptionValue[RecordsToLongFormDataset, "ValuesTo"];

      res =
          KeyValueMap[
            Function[{k, rec}, Map[Join[k, <|cnVariables -> #, cnValues -> rec[#]|>] &, Keys[rec]]],
            records
          ];

      Dataset[Flatten[res]]
    ];


(***********************************************************)
(* PivotLonger                                             *)
(***********************************************************)

(* More or less follows the interface of the R-package function tidyr::pivot_longer . *)

Clear[PivotLonger];

Options[PivotLonger] = {
  "Data" -> None,
  "Columns" -> None,
  "NamesTo" -> "Variable",
  "ValuesTo" -> "Value",
  "DropMissingValues" -> False
};

PivotLonger[ds_Association, opts : OptionsPattern[] ] := LongFormDataset[ds, opts];

PivotLonger[ data_Dataset, columnSpec : _?ColumnSpecQ, opts : OptionsPattern[] ] :=
    PivotLonger[ data, {columnSpec}, opts];

PivotLonger[ data_Dataset, columnsArg : { _?ColumnSpecQ  ..}, opts : OptionsPattern[] ] :=
    Block[{columns = columnsArg, cnVariables, cnValues, dropMissingQ, res},

      cnVariables = OptionValue[PivotLonger, "NamesTo"];
      cnValues = OptionValue[PivotLonger, "ValuesTo"];
      dropMissingQ = OptionValue[PivotLonger, "DropMissingValues"];

      res = LongFormDataset[ data, Automatic, columns, "VariablesTo" -> cnVariables, "ValuesTo" -> cnValues ];

      If[ dropMissingQ,
        res = res[ Select[ !MissingQ[#[cnValues]]& ] ];
      ];

      res
    ];


(***********************************************************)
(* WideFormDataset                                              *)
(***********************************************************)

(* Essentially a contingency dataset making. *)

Clear[WideFormDataset];

SyntaxInformation[WideFormDataset] = { "ArgumentsPattern" -> { _, _, _, _, OptionsPattern[] } };

Options[WideFormDataset] = {
  "IdentifierColumns" -> Automatic,
  "VariablesFrom" -> Automatic,
  "ValuesFrom" -> Automatic,
  "AggregationFunction" -> Total
};

WideFormDataset[ ds_Dataset, opts : OptionsPattern[] ] :=
    Block[{ idCols, varCol, valCol },
      idCols = OptionValue[WideFormDataset, "IdentifierColumns"];
      varCol = OptionValue[WideFormDataset, "VariablesFrom"];
      valCol = OptionValue[WideFormDataset, "ValuesFrom"];
      WideFormDataset[ ds, idCols, varCol, valCol, opts ]
    ];

WideFormDataset[ ds_Dataset, idColumn_Integer, variableColumn_Integer, valueColumn_Integer, opts : OptionsPattern[] ] :=
    WideFormDataset[ ds, {idColumn}, variableColumn, valueColumn, opts];

WideFormDataset[ ds_Dataset, idColumns : { _Integer.. }, variableColumn_Integer, valueColumn_Integer, opts : OptionsPattern[] ] :=
    Block[{records = Normal[ds]},

      records =
          Which[
            TrueQ[idColumns == {0}] && MatchQ[records, Association[(_ -> _Association) ..]],
            KeyValueMap[ <| "AutomaticKey" -> #1, Values[#2][[variableColumn]] -> Values[#2][[valueColumn]] |> &, records],

            ! TrueQ[idColumns == {0}] && MatchQ[records, Association[(_ -> _Association) ..]],
            Map[ <| Keys[#][[idColumns]] -> Values[#][[idColumns]] , Values[#][[variableColumn]] -> Values[#][[valueColumn]] |> &, Values[records]],

            MatchQ[records, List[(_Association) ..]],
            Map[ <| Keys[#][[idColumns]] -> Values[#][[idColumns]] , Values[#][[variableColumn]] -> Values[#][[valueColumn]] |> &, records],

            MatchQ[records, List[(_List) ..]],
            Map[ <| idColumns -> #[[idColumns]], #[[variableColumn]] -> #[[valueColumn]] |> &, records],

            True,
            Return[$Failed]
          ];

      RecordsToWideFormDataset[records, OptionValue[WideFormDataset, "AggregationFunction"] ]

    ] /; (idColumns == {0} ||
        Apply[And, 1 <= # <= Dimensions[ds][[2]] & /@ idColumns]) && (1 <=
        variableColumn <= Dimensions[ds][[2]]) && (1 <= valueColumn <=
        Dimensions[ds][[2]]) && (Length[
      Union@Flatten@{idColumns, variableColumn, valueColumn}] >= 3);


WideFormDataset::nocolkeys = "If the second and third arguments are not column indices the dataset should have named columns.";

WideFormDataset::colkeys = "If the second, third, and fourth arguments are not column indices then they are expected to be columns names of the dataset.";

Clear[ColumnNameIndex];
ColumnNameIndex[keys_, cols_List] := ColumnNameIndex[keys, #]& /@ cols;
ColumnNameIndex[keys_, col_] := First @ Flatten @ Position[keys, col];

WideFormDataset[ds_Dataset, idColumns_, variableColumn_, valueColumn_, opts : OptionsPattern[] ] :=
    Block[{keys},
      keys = Normal[ds[1]];

      If[!AssociationQ[keys],
        Message[WideFormDataset::nocolkeys];
        Return[$Failed]
      ];

      keys = Keys[keys];

      If[ ! Apply[And, Map[ MemberQ[keys, #]&, Flatten @ {idColumns, variableColumn, valueColumn} ] ],
        Message[WideFormDataset::colkeys];
        Return[$Failed]
      ];

      WideFormDataset[ds, ColumnNameIndex[keys, idColumns], ColumnNameIndex[keys, variableColumn], ColumnNameIndex[keys, valueColumn], opts ]
    ];

WideFormDataset[ds_Dataset, Automatic, variableColumn_, valueColumn_, opts : OptionsPattern[] ] :=
    Block[{keys},
      keys = Normal[ds[1]];

      If[!AssociationQ[keys],
        Message[WideFormDataset::nocolkeys];
        Return[$Failed]
      ];

      keys = Keys[keys];

      If[ ! Apply[And, Map[ MemberQ[keys, #]&, {variableColumn, valueColumn} ] ],
        Message[WideFormDataset::colkeys];
        Return[$Failed]
      ];

      WideFormDataset[ds, 0, ColumnNameIndex[keys, variableColumn], ColumnNameIndex[keys, valueColumn], opts ]
    ];

WideFormDataset::args = "The first argument is expected to be a dataset; \
the rest of the arguments are expected to be columns specifications.";

WideFormDataset[___] :=
    Block[{},
      Message[WideFormDataset::args];
      $Failed
    ];


(***********************************************************)
(* RecordsToWideFormDataset                                       *)
(***********************************************************)

RecordsToWideFormDataset[records : { (_Association) ..}, aggrFunc_] :=
    Block[{res, keyColNames, colNames},

      keyColNames = Keys[records[[1]]][[1]];

      res = GroupBy[records, {Keys[#][[1]] -> #[[1]], Keys[#][[2]]} &, aggrFunc@Map[Function[{r}, r[Keys[r][[2]]]], #] &];

      res = KeyValueMap[<|AssociationThread @@ #1[[1]], #1[[2]] -> #2|> &, res];

      res = Dataset[Values @ GroupBy[res, # /@ keyColNames &, Join[Association[#]] &]];

      colNames = DeleteDuplicates[Flatten[Normal[res[All, Keys]]]];

      res[All, Join[AssociationThread[colNames -> Missing[]], #]& ]
    ];


(***********************************************************)
(* SeparateColumn                                          *)
(***********************************************************)

Clear[SeparateColumn];

Options[SeparateColumn] = {"Separator" -> (Except[WordCharacter]..), "RemoveInputColumn" -> True };

SeparateColumn[ dsArg_Dataset, colSpec_, intoSpec_List, opts : OptionsPattern[] ] :=
    Block[{ds = dsArg, sep, removeInputColumnQ, typeRes, column, newValues, res},

      sep = OptionValue[SeparateColumn, "Separator"];
      removeInputColumnQ = TrueQ[ OptionValue[SeparateColumn, "RemoveInputColumn"] ];

      typeRes = TypeOfDataToBeReshaped[ds];

      column = ds[All, colSpec];

      Which[
        TrueQ[Dataset`GetType[column] === $Failed],
        Return[column],

        !MatchQ[ Dataset`GetType[column], h_[___] /; SymbolName[h] == "Vector" ],
        Return[$Failed]
      ];

      newValues = StringSplit[ #, sep ]& /@ Normal[column];

      If[ TrueQ[typeRes["ColumnNames"] === None],
        newValues = Map[ Take[#, UpTo @ Length @ intoSpec]&, newValues ],
        (*ELSE*)
        newValues = Map[ AssociationThread[ Take[ intoSpec, UpTo @ Length @ #], Take[#, UpTo @ Length @ intoSpec] ]&, newValues ]
      ];

      If[ removeInputColumnQ && !MemberQ[ intoSpec, colSpec ],
        Which[
          TrueQ[typeRes["ColumnNames"] === None],
          ds = ds[All, Complement[Range @ Dimensions[ds][[2]], {colSpec}]  ],

          True,
          ds = ds[All, KeyDrop[#, colSpec]& ]
        ]
      ];

      res = Join[ ds, Dataset[newValues], 2 ];

      If[ !MatchQ[ res, _Dataset],
        Return[res]
      ];

      res
    ];

End[]; (* `Private` *)

EndPackage[];