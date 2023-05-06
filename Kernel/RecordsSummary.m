BeginPackage["AntonAntonov`DataReshapers`RecordsSummary`"];

(*RecordsSummary::usage = "Summarizes datasets, lists, or associations that can be transformed into \*)
(*full two dimensional arrays. (I.e. lists of records.)";*)

Begin["`Private`"];

Needs["AntonAntonov`DataReshapers`"];

(*===========================================================*)
(* RecordsSummary and related functions                      *)
(*===========================================================*)

Clear[NumericVectorSummary, CategoricalVectorSummary];

NumericVectorSummary[dvec_] :=
    Block[{r, cm, ndvec = dvec},
      ndvec = DeleteMissing[dvec];
      If[ Length[ ndvec ] == 0,
        r = {},
        (* ELSE *)
        r = Flatten[Through[{Min, Max, Mean, Quartiles}[ndvec]]] /. x_Rational :> N[x];
        r = SortBy[Transpose[{{"Min", "Max", "Mean", "1st Qu", "Median", "3rd Qu"}, DeleteMissing[r]}], #[[2]] &];
      ];
      cm = Count[dvec, Missing[___]];
      If[ TrueQ[cm > 0], Append[r, { "Missing[___]", cm}], r ]
    ] /; VectorQ[DeleteMissing[dvec], NumberQ];

CategoricalVectorSummary[dvec_, maxTallies_Integer : 7] :=
    Block[{r, missingRows = {} },
      r = SortBy[Tally[dvec], -#[[2]] &];
      If[ !FreeQ[ r, Missing[___] ],
        missingRows = Cases[ r, {Missing[___], _} ];
        r = DeleteCases[ r, {Missing[___], _} ]
      ];
      If[Length[r] > 0 && Length[r] <= maxTallies, r,
        r = Join[r[[1 ;; maxTallies - 1]], {{"(Other)", Total[r[[maxTallies ;; -1, 2]]]}}]
      ];
      Join[ r, missingRows ]
    ] /; VectorQ[dvec];

Clear[DateObjectVectorSummary];
DateObjectVectorSummary[dvec_, args___] :=
    Block[{r, cm, ndvec = dvec},
      ndvec = DeleteMissing[dvec];
      If[Length[ndvec] == 0,
        r = {},
        (*ELSE*)
        r = NumericVectorSummary[AbsoluteTime /@ ndvec, args];
        r[[All, 2]] = DateObject /@ r[[All, 2]]
      ];
      cm = Count[dvec, Missing[___]];
      If[TrueQ[cm > 0], Append[r, {"Missing[___]", cm}], r]
    ] /; VectorQ[DeleteMissing[dvec], DateObjectQ];


Clear[DataColumnsSummary];
(* The option Thread->False is just for compatibility with RecordsSummary. *)
Options[DataColumnsSummary] = {"MaxTallies" -> 7, "NumberedColumns" -> True, Thread -> False};

DataColumnsSummary[dataColumns_, opts : OptionsPattern[]] :=
    DataColumnsSummary[dataColumns, Table["column " <> ToString[i], {i, 1, Length[dataColumns]}], opts];

DataColumnsSummary[dataColumns_, columnNamesArg_, opts : OptionsPattern[]] :=
    Block[{columnTypes, columnNames = columnNamesArg,
      maxTallies = OptionValue[DataColumnsSummary, "MaxTallies"],
      numberedColumnsQ = TrueQ[OptionValue[DataColumnsSummary, "NumberedColumns"]]},

      If[numberedColumnsQ,
        columnNames = MapIndexed[ToString[#2[[1]]] <> " " <> ToString[#1] &, columnNames]
      ];

      columnTypes =
          Map[
            Which[
              VectorQ[DeleteMissing[#], NumberQ], Number,
              VectorQ[DeleteMissing[#], DateObjectQ], DateObject,
              True, Symbol
            ] &,
            dataColumns];

      MapThread[
        Column[{
          Style[#1, Blue, FontFamily -> "Times"],

          Switch[#2,
            Number, Grid[NumericVectorSummary[#3], Alignment -> Left],

            DateObject, Grid[DateObjectVectorSummary[#3], Alignment -> Left],

            Symbol, Grid[CategoricalVectorSummary[#3, maxTallies], Alignment -> Left]
          ]
        }] &,
        {columnNames, columnTypes, dataColumns}, 1]

    ] /; Length[dataColumns] == Length[columnNamesArg];

RecordsSummary::args = "The first argument is expected to be a full array of depth 1 or 2, \
a dataset that can be converted to such a full array, an association, or a list of rules.";

RecordsSummary::igncols = "When the first argument is a dataset the second, column names argument is ignored.";

Clear[RecordsSummary];

SyntaxInformation[RecordsSummary] = {"ArgumentsPattern" -> {_, _., OptionsPattern[]}};

RecordsSummary[{}, ___] := {};

RecordsSummary[dataRecords_Dataset, dummy_List, opts : OptionsPattern[] ] :=
    Block[{},
      Message[RecordsSummary::igncols];
      RecordsSummary[dataRecords, opts]
    ];

RecordsSummary[dataRecords_Dataset, opts : OptionsPattern[] ] :=
    Block[{row1, colKeys, records},

      row1 = Normal[dataRecords[1]];
      If[ MatchQ[row1, _Association],
        colKeys = Keys[row1];
        ,
        colKeys = Table["", Length[row1]]
      ];

      Which[
        MatchQ[row1, _Association],
        records = Normal[dataRecords[All, Values]];
        If[ MatchQ[records, _Association], records = Values[records] ];
        RecordsSummary[ Normal[records], colKeys, opts ],

        True,
        records = Normal[dataRecords];
        If[ MatchQ[records, _Association], records = Values[records] ];
        RecordsSummary[ records, colKeys, opts ]
      ]
    ];

RecordsSummary[dataRecords_, opts : OptionsPattern[]] :=
    DataColumnsSummary[Transpose[dataRecords], opts] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 2 );

RecordsSummary[dataRecords_, columnNames_?AtomQ, opts : OptionsPattern[]] :=
    RecordsSummary[dataRecords, {columnNames}, opts];

RecordsSummary[dataRecords_, columnNames_, opts : OptionsPattern[]] :=
    DataColumnsSummary[Transpose[dataRecords], columnNames, opts] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 2 );

RecordsSummary[dataRecords_?AssociationQ, args___] :=
    RecordsSummary[Normal[dataRecords], args];

RecordsSummary[dataRecords_?DataRulesForClassifyQ, varNames_Rule, opts : OptionsPattern[]] :=
    Block[{newArgs = {opts}},
      newArgs = DeleteCases[newArgs, Rule[Thread, __] ];
      Rule @@
          MapThread[
            RecordsSummary[#1, #2, newArgs] &,
            {Transpose[List @@@ dataRecords], Map[Flatten@*List, List @@ varNames]}
          ]
    ] /; DataRulesForClassifyQ[List[varNames]] && MemberQ[{opts}, Thread -> True ];

RecordsSummary[dataRecords_?DataRulesForClassifyQ, args___] :=
    Block[{newArgs = {args}},
      newArgs = DeleteCases[newArgs, Rule[Thread, __] ];
      Rule @@ Map[RecordsSummary[#, newArgs]&, Transpose[List @@@ dataRecords]] /; MemberQ[{args}, Thread -> True ]
    ];

RecordsSummary[dataRecords_, args___ ] :=
    RecordsSummary[ List /@ dataRecords, args ] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 1 && Length[dataRecords] > 0);

RecordsSummary[a_Association, args___] := Map[ RecordsSummary[#, args]&, a];

RecordsSummary[___] := (Message[RecordsSummary::args];$Failed);

End[];

EndPackage[]