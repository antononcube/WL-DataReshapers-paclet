BeginPackage["AntonAntonov`DataReshapers`DatasetToMatrix`"];


(* ToAutomaticKeysAssociation::usage = "ToAutomaticKeysAssociation[ls_List, prefix_String] makes an association with \
automatically derived keys."; *)

(* DatasetToMatrix::usage = "Converts a dataset into a matrix according to option specified expected column names.
Amenable to be used in monad implementations. (Uses Echo instead of Message; uses option specified failure symbol)."; *)

Begin["`Private`"];

Needs["AntonAntonov`DataReshapers`"];

(***********************************************************)
(* Automatic keys Associations                             *)
(***********************************************************)

(* We use nd-1 because that includes the decimal point. *)
Clear[ToIDString];
ToIDString[i_Integer, nd_Integer] := ToString[NumberForm[i, {nd - 1, 0}, NumberPadding -> {"0", ""}]];

Clear[ToAutomaticKeysAssociation];
ToAutomaticKeysAssociation[ ls_List, prefix_String : "id." ] :=
    Block[{nd = Ceiling[Log10[Length[ls]]] + 1},
        AssociationThread[ Map[ prefix <> ToIDString[#, nd] &, Range[Length[ls]]], ls ]
    ];


(**************************************************************)
(* DatasetToMatrix                                            *)
(**************************************************************)

Clear[DatasetToMatrix];

SyntaxInformation[DatasetToMatrix] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[DatasetToMatrix] = {
  "ExpectedColumnNames" -> Automatic,
  "FunctionName" -> "DatasetToMatrix",
  "FailureSymbol" -> $Failed
};

DatasetToMatrix[dataArg_Dataset, opts : OptionsPattern[] ] :=
    Block[{data = dataArg, failureSymbol, functionName, namedRowsQ, expectedColNames, firstRecord, colNames},

      failureSymbol = OptionValue[ DatasetToMatrix, "FailureSymbol" ];

      functionName = OptionValue[ DatasetToMatrix, "FunctionName" ];
      If[ !StringQ[functionName],
        Echo["The value of the option \"FunctionName\" is expected to be a string.", functionName <> ":"];
        Return[failureSymbol]
      ];

      If[ AssociationQ[Normal[data]],
        namedRowsQ = True;
        data = data[Values];
      ];

      expectedColNames = OptionValue[ DatasetToMatrix, "ExpectedColumnNames" ];
      If[ !( ListQ[expectedColNames] && Length[expectedColNames] >= 1 || TrueQ[expectedColNames === Automatic]),
        Echo["The value of the option \"ExpectedColumnNames\" is expected to be a list with more than one element or Automatic.", functionName <> ":"];
        Return[failureSymbol]
      ];

      firstRecord = Normal[data[1, All]];
      colNames = If[ AssociationQ[firstRecord], Keys[firstRecord], None ];

      Which[
        TrueQ[colNames === None] && TrueQ[ expectedColNames === Automatic ],
        data = Normal @ data,

        ListQ[colNames] && TrueQ[ expectedColNames === Automatic ],
        data = Normal @ data[Values],

        TrueQ[colNames === None] && Dimensions[data][[2]] >= Length[expectedColNames],
        data = Normal @ data[All, Range[Length[expectedColNames]]],

        Length[ Intersection[ colNames, expectedColNames] ] == Length[expectedColNames],
        data = Normal @ data[All, expectedColNames][Values],

        Length[colNames] >= Length[expectedColNames],

        Echo[ "When the data argument is a dataset the expected columns are: " <> ToString @ Map[ "\"" <> # <> "\""&, expectedColNames ], functionName <> ":" ];

        (* colNames = Map[ If[ StringQ[#], "\"" <> # <> "\"", #]&, colNames ];*)

        Echo[ Row[{"Proceeding by considering the columns", Spacer[3], colNames, Spacer[3], "to correspond to the columns", Spacer[3], expectedColNames, "."}],
          functionName <> ":"
        ];

        data = Normal[data[Values, Range[Length[expectedColNames]]]],

        Length[colNames] < Length[expectedColNames],
        Echo[ "The dataset has too few columns.", functionName <> ":" ];
        Return[failureSymbol],

        True,
        Echo[ "Cannot use dataset.", functionName <> ":" ];
        Return[failureSymbol]
      ];

      If[ !MatrixQ[data, NumericQ],
        Echo[ "The columns of the dataset are expected to be numerical.", functionName <> ":" ];
        Return[failureSymbol]
      ];

      data
    ];

End[];

EndPackage[]    