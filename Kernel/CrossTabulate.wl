(* :Title: CrossTabulate *)
(* :Context: CrossTabulate` *)
(* :Author: Anton Antonov *)
(* :Date: 2017-10-13 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: cross tabulation, cross tabulate, contingency matrix, xtabs *)
(* :Discussion:

    # Motivation

    In statistics contingency tables are matrices used to show the co-occurrence of variable values
    of multi-dimensional data. They are fundamental in many types of research.
    This Mathematica package has several functions for the construction of contingency tables.

    For extensive examples and explanations see [AA1].

    The original version of this paclet file is [AAp1].

    # Usage example

        titanicData = Flatten@*List @@@ ExampleData[{"MachineLearning", "Titanic"}, "Data"];
        titanicData = DeleteCases[titanicData, {___, _Missing, ___}];

        titanicColumnNames = Flatten@*List @@ ExampleData[{"MachineLearning", "Titanic"}, "VariableDescriptions"];
        aTitanicColumnNames = AssociationThread[titanicColumnNames -> Range[Length[titanicColumnNames]]];

        ctCounts = CrossTabulate[titanicData[[All, aTitanicColumnNames /@ {"passenger class", "passenger survival"}]]];
        MatrixForm[#1, TableHeadings -> {#2, #3}] & @@ ctCounts

        ctTotalAge = CrossTabulate[titanicData[[All, aTitanicColumnNames /@ {"passenger class", "passenger survival", "passenger age"}]]];
        MatrixForm[#1, TableHeadings -> {#2, #3}] & @@ ctTotalAge

        MatrixForm[ctTotalAge[[1]]/Normal[ctCounts[[1]]], TableHeadings -> Values[Rest[ctTotalAge]]]


    # References

    [AA1] Anton Antonov, "Contingency tables creation examples", MathematicaForPrediction at WordPress.
        URL: https://mathematicaforprediction.wordpress.com/2016/10/04/contingency-tables-creation-examples/ .

    [AAp1] Anton Antonov, CrossTabulate Mathematica package, MathematicaForPrediction at GitHub.
        URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/CrossTabulate.m .

    Anton Antonov
    October 2017
    Windermere, FL, USA

*)

BeginPackage["AntonAntonov`DataReshapers`CrossTabulate`"];

(*CrossTensorate::usage = "Finds the contingency co-occurrence values for multiple columns of a matrix \*)
(*using a formula specification. The first argument is the formula with the form \*)
(*Count == cn1 + cn2 + ... or cn0 == cn1 + cn2 + ...";*)

(*CrossTensorateSplit::usage = "Splits the result of CrossTensorate along a variable. The result can be \*)
(*shown with MatrixPlot.";*)

(*ToAssociationTrie::usage = "Converts a result of CrossTabulate or CrossTensorate into a nested Association. (A trie.)";*)

(*CrossTabulate::usage = "CrossTabulate[mat] finds the contingency table (of co-occurrence values) \*)
(*for the matrix argument mat that has two or three columns. \*)
(*If mat has three columns then the third column is expected to be a numerical vector. \*)
(*The result is an association by default; with the option setting \"Sparse\"->False the result a dataset. \*)
(*The result can be shown with MatrixPlot.";*)

(*CrossTabulationMatrixQ::usage = "Gives True if the argument is an Association with keys \*)
(*\"SparseMatrix\", \"RowNames\", and \"ColumnNames\".";*)

Begin["`Private`"];

Needs["AntonAntonov`DataReshapers`"];

(*===========================================================*)
(* CrossTensorate                                            *)
(*===========================================================*)

Clear[CrossTensorate];

SyntaxInformation[CrossTensorate] = {"Arguments" -> {_, _, _.}};

SetAttributes[CrossTensorate, HoldFirst];

CrossTensorate::wcnames = "The third argument for the data column names is expected to be Automatic, \
an Association, or a list with length equal to the number of columns in the data." ;

CrossTensorate::wargs = "Wrong arguments.";

CrossTensorate::mcnames = "Not all formula column names are found in the column names specified by \
the third argument.";

CrossTensorate[formula_Equal, data_Dataset, columnNames_ : Automatic ] :=
    Block[{colKeys},

      colKeys = Normal[ data[[1]] ];

      Which[
        MatchQ[colKeys, _Association] && TrueQ[columnNames === Automatic],
        CrossTensorate[ formula, Normal[data[All, Values]], Keys[colKeys] ],

        MatchQ[colKeys, _Association],
        CrossTensorate[ formula, Normal[data[All, Values]], columnNames ],

        True,
        CrossTensorate[ formula, Normal[data], columnNames ]
      ]
    ] /; Length[Dimensions[data]] == 2;

CrossTensorate[formula_Equal, data_?MatrixQ, columnNames_ : Automatic] :=
    Block[{aColumnNames, idRules, formulaLHS, formulaRHS, t},

      Which[
        TrueQ[columnNames === Automatic],
        aColumnNames =
            AssociationThread[Range[Dimensions[data][[2]]] -> Range[Dimensions[data][[2]]]],

        ListQ[columnNames] && Length[columnNames] == Dimensions[data][[2]],
        aColumnNames = AssociationThread[columnNames -> Range[Dimensions[data][[2]]]],

        AssociationQ[columnNames],
        aColumnNames = columnNames,

        True,
        Message[CrossTensorate::wcnames];
        Return[{}]
      ];

      aColumnNames =
          Join[ aColumnNames, AssociationThread[Range[Dimensions[data][[2]]] -> Range[Dimensions[data][[2]]]] ];

      formulaLHS = Hold[formula][[1, 1]];

      If[! TrueQ[formulaLHS === Count], formulaLHS = aColumnNames[formulaLHS]];

      formulaRHS = ReleaseHold[Hold[formula] /. Plus -> List][[2]];

      If[Length[Intersection[Keys[aColumnNames], formulaRHS]] < Length[formulaRHS],
        Message[CrossTensorate::mcnames]; Return[{}]
      ];

      formulaRHS = aColumnNames /@ formulaRHS;
      idRules = Table[(t = Union[data[[All, i]]];Dispatch@Thread[t -> Range[Length[t]]]), {i, formulaRHS}];

      Which[
        TrueQ[formulaLHS === Count],
        t = SparseArray @
            Map[MapThread[Replace, {#[[1]], idRules}] -> #[[2]] &, Tally[data[[All, formulaRHS]]]],

        IntegerQ[formulaLHS],
        t = SparseArray @
          Map[
            MapThread[Replace, {#[[1]], idRules}] -> #[[2]] &,
            Map[
              {#[[1, 1 ;; -2]], Total[#[[All, -1]]]} &,
              GatherBy[data[[All, Append[formulaRHS, formulaLHS]]], Most]]
          ],

        True,
        Message[CrossTensorate::wargs]; Return[{}]
      ];

      Join[<|"XTABTensor" -> t|>, AssociationThread[ Keys[aColumnNames][[formulaRHS]] -> Map[Normal[#][[All, 1]] &, idRules]]]
    ] /; (AssociationQ[columnNames] || ListQ[columnNames] || TrueQ[columnNames === Automatic]);


(*===========================================================*)
(* CrossTensorateSplit                                       *)
(*===========================================================*)

ClearAll[CrossTensorateSplit];

CrossTensorateSplit::nvar = "The second argument is expected to be a key in the first.";

CrossTensorateSplit[varName_] := CrossTensorateSplit[#, varName] &;

CrossTensorateSplit[xtens_Association, varName_] :=
    Block[{aVars = KeyDrop[xtens, "XTABTensor"], varInd, perm},
      If[! (MemberQ[Keys[xtens], varName] && (varName != "XTABTensor")),
        Message[CrossTensorateSplit::nvar]; Return[{}]
      ];
      varInd = Position[Keys[xtens], varName][[1, 1]] - 1;
      perm = Range[2, Length[aVars]];
      perm = Join[perm[[1 ;; varInd - 1]], {1}, perm[[varInd ;; -1]]];
      Association@
          MapThread[
            Rule[#1, Join[<|"XTABTensor" -> #2|>, KeyDrop[aVars, varName]]] &,
            {xtens[varName], # & /@ Transpose[xtens["XTABTensor"], perm]}]
    ];


(*===========================================================*)
(* ToAssociationTrie                                         *)
(*===========================================================*)

Clear[ToAssociationTrie];

ToAssociationTrie[ct_] :=
    Block[{},
      ToAssociationTrie[ <|"XTABTensor" -> ct["SparseMatrix"], 1 -> ct["RowNames"], 2 -> ct["ColumnNames"]|> ]
    ] /; AssociationQ[ct] && Length[ Intersection[ Keys[ct], {"SparseMatrix", "RowNames", "ColumnNames"} ] ] == 3;

ToAssociationTrie[ct_] :=
    Block[{dims, vals, i = -2},
      dims = Values[Rest[ct]];
      vals = Normal[ct["XTABTensor"]];
      Fold[
        Function[{val, dim},
          Map[AssociationThread[dim -> #] &, val, {i--}]
        ],
        vals,
        Reverse@dims]
    ] /; MatchQ[ct, Association["XTABTensor" -> _, __]];


(*===========================================================*)
(* CrossTabulate                                             *)
(*===========================================================*)

Clear[CrossTabulate];

SyntaxInformation[CrossTabulate] = {"Arguments" -> {_, OptionsPattern[]}};

Options[CrossTabulate] = {"Sparse" -> False};

CrossTabulate::narr = "The first argument is expected to be an array with two or three columns.
If present the third column is expected to be numerical.";

CrossTabulate[ data_Dataset, opts: OptionsPattern[] ] :=
    Block[{colKeys},
      colKeys = Normal[ data[[1]] ];
      If[ MatchQ[colKeys, _Association],
        CrossTabulate[ Normal[data[All, Values]], opts ],
        CrossTabulate[ Normal[data], opts ]
      ]
    ] /; Length[Dimensions[data]] == 2;

CrossTabulate[ arr_?MatrixQ, opts: OptionsPattern[] ] :=
    Block[{idRules, t},

      idRules = Table[(t = Union[arr[[All, i]]]; Dispatch@Thread[t -> Range[Length[t]]]), {i, Min[2, Dimensions[arr][[2]]]}];

      Which[
        Dimensions[arr][[2]] == 2,
        t = {
          SparseArray[ Map[ MapThread[ Replace, {#[[1]], idRules}] -> #[[2]] &, Tally[arr]]],
          Normal[#][[All, 1]]& /@ idRules
        },

        Dimensions[arr][[2]] == 3 && VectorQ[DeleteMissing[arr[[All, 3]]], NumericQ],
        t = {
          SparseArray[Map[MapThread[Replace, {#[[1]], idRules}] -> #[[2]] &, Map[{#[[1, 1 ;; 2]], Total[#[[All, 3]]]} &, GatherBy[arr, Most]]]],
          Normal[#][[All, 1]]& /@ idRules
        },

        True,
        Message[CrossTabulate::narr];
        Return[{}]
      ];

      If[ TrueQ[ OptionValue[CrossTabulate, "Sparse"] ],
        <| "SparseMatrix" -> t[[1]], "RowNames" -> t[[2, 1]], "ColumnNames" -> t[[2, 2]] |>,
        (* ELSE *)
        Dataset@AssociationThread[t[[2, 1]], AssociationThread[t[[2, 2]], #] & /@ Normal[t[[1]]]]
      ]
    ];


(*===========================================================*)
(* CrossTabulationMatrixQ                                    *)
(*===========================================================*)

Clear[CrossTabulationMatrixQ];

CrossTabulationMatrixQ[arg_Association] :=
    Length[Intersection[Keys[arg], {"SparseMatrix", "RowNames", "ColumnNames"}]] == 3 && MatrixQ[arg["SparseMatrix"]];

CrossTabulationMatrixQ[___] := False;


(*===========================================================*)
(* xtabsViaRLink                                             *)
(*===========================================================*)

Clear[xtabsViaRLink];
xtabsViaRLink::norlink = "R is not installed.";
xtabsViaRLink[data_?ArrayQ, columnNames : {_String ..}, formula_String, sparse : (False | True) : False] :=
    Block[{},
      If[Length[DownValues[RLink`REvaluate]] == 0,
        Message[xtabsViaRLink::norlink];
        Return[$Failed]
      ];
      RLink`RSet["data", Transpose[data]];
      If[ RLink`REvaluate["class(data)"][[1]] == "matrix",
        RLink`REvaluate["dataDF <- as.data.frame( t(data), stringsAsFactors=F )"],
        (*RLink`REvaluate["dataDF <- do.call( rbind.data.frame, data )"]*)
        (*RLink`REvaluate["dataDF <- data.frame( matrix( unlist(data), nrow = " <> ToString[Length[data]] <> ", byrow = T), stringsAsFactors=FALSE)"]*)
        RLink`REvaluate["dataDF <- as.data.frame( data, srtingsAsFactors=F )"]
      ];
      RLink`RSet["columnNames", columnNames];
      RLink`REvaluate["names(dataDF)<-columnNames"];
      RLink`REvaluate["xtabs(" <> formula <> ", dataDF, sparse = " <> If[sparse, "T", "F"] <> ")"]
    ];

Clear[FromRXTabsForm];
FromRXTabsForm[rres_RLink`RObject] :=
    Block[{},
      <|"SparseMatrix" -> rres[[1]],
        "RowNames" -> ("dimnames" /. rres[[2, 3]])[[1, 1]],
        "ColumnNames" -> ("dimnames" /. rres[[2, 3]])[[1, 2]]|>
    ] /; (! FreeQ[rres, {"xtabs", "table"}, Infinity]);


(*===========================================================*)
(* UpValues                                                  *)
(*===========================================================*)

Unprotect[Association];

MatrixForm[x_Association /; (KeyExistsQ[x, "SparseMatrix"] || KeyExistsQ[x, "XTABTensor"]), opts___] ^:=
    (MatrixForm[#1, Append[{opts}, TableHeadings -> Rest[{##}]]] & @@ x);

MatrixPlot[
  x_Association /; (KeyExistsQ[x, "SparseMatrix"] || KeyExistsQ[x, "XTABTensor"]), opts___] ^:=
    (MatrixPlot[#1,
      Append[{opts}, FrameLabel -> {{Keys[x][[2]], None}, {Keys[x][[3]], None}}]] & @@ x);


Transpose[x_Association /; (KeyExistsQ[x, "SparseMatrix"] || KeyExistsQ[x, "XTABTensor"]), args___] ^:=
    Block[{assoc = x},
      If[ KeyExistsQ[x, "SparseMatrix"],
        assoc["SparseMatrix"] = Transpose[x["SparseMatrix"], args],
        assoc["XTABTensor"] = Transpose[x["XTABTensor"], args]
      ];
      assoc["ColumnNames"] = x["RowNames"];
      assoc["RowNames"] = x["ColumnNames"];
      assoc
    ];


Protect[Association];

End[]; (* `Private` *)

EndPackage[]