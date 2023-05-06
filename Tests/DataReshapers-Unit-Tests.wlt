(* :Title: DataReshapers-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2020-08-22 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Long format, Long form, Narrow format, Narrow form, Wide format, Wide form, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has unit tests of originally implemented in:

     https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/DataReshape-Unit-Tests.wlt

*)

BeginTestSection["DataReshapers-Unit-Tests.wlt"];

(***********************************************************)
(* Load package                                            *)
(***********************************************************)

VerificationTest[(* 1 *)
  Needs["AntonAntonov`DataReshapers`"];
  Greater[Length[DownValues[AntonAntonov`DataReshape`LongFormDataset]], 0]
  ,
  True
  ,
  TestID -> "LoadPaclet"
];


(***********************************************************)
(* Generate data                                           *)
(***********************************************************)

VerificationTest[
  SeedRandom[1295];

  dsSmall = Dataset @ {
    <|"a" -> "x", "b" -> 5|>,
    <|"a" -> "y", "b" -> 6|>,
    <|"a" -> "x", "b" -> 10|>,
    <|"a" -> "y", "b" -> 100|>,
    <|"a" -> "z", "b" -> Missing[]|>};

  k = 1;
  dsSmallWithIDs = dsSmall[All, Prepend[#, "ID" -> k++] &];

  SeedRandom[4];
  n = 200;
  dsLarge = Dataset @ Transpose[{RandomInteger[5, n], RandomChoice[RandomWord[5], n], RandomChoice[RandomWord[20], n], RandomReal[{-100, 100}, n]}];

  k = 1;
  dsLargeWithIDs = dsLarge[All, Prepend[#, k++] &];

  aSmall = AssociationThread[Map[#ID &, Normal@dsSmallWithIDs], Map[KeyDrop[#, "ID"] &, Normal@dsSmallWithIDs]];

  aLarge = AssociationThread[Map[First, Normal@dsLargeWithIDs], Map[Rest, Normal@dsLargeWithIDs]];

  dsAnscombe = Dataset[ExampleData[{"Statistics", "AnscombeRegressionLines"}]][All, AssociationThread[ExampleData[{"Statistics", "AnscombeRegressionLines"}, "ColumnHeadings"], #] &];

  Apply[ And, Map[ MatchQ[#, _Dataset]&, {dsSmall, dsSmallWithIDs, dsLarge, dsLargeWithIDs, dsAnscombe} ] ] &&
      AssociationQ[aSmall] &&
      AssociationQ[aLarge]
  ,
  True
  ,
  TestID -> "Generated-datasets"
];


(***********************************************************)
(* Get built in data                                       *)
(***********************************************************)

VerificationTest[
  dsTitanic = ExampleData[{"Dataset", "Titanic"}];
  MatchQ[dsTitanic, _Dataset] && Dimensions[dsTitanic] == {1309, 4}
  ,
  True
  ,
  TestID -> "Titanic-dataset"
];


(***********************************************************)
(* To long form                                            *)
(***********************************************************)

VerificationTest[
  dsTemp = LongFormDataset[dsSmall];
  MatchQ[ dsTemp, _Dataset] &&
      Dimensions[dsTemp] == { 2 * Dimensions[dsSmall][[1]], 3 } &&
      Normal[ Keys[dsTemp[[1]]] ] == { "AutomaticKey", "Variable", "Value" }
  ,
  True
  ,
  TestID -> "LongFormDataset-1"
];


VerificationTest[
  dsTemp = LongFormDataset[dsSmall, Automatic, {"a", "b"} ];
  dsTemp2 = LongFormDataset[dsSmall, "IdentifierColumns" -> Automatic, "VariableColumns" -> {"a", "b"} ];
  MatchQ[ dsTemp2, _Dataset] &&
      Dimensions[dsTemp2] == { 2 * Dimensions[dsSmall][[1]], 3 } &&
      Normal[ Keys[dsTemp2[[1]]] ] == { "AutomaticKey", "Variable", "Value" } &&
      dsTemp == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-2"
];


VerificationTest[
  dsTemp = LongFormDataset[dsSmallWithIDs, "ID", {"a", "b"}];
  MatchQ[ dsTemp, _Dataset] &&
      Dimensions[dsTemp] == { 2 * Dimensions[dsSmallWithIDs][[1]], 3 } &&
      Normal[ Keys[dsTemp[[1]]] ] == { "ID", "Variable", "Value" }
  ,
  True
  ,
  TestID -> "LongFormDataset-3"
];


VerificationTest[
  dsTemp = LongFormDataset[dsSmallWithIDs, "ID", {"a", "b"}];
  dsTemp2 = LongFormDataset[dsSmallWithIDs, "IdentifierColumns" -> "ID", "VariableColumns" -> {"a", "b"} ];
  MatchQ[ dsTemp, _Dataset] &&
      Dimensions[dsTemp] == { 2 * Dimensions[dsSmallWithIDs][[1]], 3 } &&
      Normal[ Keys[dsTemp[[1]]] ] == { "ID", "Variable", "Value" } &&
      dsTemp == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-4"
];


VerificationTest[
  mat = RandomReal[{100, 200}, {4, 7}];
  dsTemp = LongFormDataset[mat];
  MatchQ[ dsTemp, _Dataset] &&
      Dimensions[dsTemp] == { Times @@ Dimensions[mat], 3 } &&
      Normal[ Keys[dsTemp[[1]]] ] == { "AutomaticKey", "Variable", "Value" }
  ,
  True
  ,
  TestID -> "LongFormDataset-5"
];


(***********************************************************)
(* To long form equivalences                               *)
(***********************************************************)

VerificationTest[
  dsTemp1 = LongFormDataset[dsSmall];
  dsTemp2 = LongFormDataset[dsSmall, Automatic, Automatic];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Automatic-Equivalence-1"
];


VerificationTest[
  vInds = Range[ 2, Dimensions[dsSmallWithIDs][[2]] ];
  dsTemp1 = LongFormDataset[ dsSmallWithIDs, 1, vInds ];
  dsTemp2 = LongFormDataset[ dsSmallWithIDs, Automatic, vInds];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Automatic-Equivalence-2"
];


VerificationTest[
  vInds = Range[ 1, Dimensions[dsSmallWithIDs][[2]] ];
  dsTemp1 = LongFormDataset[ dsSmallWithIDs, 0, vInds ];
  dsTemp2 = LongFormDataset[ dsSmallWithIDs, Automatic, vInds];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Automatic-Equivalence-3"
];


VerificationTest[
  vInds = Range[ 2, Dimensions[dsSmallWithIDs][[2]] ];
  dsTemp1 = LongFormDataset[ dsSmallWithIDs, 1, vInds ];
  dsTemp2 = LongFormDataset[ dsSmallWithIDs, 1, Automatic ];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Automatic-Equivalence-4"
];


VerificationTest[
  vInds = Range[ 1, Dimensions[dsSmallWithIDs][[2]] ];
  dsTemp1 = LongFormDataset[ dsSmallWithIDs, 0, vInds ];
  dsTemp2 = LongFormDataset[ dsSmallWithIDs, 0, Automatic ];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Automatic-Equivalence-5"
];


VerificationTest[
  dsTemp1 = LongFormDataset[ dsSmallWithIDs, "ID" ];
  dsTemp2 = LongFormDataset[ dsSmallWithIDs, 1, Automatic ];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Automatic-Equivalence-6"
];


VerificationTest[
  dsTemp1 = LongFormDataset[ dsSmallWithIDs, "ID" ];
  dsTemp2 = LongFormDataset[ dsSmallWithIDs, "ID", Automatic ];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Automatic-Equivalence-7"
];


VerificationTest[
  dsTemp1 = LongFormDataset[ Normal @ dsSmallWithIDs, "ID" ];
  dsTemp2 = LongFormDataset[ dsSmallWithIDs, "ID" ];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Automatic-Equivalence-8"
];


(***********************************************************)
(* To long form options                                    *)
(***********************************************************)

VerificationTest[
  dsTemp1 = LongFormDataset[ dsSmallWithIDs, "IdentifierColumns" -> "ID" ];
  dsTemp2 = LongFormDataset[ dsSmallWithIDs, "ID" ];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Options-1"
];


VerificationTest[
  dsTemp1 = LongFormDataset[ dsSmallWithIDs, "IdentifierColumns" -> { "ID" } ];
  dsTemp2 = LongFormDataset[ dsSmallWithIDs, "ID" ];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Options-2"
];


VerificationTest[
  dsTemp1 = LongFormDataset[ dsSmallWithIDs, "VariableColumns" -> { "a", "b"} ];
  dsTemp2 = LongFormDataset[ dsSmallWithIDs, Automatic, {"a", "b"} ];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "LongFormDataset-Options-3"
];


VerificationTest[
  dsTemp1 = LongFormDataset[ dsSmallWithIDs, "AutomaticKeysTo" -> "SpecialID" ];
  First[ Keys @ dsTemp1[1] ] == "SpecialID"
  ,
  True
  ,
  TestID -> "LongFormDataset-Options-4"
];


(***********************************************************)
(* To long form failure                                    *)
(***********************************************************)


VerificationTest[
  LongFormDataset[ dsSmallWithIDs, "blah", {"a", "b"}]
  ,
  $Failed
  ,
  {LongFormDataset::colkeys}
  ,
  TestID -> "LongFormDataset-fail-1"
];


VerificationTest[
  LongFormDataset[ dsSmallWithIDs, "ID", {"blah", "b"}]
  ,
  $Failed
  ,
  {LongFormDataset::colkeys}
  ,
  TestID -> "LongFormDataset-fail-2"
];


VerificationTest[
  LongFormDataset[ dsLargeWithIDs, "ID", {"a", "b"}]
  ,
  $Failed
  ,
  {LongFormDataset::nocolkeys}
  ,
  TestID -> "LongFormDataset-fail-3"
];


(***********************************************************)
(* To wide form                                            *)
(***********************************************************)

VerificationTest[
  MatchQ[ WideFormDataset[LongFormDataset[dsSmall], "AutomaticKey", "Variable", "Value"], _Dataset ]
  ,
  True
  ,
  TestID -> "WideFormDataset-1"
];


VerificationTest[
  dsTemp = WideFormDataset[LongFormDataset[dsSmall], "AutomaticKey", "Variable", "Value"];
  dsTemp[All, {2,3}] == dsSmall
  ,
  True
  ,
  TestID -> "WideFormDataset-2"
];


VerificationTest[
  dsTemp = WideFormDataset[LongFormDataset[dsSmallWithIDs, "ID", Automatic], "ID", "Variable", "Value"];
  dsTemp == dsSmallWithIDs
  ,
  True
  ,
  TestID -> "WideFormDataset-3"
];


(***********************************************************)
(* Separate column                                         *)
(***********************************************************)

VerificationTest[
  dsTemp = LongFormDataset[dsAnscombe];
  dsRes = SeparateColumn[dsTemp, "Variable", {"Variable", "Set"}, "Separator" -> ""];
  MatchQ[ dsRes, _Dataset] &&
      AssociationQ[Normal[dsRes[1]]] &&
      Sort[ Keys[Normal[dsRes[1]]] ] == Union[ Append[ Keys[Normal[dsTemp[1]]], "Set"] ]
  ,
  True
  ,
  TestID -> "SeparateColumn-1"
];


VerificationTest[
  dsTemp = LongFormDataset[dsAnscombe];
  dsTemp2 = ReplacePart[dsTemp, {2, 2} -> "Z"];
  dsRes = SeparateColumn[dsTemp, "Variable", {"Variable", "Set"}, "Separator" -> ""];
  MatchQ[ dsRes, _Dataset] &&
      AssociationQ[Normal[dsRes[1]]] &&
      Sort[ Keys[Normal[dsRes[1]]] ] == Union[ Append[ Keys[Normal[dsTemp2[1]]], "Set"] ]
  ,
  True
  ,
  TestID -> "SeparateColumn-2"
];


VerificationTest[
  dsTemp = LongFormDataset[dsAnscombe];
  dsTemp2 = ReplacePart[dsTemp, {2, 2} -> "ZZZ"];
  dsRes = SeparateColumn[dsTemp, "Variable", {"Variable", "Set"}, "Separator" -> ""];
  MatchQ[ dsRes, _Dataset] &&
      AssociationQ[Normal[dsRes[1]]] &&
      Sort[ Keys[Normal[dsRes[1]]] ] == Union[ Append[ Keys[Normal[dsTemp2[1]]], "Set"] ]
  ,
  True
  ,
  TestID -> "SeparateColumn-3"
];


VerificationTest[
  dsTemp = LongFormDataset[dsAnscombe];
  dsTemp2 = ReplacePart[dsTemp, {2, 2} -> "ZZZ"];
  dsRes = SeparateColumn[dsTemp, "Variable", {"Axis", "Set"}, "Separator" -> "", "RemoveInputColumn" -> True ];
  MatchQ[ dsRes, _Dataset] &&
      AssociationQ[Normal[dsRes[1]]] &&
      Sort[ Keys[Normal[dsRes[1]]] ] == Union[ Complement[ Join[ Keys[Normal[dsTemp2[1]]], {"Axis", "Set"}], {"Variable"} ] ]
  ,
  True
  ,
  TestID -> "SeparateColumn-4"
];


VerificationTest[
  dsTemp = LongFormDataset[dsAnscombe];
  dsTemp2 = ReplacePart[dsTemp, {2, 2} -> "ZZZ"];
  dsRes = SeparateColumn[dsTemp, "Variable", {"Axis", "Set"}, "Separator" -> "", "RemoveInputColumn" -> False ];
  MatchQ[ dsRes, _Dataset] &&
      AssociationQ[Normal[dsRes[1]]] &&
      Sort[ Keys[Normal[dsRes[1]]] ] == Union[ Join[ Keys[Normal[dsTemp2[1]]], {"Axis", "Set"}] ]
  ,
  True
  ,
  TestID -> "SeparateColumn-5"
];


VerificationTest[
  dsTemp = LongFormDataset[dsAnscombe];
  dsTemp0 = dsTemp[Values];
  dsRes = SeparateColumn[dsTemp0, 2, {"Variable", "Set"}, "Separator" -> ""];
  MatchQ[ dsRes, _Dataset] &&
      ListQ[Normal[dsRes[1]]] &&
      Dimensions[dsRes][[2]] ==  Dimensions[dsTemp0][[2]] + 1
  ,
  True
  ,
  TestID -> "SeparateColumn-6"
];


VerificationTest[
  dsTemp = LongFormDataset[dsAnscombe];
  dsTemp0 = dsTemp[Values];
  dsRes = SeparateColumn[dsTemp0, 2, {"Variable", "Set"}, "Separator" -> "",  "RemoveInputColumn" -> False ];
  MatchQ[ dsRes, _Dataset] &&
      ListQ[Normal[dsRes[1]]] &&
      Dimensions[dsRes][[2]] ==  Dimensions[dsTemp0][[2]] + 2
  ,
  True
  ,
  TestID -> "SeparateColumn-7"
];



EndTestSection[]

