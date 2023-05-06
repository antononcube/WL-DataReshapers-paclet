BeginPackage["AntonAntonov`DataReshapers`GridTableForm`"];

(*GridTableForm::usage="GridTableForm[arg] arranges the elements of arg in a two-dimensional grid.";*)

Begin["`Private`"];

Needs["AntonAntonov`DataReshapers`"];

Clear[GridTableForm];

GridTableForm::nargs = "The first argument is expected to be a list or an association.";
GridTableForm::nthr = "The value of the option \"TableHeadings\" is expected to be a list, Automatic, or None.";

SyntaxInformation[GridTableForm] = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

Options[GridTableForm] =
    Join[{
      "TableHeadings" -> Automatic,
      "TableHeadingsStyle" -> {Blue, FontFamily -> "Times"},
      "RowBackground" -> {White, GrayLevel[0.96]}},
      Options[Grid]
    ];

GridTableForm[data_Association, opts : OptionsPattern[]] :=
    GridTableForm[Values[data], opts, "TableHeadings" -> Keys[data]];

GridTableForm[data_List, opts : OptionsPattern[]] :=
    Block[{headingsStyle, contrastingColorsPair, rowNames, gridHeadings,
      gridData, dataVecQ = False},

      headingsStyle = OptionValue[GridTableForm, "TableHeadingsStyle"];
      contrastingColorsPair = OptionValue[GridTableForm, "RowBackground"];
      gridHeadings = OptionValue[GridTableForm, "TableHeadings"];

      If[AtomQ[contrastingColorsPair] || TrueQ[Head[contrastingColorsPair] === RGBColor],
        contrastingColorsPair = {contrastingColorsPair, contrastingColorsPair}
      ];

      gridData = data;

      If[VectorQ[data],
        dataVecQ = True;
        gridData = List@data
      ];

      (* Headings *)
      Which[
        TrueQ[gridHeadings === None],
        {rowNames, gridHeadings} = {Automatic, Automatic},


        TrueQ[gridHeadings === Automatic],
        {rowNames, gridHeadings} = {Automatic, Automatic},

        MatchQ[gridHeadings, {_List | Automatic | None}],
        {rowNames, gridHeadings} = {gridHeadings[[1]], Automatic},

        MatchQ[gridHeadings, {_List | None | Automatic, _List | None | Automatic}],
        rowNames = gridHeadings[[1]];
        gridHeadings = gridHeadings[[2]],

        ListQ[gridHeadings],
        rowNames = Automatic,

        True,
        {rowNames, gridHeadings} = {Automatic, Automatic}
      ];

      If[TrueQ[rowNames === Automatic] || TrueQ[rowNames === None],
        rowNames = Range[Length[gridData]]
      ];

      Which[
        Length[rowNames] < Length[gridData],
        rowNames = Join[rowNames, Table[SpanFromAbove, Length[gridData] - Length[rowNames]]],

        Length[rowNames] > Length[gridData],
        rowNames = Take[rowNames, Length[gridData]]
      ];

      gridData = Map[Join[#, Table["", {Max[Length /@ gridData] - Length[#]}]] &, gridData];
      gridData = MapThread[Prepend, {gridData, rowNames}];

      Which[
        TrueQ[gridHeadings === None] || TrueQ[gridHeadings === Automatic],
        gridHeadings = Join[{"#"}, Range[1, Length[gridData[[1]]] - 1]],

        ListQ[gridHeadings],
        gridHeadings = Join[{"#"}, gridHeadings],

        True,
        Message[GridTableForm::nthr];
        gridHeadings = Join[{"#"}, Range[1, Length[gridData[[1]]] - 1]]
      ];

      gridHeadings = Map[Style[#, Sequence @@ Flatten[{headingsStyle}]] &, gridHeadings];

      Which[
        Length[gridHeadings] < Length[gridData[[1]]],
        gridHeadings = Append[gridHeadings, SpanFromLeft],

        Length[gridHeadings] > Length[gridData[[1]]],
        gridHeadings = Take[gridHeadings, Length[gridData[[1]]]]
      ];

      (* Final grid data *)
      gridData = Prepend[gridData, gridHeadings];

      Grid[gridData,
        FilterRules[{opts}, Options[Grid]],
        Alignment -> Left,
        Dividers -> {Join[{1 -> Black, 2 -> Black},
          Thread[Range[3, Length[gridData[[2]]] + 1] ->
              GrayLevel[0.8]], {Length[gridData[[2]]] + 1 -> Black}], {True,
          True, {False}, True}},
        Background -> {Automatic,
          If[EvenQ[Length[gridData]], #, Append[#, contrastingColorsPair[[1]]]] &@
              Flatten[Table[contrastingColorsPair, {Length[gridData] / 2}]]}
      ]
    ];

GridTableForm[ds_Dataset] :=
    Which[
      AssociationQ[Normal[ds]] && AssociationQ[Normal[ds][[1]]],
      GridTableForm[Normal@ds[Values, Values], TableHeadings -> Normal[Keys[ds[[1]]]]],

      ListQ[Normal[ds]] && AssociationQ[Normal[ds][[1]]],
      GridTableForm[Normal@ds[Values], TableHeadings -> Normal[Keys[ds[[1]]]]],

      True,
      GridTableForm[Normal@ds]
    ];

GridTableForm[___] :=
    Block[{},
      Message[GridTableForm::nargs];
      $Failed
    ];

End[];

EndPackage[]