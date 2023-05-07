# DataReshapers WL paclet

Wolfram Language (aka Mathematica) paclet for data reshaping functions, like, long- and wide form, cross tabulation, etc.

-------

## Usage examples

Here we get the Titanic dataset:

```mathematica
dsTitanic = ResourceFunction["ExampleDataset"][{"MachineLearning", "Titanic"}];
dsTitanic[1 ;; -1 ;; 400]
```

Here is a summary of the dataset:

```mathematica
RecordsSummary[dsTitanic]
```

Here is computed the contingency table for passenger survival vs class:

```mathematica
CrossTabulate[dsTitanic[All, {"passenger survival", "passenger class"}]]
```

Here the Titanic dataset is converted into long form:

```mathematica
LongFormDataset[dsTitanic]
```

Here is computed the contingency tensor for passenger survival vs class vs sex:

```mathematica
cols = {"passenger survival", "passenger class", "passenger sex"};
MatrixForm@
CrossTensorate[Count == Evaluate[(Plus @@ cols)], dsTitanic[All, cols]]
```

Here is a grid table form of a full integer array with "custom" column names:

```mathematica
SeedRandom[23];
GridTableForm[RandomInteger[100, {5, 7}], TableHeadings -> RandomWord[7]]
````

------- 


## References

### Articles

[AA1] Anton Antonov,
["Contingency tables creation examples"](https://mathematicaforprediction.wordpress.com/2016/10/04/contingency-tables-creation-examples/),
(2016),
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).

[Wk1] Wikipedia entry, [Contingency table](https://en.wikipedia.org/wiki/Contingency_table).

[Wk2] Wikipedia entry, [Wide and narrow data](https://en.wikipedia.org/wiki/Wide_and_narrow_data).

### Functions, repositories

[AAf1] Anton Antonov,
[CrossTabulate](https://resources.wolframcloud.com/FunctionRepository/resources/CrossTabulate),
(2019),
[Wolfram Function Repository](https://resources.wolframcloud.com/FunctionRepository).

[AAf2] Anton Antonov,
[LongFormDataset](https://resources.wolframcloud.com/FunctionRepository/resources/LongFormDataset),
(2020),
[Wolfram Function Repository](https://resources.wolframcloud.com/FunctionRepository).

[AAf3] Anton Antonov,
[WideFormDataset](https://resources.wolframcloud.com/FunctionRepository/resources/WideFormDataset),
(2021),
[Wolfram Function Repository](https://resources.wolframcloud.com/FunctionRepository).

[AAf4] Anton Antonov,
[RecordsSummary](https://resources.wolframcloud.com/FunctionRepository/resources/RecordsSummary),
(2019),
[Wolfram Function Repository](https://resources.wolframcloud.com/FunctionRepository).


### Videos

[AAv1] Anton Antonov,
["Multi-language Data-Wrangling Conversational Agent"](https://www.youtube.com/watch?v=pQk5jwoMSxs),
(2020),
[YouTube channel of Wolfram Research, Inc.](https://www.youtube.com/channel/UCJekgf6k62CQHdENWf2NgAQ).
(Wolfram Technology Conference 2020 presentation.)

[AAv2] Anton Antonov,
["Data Transformation Workflows with Anton Antonov, Session #1"](https://www.youtube.com/watch?v=iXrXMQdXOsM),
(2020),
[YouTube channel of Wolfram Research, Inc.](https://www.youtube.com/channel/UCJekgf6k62CQHdENWf2NgAQ).

[AAv3] Anton Antonov,
["Data Transformation Workflows with Anton Antonov, Session #2"](https://www.youtube.com/watch?v=DWGgFsaEOsU),
(2020),
[YouTube channel of Wolfram Research, Inc.](https://www.youtube.com/channel/UCJekgf6k62CQHdENWf2NgAQ).
