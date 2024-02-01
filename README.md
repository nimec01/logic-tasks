# logic-tasks [![Haskell CI](https://github.com/fmidue/logic-tasks/actions/workflows/haskell.yml/badge.svg)](https://github.com/fmidue/logic-tasks/actions/workflows/haskell.yml)

## Mapping from Autotool to relevant modules in this repo

| Autotool inventory | Direct | Quiz | Autotool module | `logic-tasks` module(s) |
| :-- | :-: | :-: | :-- | :-- |
| Aussagenlogik/Syntax/LogicInvalidCnfs | | x |  `Logic.Syntax.LegalCnf` | `LogicTasks.Syntax.IllegalCnfs` + `Tasks.LegalCNF.Quiz` |
| Aussagenlogik/Syntax/LogicInvalidFormulas | | x |  `Logic.Syntax.LegalFormula` | `LogicTasks.Syntax.IllegalFormulas` + `Tasks.LegalProposition.Quiz` |
| Aussagenlogik/Syntax/LogicRemoveBrackets | | x |  `Logic.Syntax.SimplestFormula` | `LogicTasks.Syntax.SimplestFormula` + `Tasks.SuperfluousBrackets.Quiz` |
| Aussagenlogik/Syntax/LogicSubformulas | | x |  `Logic.Syntax.SubFormula` | `LogicTasks.Syntax.SubTreeSet` + `Tasks.SubTree.Quiz` |
| Aussagenlogik/Syntax/LogicTreeToFormula | | x |  `Logic.Syntax.TreeToFormula` | `LogicTasks.Syntax.TreeToFormula` + `Tasks.TreeToFormula.Quiz` |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableFillGaps | x | x |  `Logic.Semantics.FillGaps` | `LogicTasks.Semantics.Fill` |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableChooseForFormula | x | x |  `Logic.Semantics.ChooseTable` | `LogicTasks.Semantics.Pick` |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableFindMistakes | x | x |  `Logic.Semantics.FindMistakes` | `LogicTasks.Semantics.Decide` |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableMaxterm | x | x |  `Logic.Semantics.MaxTerm` | `LogicTasks.Semantics.Max` |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableMinTerm | x | x |  `Logic.Semantics.MinTerm` | `LogicTasks.Semantics.Min` |
| Aussagenlogik/Semantik/Resolution/LogicResolutionStep | x | x |  `Logic.Semantics.ResolutionStep` | `LogicTasks.Semantics.Step ` |
| Aussagenlogik/Semantik/Resolution/LogicResolutionComplete | x | x |  `Logic.Semantics.ResolutionFull` | `LogicTasks.Semantics.Resolve` |
| Aussagenlogik/Semantik/Resolution/PrologResolutionStep | x | x |  `Logic.Semantics.ResolutionStepProlog` | `LogicTasks.Semantics.Prolog` |
