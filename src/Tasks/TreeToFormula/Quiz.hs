{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.TreeToFormula.Quiz(
    generateTreeToFormulaInst,
    ) where


import Trees.Generate (genSynTree)
import Test.QuickCheck (Gen,)

import Tasks.TreeToFormula.Config (TreeToFormulaConfig(..), TreeToFormulaInst(..))
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Print (transferToPicture, display)




generateTreeToFormulaInst :: TreeToFormulaConfig -> Gen TreeToFormulaInst
generateTreeToFormulaInst TreeToFormulaConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- genSynTree
        (minNodes, maxNodes)
        maxDepth
        usedLiterals
        atLeastOccurring
        allowArrowOperators
        maxConsecutiveNegations
        minUniqueBinOperators
    return $ TreeToFormulaInst
      { tree
      , latexImage = transferToPicture tree
      , correct = display tree
      , addExtraHintsOnSemanticEquivalence = extraHintsOnSemanticEquivalence
      , addText = extraText
      , showSolution = printSolution
      }
