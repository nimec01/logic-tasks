module LogicTasks.Formula
       (
         module Formula
       , Literal(..)
       , Clause
       , Con
       , Cnf
       , Dnf
       , Allocation
       , TruthValue(..)
       , PrologLiteral
       , PrologClause
       , terms
       , getClauses
       , getConjunctions
       , Formula(..)
       , genLiteral
       , genClause
       , genCon
       , genCnf
       , genDnf
       , opposite
       , possibleAllocations
       ) where



import Types
import Formula