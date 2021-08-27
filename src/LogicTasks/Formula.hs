module LogicTasks.Formula
       (
         module Formula
       , Literal(..)
       , Clause
       , Cnf
       , Allocation
       , TruthValue(..)
       , getClauses
       , Formula(..)
       , genLiteral
       , genClause
       , genCnf
       , opposite
       , possibleAllocations
       ) where



import Types
import Formula