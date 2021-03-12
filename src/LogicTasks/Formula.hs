module LogicTasks.Formula
       (
         module Formula
       , Literal(..)
       , Clause
       , Cnf
       , Allocation
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