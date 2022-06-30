module SynTreeSpec where
import Test.Hspec
import Test.QuickCheck
import Types ( depwinode, genSynTree )
import Parsing ( normParse )
import Data.Char (isLetter)
import qualified Control.Exception as Exc (evaluate)
import Data.Maybe 
-- chooseletter :: Bool -> Char ->Bool
-- chooseletter False _ = False
-- chooseletter _ t =isLetter t
invalidBoundsSyntr :: Gen ((Integer,Integer),Integer,[Char])
invalidBoundsSyntr = do
    validChars <- sublistOf ['A'..'Z']
    minnode <- chooseInt (2,100)
    maxnode <- chooseInt (1,minnode-1)
    maxdepth <- chooseInt (fromInteger $ fst(depwinode $toInteger  minnode),maxnode)
    pure ((toInteger minnode,toInteger maxnode), toInteger maxdepth ,validChars)

validBoundsSyntr :: Gen ((Integer,Integer),Integer,[Char])
validBoundsSyntr = do
    validChars <- sublistOf ['A'..'Z']
    minnode <- chooseInt (1,100)
    maxnode <- chooseInt (minnode,100)
    maxdepth <- chooseInt (fromInteger $ fst(depwinode $toInteger  minnode),maxnode)
    pure ((toInteger minnode,toInteger maxnode), toInteger maxdepth ,validChars)

spec :: Spec
spec = do
    describe "genSyntaxTree" $ do
        it "should generate a random SyntaxTree from the given parament and can be parsed by normParse" $
            forAll validBoundsSyntr $ \((minnode,maxnode), maxdepth ,validChars)->forAll ( genSynTree (minnode,maxnode) maxdepth validChars)  $ \synTree -> normParse (show (fromJust synTree))==Right  (fromJust synTree)
        it "should throw an error call" $
            forAll invalidBoundsSyntr $ \((minnode,maxnode), maxdepth ,validChars)->forAll ( genSynTree (minnode,maxnode) maxdepth validChars)  $ \synTree -> synTree==Nothing
            
    