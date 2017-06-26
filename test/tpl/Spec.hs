import           Test.Hspec
import           TplLC

main :: IO ()
main = hspec $ do
  describe "TplLC" $ do
    it "works for id" $ do
      let term = App tmId tmTru
      eval term `shouldBe` tmTru

    it "works for a composite term" $ do
      let term = App (App tmTru tmTru) tmFls
      eval term `shouldBe` tmTru

-- λx. x
tmId = Abs (Var 0)
-- λt. λf. t
tmTru = Abs (Abs (Var 1))
-- λt. λf. f
tmFls = Abs (Abs (Var 0))
-- λb. λc. b c tru
tmAnd = Abs (Abs (App (App (Var 1) (Var 0)) tmFls))
-- λb. λc. b tru c
tmOr = Abs (Abs (App (App (Var 1) tmTru) (Var 0)))
-- λb. b fls tru
tmNot = Abs (App (App (Var 0) tmFls) tmTru)
