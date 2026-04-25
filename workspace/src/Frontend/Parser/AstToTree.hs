module Frontend.Parser.AstToTree (astToTree) where

import Frontend.Syntax.SlSyntax
import Data.Tree
import Text.Printf (printf)



astToTree :: Sl -> Tree String
astToTree (Sl defs) = 
    Node "PROGRAMA (Sl)" (map defToTree defs)


defToTree :: Definition -> Tree String
defToTree (DStruct name fields) = 
    Node (printf "DStruct: %s" name) 
         [Node "Fields" (map fieldToTree fields)]

defToTree (Dfunc name typeVars params retType block) =
    Node (printf "Dfunc: %s" name) 
         ([Node "Generics" (map (\tv -> Node ("TypeVar: " ++ tv) []) typeVars)]
          ++ [Node "Params" (map paramToTree params)]
          ++ [Node "RetType" [maybe (Node "Void (Inferido)" []) typeToTree retType]]
          ++ [Node "Body" [blockToTree block]])

fieldToTree :: Field -> Tree String
fieldToTree (Field name t) = 
    Node (printf "Field: %s" name) [typeToTree t]

paramToTree :: Param -> Tree String
paramToTree (Param name mType) = 
    Node (printf "Param: %s" name) [maybe (Node "Tipo: Inferido" []) typeToTree mType]


typeToTree :: Type -> Tree String
typeToTree TInt = Node "TInt" []
typeToTree TFloat = Node "TFloat" []
typeToTree TString = Node "TString" []
typeToTree TBool = Node "TBool" []
typeToTree TVoid = Node "TVoid" []
typeToTree (TStruct name) = Node (printf "TStruct: %s" name) []
typeToTree (TVar name) = Node (printf "TVar (Generic): %s" name) []
typeToTree (TVector inner) = 
    Node "TVector (Dynamic Array)" [typeToTree inner]
typeToTree (TVectorN inner n) = 
    Node (printf "TVectorN (Static Array): Size %d" n) [typeToTree inner]
typeToTree (TFunc args ret) = 
    Node "TFunc" 
         ([Node "Args" (map typeToTree args)] 
          ++ [Node "Return" [typeToTree ret]])



blockToTree :: Block -> Tree String
blockToTree stmts = Node (printf "Block (%d Stmts)" (length stmts)) (map stmtToTree stmts)

stmtToTree :: Stmt -> Tree String
stmtToTree (SAssign lexp rexp) = 
    Node "SAssign" [Node "LHS" [expToTree lexp], Node "RHS" [expToTree rexp]]

stmtToTree (SLet var t mExp) = 
    Node (printf "SLet: %s" var) 
         ([typeToTree t] ++ maybe [] (pure . expToTree) mExp)

stmtToTree (SLetInfer var exp) = 
    Node (printf "SLetInfer: %s" var) [expToTree exp]
    
stmtToTree (SRead exp) = Node "SRead" [expToTree exp]
stmtToTree (SPrint exp) = Node "SPrint" [expToTree exp]

stmtToTree (SIf cond thenB elseB) = 
    Node "SIf" 
         [Node "Condition" [expToTree cond]
         , Node "Then Block" [blockToTree thenB]
         , Node "Else Block" [blockToTree elseB]]

stmtToTree (SWhile cond block) = 
    Node "SWhile" 
         [Node "Condition" [expToTree cond]
         , Node "Body" [blockToTree block]]

stmtToTree (SFor initS cond stepS block) = 
    Node "SFor" 
         [Node "Init" [stmtToTree initS]
         , Node "Condition" [expToTree cond]
         , Node "Step" [stmtToTree stepS]
         , Node "Body" [blockToTree block]]

stmtToTree (SReturn exp) = 
    Node "SReturn" [expToTree exp]

stmtToTree (SExpr exp) = 
    Node "SExpr" [expToTree exp]


expToTree :: Exp -> Tree String
expToTree (EValue v) = Node (printf "EValue: %s" (valueToString v)) []
expToTree (EVar name) = Node (printf "EVar: %s" name) []

expToTree (EVector exps) = 
    Node (printf "EVector (Size %d)" (length exps)) (map expToTree exps)
expToTree (EStruct name exps) = 
    Node (printf "EStruct: %s" name) (map expToTree exps)
expToTree (EIndex arr idx) = 
    Node "EIndex (Arr[Idx])" [Node "Array" [expToTree arr], Node "Index" [expToTree idx]]
expToTree (EField obj field) = 
    Node (printf "EField: .%s" field) [expToTree obj]
expToTree (EArraySize exp) = 
    Node "EArraySize: .size" [expToTree exp]
expToTree (ENew t size) = 
    Node "ENew" [Node "Type" [typeToTree t], Node "Size" [expToTree size]]

expToTree (ECall func args) = 
    Node "ECall" 
         ([Node "Function" [expToTree func]] 
          ++ [Node "Arguments" (map expToTree args)])
expToTree (EIncrement exp) = 
    Node "EIncrement (PostFix ++)" [expToTree exp]
expToTree (ENot exp) = 
    Node "ENot (!)" [expToTree exp]
expToTree (EMinus exp) = 
    Node "EMinus (Unary -)" [expToTree exp]

expToTree (l :+: r) = binOpToTree "+" l r
expToTree (l :-: r) = binOpToTree "-" l r
expToTree (l :*: r) = binOpToTree "*" l r
expToTree (l :/: r) = binOpToTree "/" l r
expToTree (l :%: r) = binOpToTree "%" l r
expToTree (l :&&: r) = binOpToTree "&&" l r
expToTree (l :||: r) = binOpToTree "||" l r
expToTree (l :<: r) = binOpToTree "<" l r
expToTree (l :>: r) = binOpToTree ">" l r
expToTree (l :<=: r) = binOpToTree "<=" l r
expToTree (l :>=: r) = binOpToTree ">=" l r
expToTree (l :==: r) = binOpToTree "==" l r
expToTree (l :!=: r) = binOpToTree "!=" l r

binOpToTree :: String -> Exp -> Exp -> Tree String
binOpToTree op l r = 
    Node (printf "BinOp: %s" op) [Node "LHS" [expToTree l], Node "RHS" [expToTree r]]

valueToString :: Value -> String
valueToString (VInt n) = printf "Int: %d" n
valueToString (VFloat f) = printf "Float: %f" f
valueToString (VString s) = printf "String: \"%s\"" s
valueToString (VBool b) = printf "Bool: %s" (show b)