module UndeclaredPropInspection where

import Lexer
import Parser

inspectUndeclared :: Block -> ([Token], [Token])
inspectUndeclared tree = ([], visitBlock tree [])
    where
        visitBlock (Block cmds) scope = visitCommands cmds ([] : scope)

        visitCommands (Cmds cmds) scope = fst $ foldl (\(errs, scope') cmd -> 
            let (errProps, scope'') = visitCommand cmd scope'
            in (errs ++ errProps, scope'')) ([], scope) cmds

        visitCommand (GuardCmd guard commands) scope = 
            ((visitGuard guard scope) ++ (visitCommands commands scope), scope)
        visitCommand (ExprCmd expr) scope = (visitExpression expr scope, scope)
        visitCommand (ReturnCmd expr) scope = (visitExpression expr scope, scope)
        visitCommand (AssgCmd ref expr) scope = 
            let scope' = declareInScope ref scope
            in (visitExpression expr scope', scope')

        declareInScope (Ref stars name) scope =
            case not $ null stars of
                True -> scope 
                _ -> ((text name) : (head scope)) : (tail scope)

        visitGuard (Guard bools) scope = visitAll visitBoolExpr bools scope

        visitAll visitor rules scope = concat $ map (\rule -> visitor rule scope) rules

        visitBoolExpr (BoolEq e1 e2) scope = (visitExpression e1 scope) ++ (visitExpression e2 scope)

        visitExpression (AddExpr primary _ exprs) scope = (visitPrimary primary scope) ++ (visitAll visitExpression exprs scope)

        visitPrimary (BlockPrim b) scope = visitBlock b scope
        visitPrimary (ParensPrim e) scope = visitExpression e scope
        visitPrimary (RefPrim ref) scope = visitReference ref scope
        visitPrimary _ _ = []

        visitReference (Ref stars name) scope =
            case length stars >= length scope of
                True -> [name]
                _ -> if not $ elem (text name) (scope !! length stars) then [name] else []