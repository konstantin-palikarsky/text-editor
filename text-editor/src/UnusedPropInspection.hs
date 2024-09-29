module UnusedPropInspection where

import Lexer
import Parser


inspectUnused :: Block -> ([Token], [Token])
inspectUnused tree =
    let assgs = findAllAssgs tree
        unusedProps = map (fst) $ filter (\(prop, parent) -> not $ isPropUsed prop parent 0) assgs
    in
        (unusedProps, [])



findAllAssgs :: Block -> [(Token, Block)]
findAllAssgs tree = visitBlock tree
    where
        visitBlock parent@(Block commands) = visitCommands commands parent

        visitCommands (Cmds cmds) parent = visitAll visitCommand cmds parent

        visitAll visitor rules parent = concat $ map (\rule -> visitor rule parent) rules

        visitCommand (GuardCmd guard commands) parent = (visitGuard guard parent) ++ (visitCommands commands parent)
        visitCommand (ExprCmd expr) parent = visitExpression expr parent
        visitCommand (ReturnCmd expr) parent = visitExpression expr parent
        visitCommand (AssgCmd ref expr) parent = (visitReference ref parent) ++ (visitExpression expr parent)

        visitGuard (Guard bools) parent = visitAll visitBoolExpr bools parent

        visitBoolExpr (BoolEq e1 e2) parent = (visitExpression e1 parent) ++ (visitExpression e2 parent)

        visitExpression (AddExpr primary _ exprs) parent = (visitPrimary primary parent) ++ (visitAll visitExpression exprs parent)

        visitPrimary (BlockPrim b) _ = visitBlock b
        visitPrimary (ParensPrim e) parent = visitExpression e parent
        visitPrimary _ _ = []

        visitReference (Ref stars name) parent =
            case null stars of True -> [(name, parent)]
                               _ -> []

isPropUsed :: Token -> Block -> Int -> Bool
isPropUsed p scope nestingLevel = visitBlock p scope nestingLevel
    where
        visitBlock prop (Block commands) nesting = visitCommands prop commands nesting

        visitCommands prop (Cmds cmds) nesting = any (\c -> visitCommand prop c nesting) cmds

        visitCommand prop (GuardCmd guard commands) nesting = (visitGuard prop guard nesting) || (visitCommands prop commands nesting)
        visitCommand prop (ExprCmd expr) nesting = visitExpression prop expr nesting
        visitCommand prop (ReturnCmd expr) nesting = visitExpression prop expr nesting
        visitCommand prop (AssgCmd _ expr) nesting = visitExpression prop expr nesting

        visitGuard prop (Guard bools) nesting = any (\b -> visitBoolExpr prop b nesting) bools

        visitBoolExpr prop (BoolEq e1 e2) nesting = (visitExpression prop e1 nesting) || (visitExpression prop e2 nesting)

        visitExpression prop (AddExpr primary _ exprs) nesting = (visitPrimary prop primary nesting) || (any (\e -> visitExpression prop e nesting) exprs)

        visitPrimary prop (BlockPrim b) nesting = visitBlock prop b (nesting + 1)
        visitPrimary prop (RefPrim ref) nesting = visitReference prop ref nesting
        visitPrimary prop (ParensPrim e) nesting = visitExpression prop e nesting
        visitPrimary _ _ _ = False

        visitReference prop (Ref stars ref) nesting = 
            (length stars == nesting) && 
            isAfter ref prop &&
            (text prop == text ref)