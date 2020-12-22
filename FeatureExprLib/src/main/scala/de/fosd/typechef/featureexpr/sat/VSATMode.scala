package de.fosd.typechef.featureexpr.sat

object VSATMode extends Enumeration {
    type VSATMode = Value
    val Unknown
    , ArgParse
    , LoadFM
    , LoadAST
    , Serialize
    , Lexing
    , Parsing
    , TypeSystemInit
    , TypeChecking
    , Interfaces
    , WriteInterfaces
    , CallGraph
    , StaticAnalysis = Value
}