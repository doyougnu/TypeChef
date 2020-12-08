package de.fosd.typechef.featureexpr.sat

object VSATMode extends Enumeration {
    type VSATMode = Value
    val Unknown, ArgParse, Lexing, Parsing, TypeChecking = Value
}