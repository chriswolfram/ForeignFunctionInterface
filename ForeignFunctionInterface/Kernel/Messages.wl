BeginPackage["ChristopherWolfram`ForeignFunctionInterface`Messages`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


ForeignFunction::invlib = "Library `1` not found.";

OpaqueRawPointer::invaddress = "Expected an integer representing an address in memory, but found `1` instead in OpaqueRawPointer.";

RawPointer::invaddress = "Expected an integer representing an address in memory, but found `1` instead in RawPointer.";

ForeignFunction::invtype = "Expected a function type, but found `1` instead in ForeignFunction.";

ManagedExpression::inv = "Expected an expression to manage and a freeing function in ManagedExpression, but found `1` instead.";

GetManagedExpression::invManagedExpr = "Expected a ManagedExpression, but found `1` instead."


EndPackage[]
