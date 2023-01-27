BeginPackage["ChristopherWolfram`ForeignFunctionInterface`Messages`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


LoadExternalLibrary::cannotopen = "Failed to open external library with message: \"`1`\"";

OpaqueRawPointer::invaddress = "Expected an integer representing an address in memory, but found `1` instead in OpaqueRawPointer.";

RawPointer::invaddress = "Expected an integer representing an address in memory, but found `1` instead in RawPointer.";

ForeignFunction::invtype = "Expected a function type, but found `1` instead in ForeignFunction.";


EndPackage[]
