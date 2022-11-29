BeginPackage["ForeignFunctionInterface`"]


CreateManagedExpression
GetManagedExpression

CreateCArray
CArrayToNumericArray
StringToCArray
CArrayToString
FreeCArray

LoadExternalLibrary
UnloadExternalLibrary
CreateForeignFunctionWithLibrary
CreateForeignFunction
CallForeignFunction

FFIType

OpaqueRawPointer


Begin["`Private`"]


Needs["ForeignFunctionInterface`ManagedExpression`"]
Needs["ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ForeignFunctionInterface`CArray`"]
Needs["ForeignFunctionInterface`LibFFI`"]


End[] (* End `Private` *)

EndPackage[]
