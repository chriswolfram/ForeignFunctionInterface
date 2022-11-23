BeginPackage["ForeignFunctionInterface`"]


CreateManagedExpression
GetManagedExpression

CreateCArray
FreeMemory

LoadExternalLibrary
UnloadExternalLibrary
CreateForeignFunctionWithLibrary
CreateForeignFunction
CallForeignFunction

GetFFIType


Begin["`Private`"]


Needs["ForeignFunctionInterface`ManagedExpression`"]
Needs["ForeignFunctionInterface`RawObject`"]
Needs["ForeignFunctionInterface`ForeignFunction`"]
Needs["ForeignFunctionInterface`LibFFI`"]


End[] (* End `Private` *)

EndPackage[]
