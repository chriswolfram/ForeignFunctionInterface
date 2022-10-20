BeginPackage["ForeignFunctionInterface`"]


CreateManagedExpression
GetManagedExpression

CreateCArray
FreeMemory

LoadExternalLibrary
CreateForeignFunction
CallForeignFunction

GetFFITypeSignedInt32
GetFFITypeDouble


Begin["`Private`"]


Needs["ForeignFunctionInterface`ManagedExpression`"]
Needs["ForeignFunctionInterface`RawObject`"]
Needs["ForeignFunctionInterface`ForeignFunction`"]
Needs["ForeignFunctionInterface`LibFFI`"]


End[] (* End `Private` *)

EndPackage[]
