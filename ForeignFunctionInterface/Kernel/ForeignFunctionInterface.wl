BeginPackage["ForeignFunctionInterface`"]


CreateManagedExpression
GetManagedExpression

CreateCArray
FreeMemory

LoadExternalLibrary
CreateForeignFunction
CallForeignFunction

GetFFIType
GetFFIType2


Begin["`Private`"]


Needs["ForeignFunctionInterface`ManagedExpression`"]
Needs["ForeignFunctionInterface`RawObject`"]
Needs["ForeignFunctionInterface`ForeignFunction`"]
Needs["ForeignFunctionInterface`LibFFI`"]


End[] (* End `Private` *)

EndPackage[]
