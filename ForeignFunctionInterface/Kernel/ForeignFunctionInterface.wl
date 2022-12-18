BeginPackage["ChristopherWolfram`ForeignFunctionInterface`"]

OpaqueRawPointer

LoadExternalLibrary
UnloadExternalLibrary
CreateForeignFunctionWithLibrary
CreateForeignFunction
CallForeignFunction

CreateCallback
FreeCallback

CreateBuffer
BufferToNumericArray
NumericArrayToBuffer
StringToBuffer
BufferToString
DereferenceBuffer
FreeBuffer

CreateManagedExpression
GetManagedExpression


Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`ManagedExpression`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`Buffer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


End[] (* End `Private` *)

EndPackage[]
