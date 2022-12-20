BeginPackage["ChristopherWolfram`ForeignFunctionInterface`"]

OpaqueRawPointer

LoadExternalLibrary
UnloadExternalLibrary
CreateForeignFunctionWithLibrary
CreateForeignFunction
CallForeignFunction

CreateCallback
DeleteCallback

CreateBuffer
BufferToNumericArray
NumericArrayToBuffer
StringToBuffer
BufferToString
DereferenceBuffer
DeleteBuffer

CreateManagedExpression
GetManagedExpression


Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`ManagedExpression`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`Buffer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


End[] (* End `Private` *)

EndPackage[]
