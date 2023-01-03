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
DeleteBuffer

BufferToNumericArray
NumericArrayToBuffer

StringToBuffer
BufferToString

BufferToList
ListToBuffer

PopulateBuffer
DereferenceBuffer


CreateManagedExpression
GetManagedExpression


Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`ManagedExpression`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`Buffer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


End[] (* End `Private` *)

EndPackage[]
