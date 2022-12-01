BeginPackage["ChristopherWolfram`ForeignFunctionInterface`"]


CreateManagedExpression
GetManagedExpression

CreateBuffer
BufferToNumericArray
StringToBuffer
BufferToString
DereferenceBuffer
FreeBuffer

LoadExternalLibrary
UnloadExternalLibrary
CreateForeignFunctionWithLibrary
CreateForeignFunction
CallForeignFunction

FFIType

OpaqueRawPointer


Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`ManagedExpression`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`Buffer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


End[] (* End `Private` *)

EndPackage[]
