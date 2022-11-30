BeginPackage["ForeignFunctionInterface`"]


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


Needs["ForeignFunctionInterface`ManagedExpression`"]
Needs["ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ForeignFunctionInterface`Buffer`"]
Needs["ForeignFunctionInterface`LibFFI`"]


End[] (* End `Private` *)

EndPackage[]
