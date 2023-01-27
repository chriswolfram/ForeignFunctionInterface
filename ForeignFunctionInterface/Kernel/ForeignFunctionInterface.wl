BeginPackage["ChristopherWolfram`ForeignFunctionInterface`"]

OpaqueRawPointer
RawPointer

ExternalLibrary
LoadExternalLibrary
UnloadExternalLibrary

ForeignFunction

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
Needs["ChristopherWolfram`ForeignFunctionInterface`RawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`Buffer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`ForeignFunction`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`ExternalLibrary`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`BaseTypeConversion`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`Messages`"]


End[] (* End `Private` *)

EndPackage[]
