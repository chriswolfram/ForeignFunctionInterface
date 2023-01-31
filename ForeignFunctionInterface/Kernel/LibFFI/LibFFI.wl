BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


CreateFFIType
DeleteFFIType

NameFFITypeID
RawFFIType

FFITypeByteCount
FFITypeElementCount

ExpressionToC
CToExpression

CreateForeignFunction
CallForeignFunction


$LibFFIPaths = {"libffi", "ffiConstants"};


Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CreateForeignFunction`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CallForeignFunction`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Callback`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`ExpressionConversion`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`BaseTypes`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Constants`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`FFIType`"]


DeclareCompiledComponent["ForeignFunctionInterface", "ExternalLibraries" -> $LibFFIPaths];


EndPackage[]
