BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


CreateFFIType
DeleteFFIType

NameFFITypeID
RawFFIType

FFITypeByteCount
FFITypeElementCount

ExpressionToC
CToExpression


$LibFFIPaths = {"libffi", "ffiConstants"};


Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CreateForeignFunction`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CallForeignFunction`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Callback`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`ExpressionConversion`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`BaseTypes`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Constants`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`FFIType`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`RawFunctionLoading`"]


DeclareCompiledComponent["ForeignFunctionInterface", "ExternalLibraries" -> $LibFFIPaths];


EndPackage[]
