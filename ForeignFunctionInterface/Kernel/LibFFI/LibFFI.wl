BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


$LibFFIPaths = {"libffi", "ffiConstants"};


Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Base`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`BaseTypes`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Constants`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`RawFunctionLoading`"]


DeclareCompiledComponent["ForeignFunctionInterface", "ExternalLibraries" -> $LibFFIPaths];


EndPackage[]
