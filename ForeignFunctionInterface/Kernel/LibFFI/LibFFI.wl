BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


$LibFFIPaths = {"/lib/x86_64-linux-gnu/libffi.so", "ffiConstants"};


Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Base`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`BaseTypes`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Constants`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`RawFunctionLoading`"]


DeclareCompiledComponent["ForeignFunctionInterface", "ExternalLibraries" -> $LibFFIPaths];


EndPackage[]