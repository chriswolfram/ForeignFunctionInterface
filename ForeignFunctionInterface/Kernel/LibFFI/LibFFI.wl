BeginPackage["ForeignFunctionInterface`LibFFI`"]


$LibFFIPaths = {"/lib/x86_64-linux-gnu/libffi.so", "ffiConstants"};


Needs["ForeignFunctionInterface`LibFFI`Base`"]
Needs["ForeignFunctionInterface`LibFFI`BaseTypes`"]
Needs["ForeignFunctionInterface`LibFFI`Constants`"]
Needs["ForeignFunctionInterface`LibFFI`RawFunctionLoading`"]


DeclareCompiledComponent["ForeignFunctionInterface", "ExternalLibraries" -> $LibFFIPaths];


EndPackage[]