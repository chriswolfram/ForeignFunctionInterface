BeginPackage["ForeignFunctionInterface`LibFFI`"]


$LibFFIPaths = {"/usr/local/opt/libffi/lib/libffi.dylib", "/Users/christopher/git/ForeignFunctionInterface/LibFFIInterface/libFFIInterface.dylib"};


Needs["ForeignFunctionInterface`LibFFI`Base`"]


DeclareCompiledComponent["ForeignFunctionInterface", "ExternalLibraries" -> $LibFFIPaths];


EndPackage[]