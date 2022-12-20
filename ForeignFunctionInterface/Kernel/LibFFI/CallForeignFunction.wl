BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CallForeignFunction`"]

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Callback`"] (* for GetCallbackPointer *)



DeclareCompiledComponent["ForeignFunctionInterface", {

	FunctionDeclaration[CallForeignFunction,
		Typed[{"ForeignFunctionObject", "InertExpression"} -> "InertExpression"]@
		Function[{ff, args},
			Module[{argCount},
				argCount = Cast[ff["CallInterface"]["ArgumentCount"], "MachineInteger", "CCast"];

				If[Head[args] =!= InertExpression[List] || Length[args] =!= argCount,
					Native`ThrowWolframExceptionCode["Argument"]
				];

				Do[
					ExpressionToC[
						FromRawPointer[ff["ArgumentPointers"], i-1],
						FromRawPointer[ff["CallInterface"]["ArgumentTypes"], i-1],
						args[[i]]
					],
					{i, argCount}
				];

				LibraryFunction["ffi_call"][ff["CallInterface"], ff["FunctionPointer"], ff["OutputPointer"], ff["ArgumentPointers"]];

				CToExpression[ff["OutputPointer"], ff["CallInterface"]["OutputType"]]
			]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CallForeignFunction
}];



End[] (* End `Private` *)

EndPackage[]
