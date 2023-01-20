BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`RawFunctionLoading`"]

GetExternalLibraryHandle

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]


(***************************************************)
(***************** dlopen / dlsym ******************)
(***************************************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Alias", "ExternalLibraryHandle", "OpaqueRawPointer", "AbstractTypes" -> {"DataStructures"}],

	LibraryFunctionDeclaration["dlopen",
		{"CString", "CInt"} -> "ExternalLibraryHandle"],

	LibraryFunctionDeclaration["dlclose",
		{"ExternalLibraryHandle"} -> "CInt"],

	LibraryFunctionDeclaration["dlsym",
		{"ExternalLibraryHandle", "CString"} -> "OpaqueRawPointer"],

	LibraryFunctionDeclaration["dlerror",
		{} -> "CString"],


	CompiledExpressionDeclaration["ExternalLibrary",
		Function[{expr,ty},
			Head[expr] === InertExpression[ExternalLibrary] && Length[ExternalLibrary] === 2
		]
	],


	FunctionDeclaration[LoadExternalLibrary,
		Typed[{"InertExpression"} -> "InertExpression"]@
		Function[libSpec,
			Module[{libPathExpr, libPath, ptr},
				libPathExpr = Typed[KernelFunction[FindLibrary], {"InertExpression"} -> "InertExpression"][libSpec];
				libPath = Cast[libPathExpr, "String"];
				ptr = LibraryFunction["dlopen"][Cast[libPath, "Managed"::["CString"]], LibraryFunction["get_RTLD_LAZY"][]];
				If[Cast[ptr, "OpaqueRawPointer", "BitCast"] === Cast[0, "OpaqueRawPointer", "BitCast"],
					loadingFailure[libPath],
					Construct[InertExpression[ExternalLibrary],
						libPathExpr,
						PointerToExpression[Cast[ptr,"OpaqueRawPointer","BitCast"]]
					]
				]
			]
		]
	],

	FunctionDeclaration[loadingFailure,
		Typed[{"String"} -> "InertExpression"]@
		Function[libPath,
			With[{error = CreateTypeInstance["String", LibraryFunction["dlerror"][]]},
				Construct[InertExpression[Failure],
					InertExpression["ExternalLibraryOpenFailure"],
					Construct[InertExpression[Association],
						InertExpression["MessageTemplate" :> LoadExternalLibrary::cannotopen],
						Construct[InertExpression[Rule],
							InertExpression["MessageParameters"],
							Construct[InertExpression[List],
								Cast[error,"InertExpression"]
							]
						],
						Construct[InertExpression[Rule],
							InertExpression["LibraryPath"],
							Cast[libPath,"InertExpression"]
						],
						Construct[InertExpression[Rule],
							InertExpression["ErrorMessage"],
							Cast[error,"InertExpression"]
						]
					]
				]
			]
		]
	],

	FunctionDeclaration[GetDefaultExternalLibrary,
		Typed[{} -> "InertExpression"]@
		Function[{},
			Construct[InertExpression[ExternalLibrary],
				InertExpression[None],
				PointerToExpression[Cast[LibraryFunction["get_RTLD_DEFAULT"][],"OpaqueRawPointer","BitCast"]]
			]
		]
	],

	FunctionDeclaration[UnloadExternalLibrary,
		Typed[{"ExternalLibrary"} -> "Null"]@
		Function[extlib,
			LibraryFunction["dlclose"][GetExternalLibraryHandle[extlib]];
		]
	],

	FunctionDeclaration[GetExternalLibraryHandle,
		Typed[{"ExternalLibrary"} -> "ExternalLibraryHandle"]@
		Function[expr,
			Cast[ExpressionToPointer[Cast[expr,"InertExpression"][[2]]], "ExternalLibraryHandle", "BitCast"]
		]
	]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	LoadExternalLibrary,
	UnloadExternalLibrary,
	GetDefaultExternalLibrary
}];


End[] (* End `Private` *)

EndPackage[]
