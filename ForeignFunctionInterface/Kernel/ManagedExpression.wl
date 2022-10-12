BeginPackage["ForeignFunctionInterface`ManagedExpression`", {
	"ForeignFunctionInterface`",
	"Compile`Utilities`Components`"
}]

Begin["`Private`"]



DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Product", "ManagedExpression",
			<|
				"Expression" -> "InertExpression",
				"FreeingFunction" -> "InertExpression"
			|>,
			"AbstractTypes" -> {"DataStructures"}
		],

		FunctionDeclaration[CompilerCallback["OnFree"],
			Typed[{"ManagedExpression"} -> "Null"]@
			Function[manExpr,
				InertEvaluate[Construct[manExpr["FreeingFunction"], manExpr["Expression"]]];
			]
		],

		FunctionDeclaration[CreateManagedExpression,
			Typed[{"InertExpression", "InertExpression"} -> "ManagedExpression"]@
			Function[{expr, f},
				CreateTypeInstance["ManagedExpression", <|"Expression" -> expr, "FreeingFunction" -> f|>]
			]
		],

		FunctionDeclaration[GetManagedExpression,
			Typed[{"ManagedExpression"} -> "InertExpression"]@
			Function[manExpr,
				manExpr["Expression"]
			]
		]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateManagedExpression
}];



End[] (* End `Private` *)

EndPackage[]
