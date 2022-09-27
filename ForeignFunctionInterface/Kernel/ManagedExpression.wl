BeginPackage["ForeignFunctionInterface`ManagedExpression`", {
	"ForeignFunctionInterface`",
	"Compile`Utilities`Components`"
}]

Begin["`Private`"]



CompiledComponentAppendTo["ForeignFunctionInterface", {

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


CompiledComponentAppendTo["ForeignFunctionInterface", "InstalledFunctions", {
	CreateManagedExpression
}];



End[] (* End `Private` *)

EndPackage[]
