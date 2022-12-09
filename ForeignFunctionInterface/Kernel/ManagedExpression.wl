BeginPackage["ChristopherWolfram`ForeignFunctionInterface`ManagedExpression`"]

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]


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
		],

		FunctionDeclaration[GetManagedExpression,
			Typed[{"InertExpression"} -> "InertExpression"]@
			Function[expr,
				If[Native`PrimitiveFunction["TestGet_ObjectInstanceByName"][expr, Typed["ManagedExpression","CString"], ToRawPointer[]],
					GetManagedExpression[Cast[expr,"ManagedExpression"]],
					expr
				]
			]
		]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateManagedExpression
}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	GetManagedExpression -> Typed[GetManagedExpression, {"InertExpression"} -> "InertExpression"]
|>];



End[] (* End `Private` *)

EndPackage[]
