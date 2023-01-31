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

		FunctionDeclaration[iCreateManagedExpression,
			Typed[{"InertExpression", "InertExpression"} -> "ManagedExpression"]@
			Function[{expr, f},
				CreateTypeInstance["ManagedExpression", <|"Expression" -> expr, "FreeingFunction" -> f|>]
			]
		],

		FunctionDeclaration[iGetManagedExpression,
			Typed[{"ManagedExpression"} -> "InertExpression"]@
			Function[manExpr,
				manExpr["Expression"]
			]
		],

		FunctionDeclaration[iGetManagedExpressionFunction,
			Typed[{"ManagedExpression"} -> "InertExpression"]@
			Function[manExpr,
				manExpr["FreeingFunction"]
			]
		]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	iCreateManagedExpression,
	iGetManagedExpression,
	iGetManagedExpressionFunction
}];



(* DownValues *)


GetManagedExpression[ManagedExpression[obj_DataStructure]] :=
	iGetManagedExpression[obj]

GetManagedExpression[arg_] :=
	(
		Message[GetManagedExpression::invManagedExpr, arg];
		Failure["InvalidManagedExpression", <|
			"MessageTemplate" :> GetManagedExpression::invManagedExpr,
			"MessageParameters" -> {arg}
		|>]
	)


(*
	ManagedExpression[obj]
		represents a managed expression object.
*)

(* Constructors *)

HoldPattern[ManagedExpression][expr_, f_] :=
	ManagedExpression[iCreateManagedExpression[expr, f]]



(* Validators *)

HoldPattern[ManagedExpression][man:Except[_DataStructure]] :=
	(
		Message[ManagedExpression::inv, man];
		Failure["InvalidManagedExpression", <|
			"MessageTemplate" :> ManagedExpression::inv,
			"MessageParameters" -> {man}
		|>]
	)

HoldPattern[ManagedExpression][man_, args__] :=
	ArgumentsOptions[ManagedExpression[man, args]]


(* Summary box *)

ManagedExpression /: MakeBoxes[expr:HoldPattern[ManagedExpression][obj_DataStructure], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		ManagedExpression,
		expr,
		None,
		{
			{"value: ", GetManagedExpression[expr]},
			{"freeing function: ", iGetManagedExpressionFunction[obj]}
		},
		{},
		form
	]



End[] (* End `Private` *)

EndPackage[]
