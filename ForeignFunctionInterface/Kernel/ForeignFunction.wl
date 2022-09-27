BeginPackage["ForeignFunctionInterface`ForeignFunction`", {
	"ForeignFunctionInterface`",
	"ForeignFunctionInterface`RawObject`", (* for CreateRawObject *)

	"Compile`Utilities`Components`"
}]

Begin["`Private`"]


$FFICompilerEnvironment := $FFICompilerEnvironment =
	CreateCompilerEnvironment[CompiledComponents -> {"ForeignFunctionInterface"}]


processType[tyEnv_, ty_] :=
	If[tyEnv["implementsQ", ty, "DirectBoxables"], ty, tyEnv["resolve", "RawObject"::[ty]]]


CreateForeignFunction[name_, lib_, type_] :=
	Module[{tyEnv, resolvedTy},

		tyEnv = $FFICompilerEnvironment["TypeEnvironment"];
		resolvedTy = tyEnv["resolve", type];

		FunctionCompile[{

				LibraryFunctionDeclaration[name, lib, type]

			},

			TemplateApply[TemplateExpression[

				Typed[TemplateSlot["ArgumentTypes"] -> TemplateSlot["ReturnType"]]@
				Function[TemplateSlot["Arguments"],
					CreateRawObject@
						LibraryFunction[name][TemplateSequence[CreateRawObject@TemplateSlot[1], TemplateSlot["Arguments"]]]
				]

			], <|
				"Arguments" -> Table[Unique[$Context <> "arg"], Length@resolvedTy["arguments"]],
				"ArgumentTypes" -> (processType[tyEnv, #]& /@ resolvedTy["arguments"]),
				"ReturnType" -> processType[tyEnv, resolvedTy["result"]]
			|>],

			CompilerEnvironment -> $FFICompilerEnvironment

		]

	]



End[] (* End `Private` *)

EndPackage[]
