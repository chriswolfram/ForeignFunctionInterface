BeginPackage["ForeignFunctionInterface`ForeignFunction`", {
	"ForeignFunctionInterface`",
	"ForeignFunctionInterface`RawObject`" (* for CreateRawObject, UnwrapRawObject *)
}]

Begin["`Private`"]


$FFICompilerEnvironment := $FFICompilerEnvironment =
	CreateCompilerEnvironment[CompiledComponents -> {"ForeignFunctionInterface"}]


rawTypeQ[tyEnv_, ty_] :=
	Which[

		tyEnv["implementsQ", ty, "Numbers"],
			False,

		tyEnv["implementsQ", ty, "DataStructures"],
			False,

		ty["sameQ", tyEnv["resolve", "String"]],
			False,

		True,
			True

		]


(* CreateForeignFunction[name_, lib_, type_] :=
	Module[{tyEnv, resolvedTy, returnType, processedReturnType, args},

		tyEnv = $FFICompilerEnvironment["TypeEnvironment"];
		resolvedTy = tyEnv["resolve", type];
		returnType = resolvedTy["result"];
		processedReturnType = If[rawTypeQ[tyEnv, returnType], "RawObject"::[returnType], returnType];
		args = Table[Unique[$Context <> "arg"], Length@resolvedTy["arguments"]];

		FunctionCompile[{

				LibraryFunctionDeclaration[funcName -> name, lib, type]

			},

			TemplateApply[TemplateExpression[

				Typed[TemplateSlot["ArgumentTypes"] -> TemplateSlot["ReturnType"]]@
				Function[TemplateSlot["Arguments"],
					TemplateIf[TemplateSlot["NeedsWrapping"], CreateRawObject, Identity]@
						TemplateSlot["FunctionName"][TemplateSequence[TemplateIf[TemplateSlot[2], UnwrapRawObject, Identity]@TemplateSlot[1], TemplateSlot["ArgumentsNeedWrapping"]]]
				]

			], <|
				"Arguments" -> args,
				"ArgumentTypes" -> (If[rawTypeQ[tyEnv, #], "RawObject"::[#], #]& /@ resolvedTy["arguments"]),
				"ArgumentsNeedWrapping" -> Transpose[{args, (rawTypeQ[tyEnv, #] &/@ resolvedTy["arguments"])}],
				"ReturnType" -> processedReturnType,
				"NeedsWrapping" -> !returnType["sameQ", processedReturnType],
				"FunctionName" -> funcName
			|>],

			CompilerEnvironment -> $FFICompilerEnvironment

		]

	] *)



End[] (* End `Private` *)

EndPackage[]
