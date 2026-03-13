package refactor.refactor.extractmethod;

import refactor.discover.Identifier;

class CodeGenEmptyReturn extends CodeGenBase {
	final returnTokens:Array<TokenTree>;

	final assignments:Array<Identifier>;
	final vars:Array<Identifier>;

	public function new(extractData:ExtractMethodData, context:RefactorContext, neededIdentifiers:Array<Identifier>, returnTokens:Array<TokenTree>,
			assignments:Array<Identifier>, vars:Array<Identifier>) {
		super(extractData, context, neededIdentifiers);

		this.returnTokens = returnTokens;
		this.assignments = assignments;
		this.vars = vars;
	}

	public function makeCallSite():String {
		final callParams:String = neededIdentifiers.map(i -> i.name).join(", ");
		final call = '${extractData.newMethodName}($callParams)';

		return switch [assignments.length, vars.length] {
			case [0, 0]:
				'if (!$call) {\nreturn;\n}';
			case [1, 0]:
				'switch ($call) {\n' + 'case None:\n' + 'return;\n' + 'case Some(data):\n' + '${assignments[0].name} = data;\n}';
			case [0, 1]:
				'var ${vars[0].name};\n'
				+ 'switch ($call) {\n'
				+ 'case None:\n'
				+ 'return;\n'
				+ 'case Some(data):\n'
				+ '${vars[0].name} = data;\n}';
			case [_, _]:
				final dataVars = vars.map(v -> 'var ${v.name};').join("\n");
				final assignData = assignments.concat(vars).map(a -> '${a.name} = data.${a.name};').join("\n");
				dataVars + 'switch ($call) {\n' + "case None:\n" + "return;\n" + "case Some(data):\n" + '${assignData}\n' + "}";
		}
	}

	public function makeReturnTypeHint():Promise<String> {
		return switch [assignments.length, vars.length] {
			case [0, 0]:
				return Promise.resolve(":Bool");
			case [1, 0]:
				return findTypeOfIdentifier(assignments[0]).then(function(typeHint):Promise<String> {
					if (typeHint == null) {
						return Promise.resolve("");
					}
					return Promise.resolve(":haxe.ds.Option<" + typeHint.printTypeHint() + ">");
				});
			case [0, 1]:
				return findTypeOfIdentifier(vars[0]).then(function(typeHint):Promise<String> {
					if (typeHint == null) {
						return Promise.resolve("");
					}
					return Promise.resolve(":haxe.ds.Option<" + typeHint.printTypeHint() + ">");
				});
			case [_, _]:
				var promises:Array<Promise<String>> = [];
				for (assign in assignments) {
					promises.push(findTypeOfIdentifier(assign).then(function(typeHint):Promise<String> {
						if (typeHint == null) {
							return Promise.resolve(assign.name + ":Any");
						}
						return Promise.resolve(assign.name + ":" + typeHint.printTypeHint());
					}));
				}
				for (v in vars) {
					promises.push(findTypeOfIdentifier(v).then(function(typeHint):Promise<String> {
						if (typeHint == null) {
							return Promise.resolve(v.name + ":Any");
						}
						return Promise.resolve(v.name + ":" + typeHint.printTypeHint());
					}));
				}
				return Promise.all(promises).then(function(fields) {
					return Promise.resolve(":haxe.ds.Option<{" + fields.join(", ") + "}>");
				});
		}
	}

	public function makeBody():String {
		return switch [assignments.length, vars.length] {
			case [0, 0]:
				final snippet = replaceReturnValues(returnTokens, value -> 'false');
				" {\n" + snippet + "\nreturn true;\n}\n";
			case [1, 0]:
				final snippet = replaceReturnValues(returnTokens, value -> 'None');
				final returnAssigmentVar = '\nreturn Some(${assignments[0].name});';
				" {\n" + snippet + returnAssigmentVar + "\n}\n";
			case [0, 1]:
				final snippet = replaceReturnValues(returnTokens, value -> 'None');
				final returnLocalVar = '\nreturn Some(${vars[0].name});';
				" {\n" + snippet + returnLocalVar + "\n}\n";
			case [_, _]:
				final snippet = replaceReturnValues(returnTokens, value -> 'None');
				final assignData = assignments.concat(vars).map(a -> '${a.name}: ${a.name},\n');
				final returnAssigments = "\nreturn Some({\n" + assignData + "\n});";
				" {\n" + snippet + returnAssigments + "\n}\n";
		}
	}
}
