package refactor.refactor.extractmethod;

import refactor.discover.Identifier;

class CodeGenNoReturn extends CodeGenBase {
	final assignments:Array<Identifier>;
	final vars:Array<Identifier>;

	public function new(extractData:ExtractMethodData, context:RefactorContext, neededIdentifiers:Array<Identifier>, assignments:Array<Identifier>,
			vars:Array<Identifier>) {
		super(extractData, context, neededIdentifiers);

		this.assignments = assignments;
		this.vars = vars;
	}

	public function makeCallSite():String {
		final callParams:String = neededIdentifiers.map(i -> i.name).join(", ");
		final call = '${extractData.newMethodName}($callParams)';

		return switch [assignments.length, vars.length] {
			case [0, 0]:
				'$call;';
			case [0, 1]:
				'var ${vars[0].name} = $call;';
			case [1, 0]:
				'${assignments[0].name} = $call;';
			case [_, _]:
				final dataVars = vars.map(v -> 'var ${v.name};').join("\n");
				final assignData = assignments.concat(vars).map(a -> '${a.name} = data.${a.name};').join("\n");
				'$dataVars\n{\nfinal data = $call;\n' + assignData + "\n}";
		}
	}

	public function makeReturnTypeHint():Promise<String> {
		return switch [assignments.length, vars.length] {
			case [0, 0]:
				return Promise.resolve(":Void");
			case [0, 1]:
				return findTypeOfIdentifier(vars[0]).then(function(typeHint):Promise<String> {
					if (typeHint == null) {
						return Promise.resolve("");
					}
					return Promise.resolve(":" + typeHint.printTypeHint());
				});
			case [1, 0]:
				return findTypeOfIdentifier(assignments[0]).then(function(typeHint):Promise<String> {
					if (typeHint == null) {
						return Promise.resolve("");
					}
					return Promise.resolve(":" + typeHint.printTypeHint());
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
					return Promise.resolve(":{" + fields.join(", ") + "}");
				});
		}
	}

	public function makeBody():String {
		final selectedSnippet = RefactorHelper.extractText(context.converter, extractData.content, extractData.startToken.pos.min,
			extractData.endToken.pos.max);
		return switch [assignments.length, vars.length] {
			case [0, 0]:
				" {\n" + selectedSnippet + "\n}\n";
			case [0, 1]:
				final returnAssigmentVar = '\nreturn ${vars[0].name};';
				" {\n" + selectedSnippet + returnAssigmentVar + "\n}\n";
			case [1, 0]:
				final returnAssigmentVar = '\nreturn ${assignments[0].name};';
				" {\n" + selectedSnippet + returnAssigmentVar + "\n}\n";
			case [_, _]:
				final assignData = assignments.concat(vars).map(a -> '${a.name}: ${a.name},').join("\n");
				final returnAssigments = "\nreturn {\n" + assignData + "\n};";
				" {\n" + selectedSnippet + returnAssigments + "\n}\n";
		}
	}
}
