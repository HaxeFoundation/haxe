package refactor.refactor.extractmethod;

import refactor.discover.Identifier;

class CodeGenAsExpression extends CodeGenBase {
	public function new(extractData:ExtractMethodData, context:RefactorContext, neededIdentifiers:Array<Identifier>) {
		super(extractData, context, neededIdentifiers);
	}

	public function makeCallSite():String {
		final callParams:String = neededIdentifiers.map(i -> i.name).join(", ");
		final call = '${extractData.newMethodName}($callParams)';

		return switch (extractData.endToken.tok) {
			case Semicolon | BrClose:
				'$call;';
			default:
				'$call';
		}
	}

	public function makeReturnTypeHint():Promise<String> {
		var parent = extractData.startToken.parent;
		switch (parent.tok) {
			case Binop(OpAssign) | Binop(OpAssignOp(_)):
				parent = parent.parent;
				return TypingHelper.findTypeWithTyper(context, context.what.fileName, parent.pos.max - 1).then(function(typeHint):Promise<String> {
					if (typeHint == null) {
						return Promise.resolve("");
					}
					return Promise.resolve(":" + typeHint.printTypeHint());
				});
			case Kwd(KwdReturn):
				return parentTypeHint().then(function(typeHint):Promise<String> {
					if (typeHint == null) {
						return Promise.resolve("");
					}
					return Promise.resolve(":" + typeHint.printTypeHint());
				});
			default:
		}
		return TypingHelper.findTypeWithTyper(context, context.what.fileName, extractData.endToken.pos.max - 1).then(function(typeHint):Promise<String> {
			if (typeHint == null) {
				return Promise.resolve("");
			}
			return Promise.resolve(":" + typeHint.printTypeHint());
		});
	}

	public function makeBody():String {
		final selectedSnippet = RefactorHelper.extractText(context.converter, extractData.content, extractData.startToken.pos.min,
			extractData.endToken.pos.max);
		return " {\n" + "return " + selectedSnippet + "\n}\n";
	}
}
