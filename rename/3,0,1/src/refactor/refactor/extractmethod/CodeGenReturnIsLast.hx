package refactor.refactor.extractmethod;

import refactor.discover.Identifier;

class CodeGenReturnIsLast extends CodeGenBase {
	final returnEmpty:Bool;

	public function new(extractData:ExtractMethodData, context:RefactorContext, neededIdentifiers:Array<Identifier>, returnEmpty:Bool) {
		super(extractData, context, neededIdentifiers);
		this.returnEmpty = returnEmpty;
	}

	public function makeCallSite():String {
		final callParams:String = neededIdentifiers.map(i -> i.name).join(", ");
		final call = '${extractData.newMethodName}($callParams)';
		if (returnEmpty) {
			return '$call;';
		} else {
			return 'return $call;';
		}
	}

	public function makeReturnTypeHint():Promise<String> {
		if (returnEmpty) {
			return Promise.resolve(":Void");
		}
		return parentTypeHint().then(function(typeHint):Promise<String> {
			if (typeHint == null) {
				return Promise.resolve("");
			}
			return Promise.resolve(":" + typeHint.printTypeHint());
		});
	}

	public function makeBody():String {
		final selectedSnippet = RefactorHelper.extractText(context.converter, extractData.content, extractData.startToken.pos.min,
			extractData.endToken.pos.max);
		return " {\n" + selectedSnippet + "\n}\n";
	}
}
