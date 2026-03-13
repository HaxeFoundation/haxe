package refactor.refactor.extractmethod;

import refactor.discover.Identifier;
import refactor.typing.TypeHintType;

abstract class CodeGenBase implements ICodeGen {
	final extractData:ExtractMethodData;
	final context:RefactorContext;
	final neededIdentifiers:Array<Identifier>;

	public function new(extractData:ExtractMethodData, context:RefactorContext, neededIdentifiers:Array<Identifier>) {
		this.extractData = extractData;
		this.context = context;
		this.neededIdentifiers = neededIdentifiers;
	}

	function findTypeOfIdentifier(identifier:Identifier):Promise<TypeHintType> {
		return ExtractMethod.findTypeOfIdentifier(context, identifier);
	}

	function replaceReturnValues(returnTokens:Array<TokenTree>, callback:ReturnValueCallback):String {
		final selectedSnippet = RefactorHelper.extractText(context.converter, extractData.content, extractData.startToken.pos.min,
			extractData.endToken.pos.max);
		var startOffset = context.converter(extractData.content, extractData.startToken.pos.min);
		var snippet = selectedSnippet;
		returnTokens.reverse();
		for (token in returnTokens) {
			var pos = token.getPos();
			var returnStart = context.converter(extractData.content, pos.min);
			var returnEnd = context.converter(extractData.content, pos.max);
			returnStart -= startOffset;
			returnEnd -= startOffset;
			var before = snippet.substring(0, returnStart);
			var after = snippet.substring(returnEnd);
			var retValue = snippet.substring(returnStart + 7, returnEnd - 1);
			snippet = before + "return " + callback(retValue) + ";" + after;
		}
		return snippet;
	}

	function parentTypeHint():Promise<TypeHintType> {
		var func:Null<TokenTree> = findParentFunction();
		if (func == null) {
			return Promise.reject("failed to find return type of selected code");
		}
		return TypingHelper.findTypeWithTyper(context, context.what.fileName, func.pos.max - 1).then(function(typeHint) {
			return switch (typeHint) {
				case null | ClasspathType(_) | LibType(_) | StructType(_) | UnknownType(_) | NamedType(_):
					Promise.resolve(typeHint);
				case FunctionType(args, retVal):
					Promise.resolve(retVal);
			}
		});
	}

	function findParentFunction():Null<TokenTree> {
		var token = extractData.startToken.parent;
		while (true) {
			if (token == null) {
				return null;
			}
			switch (token.tok) {
				case Kwd(KwdFunction):
					var child = token.getFirstChild();
					if (child == null) {
						return null;
					}
					switch (child.tok) {
						case Const(_):
							return child;
						default:
							return token;
					}
				case Arrow:
					return token;
				case Root:
					return null;
				default:
					token = token.parent;
			}
		}
	}
}

typedef ReturnValueCallback = (value:String) -> String;
