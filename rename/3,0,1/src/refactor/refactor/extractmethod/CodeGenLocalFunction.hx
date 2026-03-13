package refactor.refactor.extractmethod;

import refactor.discover.Identifier;

class CodeGenLocalFunction extends CodeGenBase {
	final localParams:Array<Identifier>;
	final returnIsEmpty:Bool;

	public function new(extractData:ExtractMethodData, context:RefactorContext, neededIdentifiers:Array<Identifier>, localParams:Array<Identifier>,
			returnIsEmpty:Bool) {
		super(extractData, context, neededIdentifiers);
		this.localParams = localParams;
		this.returnIsEmpty = returnIsEmpty;
	}

	public function makeCallSite():String {
		return switch (extractData.functionType) {
			case NoFunction:
				"";
			case Named(_):
				"";
			case Unnamed if (neededIdentifiers.length == 0):
				extractData.newMethodName;
			case Unnamed if (returnIsEmpty):
				final outerParams:String = localParams.map(i -> i.name).join(", ");
				final innerParams:String = localParams.concat(neededIdentifiers).map(i -> i.name).join(", ");
				'function($outerParams) { ${extractData.newMethodName}($innerParams); }';
			case Unnamed:
				final outerParams:String = localParams.map(i -> i.name).join(", ");
				final innerParams:String = localParams.concat(neededIdentifiers).map(i -> i.name).join(", ");
				'function($outerParams) { return ${extractData.newMethodName}($innerParams); }';
		}
	}

	public function makeReturnTypeHint():Promise<String> {
		return functionHint().then(function(typeHint):Promise<String> {
			if (typeHint == null) {
				return Promise.resolve("");
			}
			return Promise.resolve(":" + typeHint.printTypeHint());
		});
	}

	function functionHint():Promise<TypeHintType> {
		var func:Null<TokenTree> = switch (extractData.functionType) {
			case NoFunction:
				null;
			case Named(_):
				extractData.startToken.access().firstChild().token;
			case Unnamed:
				switch (extractData.startToken.tok) {
					case Kwd(KwdFunction):
						extractData.startToken;
					case POpen | Const(_):
						extractData.startToken.access().firstOf(Arrow).token;
					default:
						null;
				}
		}
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

	public function makeBody():String {
		var body:Null<TokenTree> = switch (extractData.functionType) {
			case NoFunction:
				null;
			case Named(_):
				extractData.startToken.access().firstChild().firstOf(BrOpen).token;
			case Unnamed:
				switch (extractData.startToken.tok) {
					case Kwd(KwdFunction):
						extractData.startToken.access().firstOf(BrOpen).token;
					case POpen | Const(_):
						extractData.startToken.access().firstOf(Arrow).firstChild().token;
					default:
						null;
				}
		}
		if (body == null) {
			return "";
		}

		var pos:Position = switch (body.tok) {
			case BrOpen:
				final firstChild = body.getFirstChild();
				if (firstChild == null) {
					return "";
				}
				final brClose = body.access().firstOf(BrClose).token;
				if (brClose == null) {
					return "";
				}
				final prev = brClose.previousSibling;
				var endPos = brClose.pos.min - 1;
				if (prev != null) {
					final lastChild = TokenTreeCheckUtils.getLastToken(prev);
					if (lastChild != null) {
						endPos = lastChild.pos.max;
					}
				}
				{
					file: body.pos.file,
					min: firstChild.pos.min,
					max: endPos
				}
			default:
				body.getPos();
		}

		final selectedSnippet = RefactorHelper.extractText(context.converter, extractData.content, pos.min, pos.max);
		return " {\n" + selectedSnippet + "\n}\n";
	}
}
