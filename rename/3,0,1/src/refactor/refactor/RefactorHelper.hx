package refactor.refactor;

import formatter.Formatter;
import refactor.CacheAndTyperContext.ByteToCharConverterFunc;
import refactor.discover.File;

class RefactorHelper {
	public static function findTokensAtPos(root:TokenTree, searchPos:Int):TokensAtPos {
		var tokens:TokensAtPos = {
			before: null,
			after: null
		}
		var distanceBefore:Int = 10000;
		var distanceAfter:Int = 10000;

		root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			if (token.pos.min < searchPos) {
				var distance = searchPos - token.pos.max;
				if (distanceBefore > distance) {
					tokens.before = token;
					distanceBefore = distance;
				}
			}

			if (token.pos.max > searchPos) {
				var distance = token.pos.min - searchPos;
				if (distanceAfter > distance) {
					tokens.after = token;
					distanceAfter = distance;
				}
			}
			return GoDeeper;
		});

		return tokens;
	}

	public static function extractText(converter:ByteToCharConverterFunc, text:String, start:Int, end:Int):String {
		return text.substring(converter(text, start), converter(text, end));
	}

	public static function findFirstImport(tree:TokenTree):Null<TokenTree> {
		if (!tree.hasChildren()) {
			return null;
		}
		for (child in tree.children) {
			switch (child.tok) {
				case Kwd(KwdImport) | Kwd(KwdUsing):
					return child;
				case Kwd(KwdAbstract) | Kwd(KwdClass) | Kwd(KwdEnum) | Kwd(KwdInterface) | Kwd(KwdTypedef) | Kwd(KwdVar) | Kwd(KwdFinal) | Kwd(KwdFunction):
					return child;
				case Sharp(_):
					return null;
				default:
			}
		}
		return null;
	}

	public static function makeNewFileImports(context:CanRefactorContext, file:File, potentialNames:Array<String>) {
		final needImports:Array<Import> = [];
		final importedTypes:Array<String> = [];
		final imports:Array<Import> = [];
		for (imp in file.importList) {
			if (imp.moduleName.type == UsingModul) {
				needImports.push(imp);
			}
			if (imp.starImport) {
				needImports.push(imp);
				continue;
			}
			if (imp.alias != null) {
				importedTypes.push(imp.alias.name);
				imports.push(imp);
				continue;
			}
			final parts = imp.moduleName.name.split(".");
			importedTypes.push(parts.pop());
			imports.push(imp);
		}

		for (name in potentialNames) {
			final index = importedTypes.indexOf(name);
			if (index >= 0) {
				final imp = imports[index];
				if (needImports.contains(imp)) {
					continue;
				}
				needImports.push(imp);
			}
		}
		final buf:StringBuf = new StringBuf();
		for (imp in needImports) {
			switch (imp.moduleName.type) {
				case ImportModul:
					buf.add("import ");
					buf.add(imp.moduleName.name);
					if (imp.starImport) {
						buf.add(".*");
					}
					buf.add(";\n");
				case ImportAlias:
					buf.add("import ");
					buf.add(imp.moduleName.name);
					buf.add(" as ");
					buf.add(imp.alias.name);
					buf.add(";\n");
				case UsingModul:
					buf.add("using ");
					buf.add(imp.moduleName.name);
					buf.add(";\n");
				default:
			}
		}

		final imports = buf.toString();
		if (imports.length <= 0) {
			return "";
		}

		return imports + "\n";
	}

	public static function getLastHeaderToken(tree:TokenTree):Null<TokenTree> {
		final headers:Array<TokenTree> = tree.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch token.tok {
				case Kwd(KwdAbstract) | Kwd(KwdClass) | Kwd(KwdEnum) | Kwd(KwdInterface) | Kwd(KwdTypedef) | Kwd(KwdVar) | Kwd(KwdFinal) | Kwd(KwdFunction):
					return FoundSkipSubtree;
				case Kwd(KwdPackage), Kwd(KwdImport), Kwd(KwdUsing):
					return SkipSubtree;
				case Comment(_) | CommentLine(_):
					return SkipSubtree;
				default:
			}
			return GoDeeper;
		});
		return headers.shift();
	}

	public static function calcIndentation(context:CacheAndTyperContext, content:String, fileName:String, pos:Int):Int {
		final config = Formatter.loadConfig(fileName);
		var startPos = pos - 200;
		if (startPos < 0) {
			startPos = 0;
		}

		var text = extractText(context.converter, content, startPos, pos);
		var indexLF = text.lastIndexOf("\n");
		var indexCR = text.lastIndexOf("\r");
		if (indexCR < 0) {
			indexCR = indexLF;
		}
		if (indexLF < 0) {
			indexLF = indexCR;
		}
		if (indexLF < 0) {
			return 0;
		}
		final index = if (indexLF > indexCR) indexLF else indexCR;
		final line = text.substr(index);
		final regex = ~/([\t ]+).*/;
		if (!regex.match(line)) {
			return 0;
		}
		var match = regex.matched(1);
		var character = config?.indentation?.character ?? "tab";
		if (character.toLowerCase() == "tab") {
			character = "\t";
		}
		var parts = match.split(character);
		if (parts.length <= 0) {
			return 0;
		}
		return parts.length - 1;
	}

	public static function rangeInSameScope(context:CanRefactorContext):Bool {
		if (context.what.posStart == context.what.posEnd) {
			return true;
		}

		final fileContent = context.fileReader(context.what.fileName);
		var content:String;
		var root:TokenTree;
		switch (fileContent) {
			case Text(_):
				return null;
			case Token(tokens, text):
				content = text;
				root = tokens;
		}
		if (root == null) {
			return false;
		}
		if (content == null) {
			return false;
		}

		final file:Null<File> = context.fileList.getFile(context.what.fileName);
		if (file == null) {
			return false;
		}
		// find corresponding tokens in tokentree, selection start/end in whitespace
		final tokensStart:TokensAtPos = RefactorHelper.findTokensAtPos(root, context.what.posStart);
		final tokensEnd:TokensAtPos = RefactorHelper.findTokensAtPos(root, context.what.posEnd);
		if (tokensStart.after == null || tokensEnd.before == null) {
			return false;
		}

		final tokenStart:Null<TokenTree> = tokensStart.after;
		final tokenEnd:Null<TokenTree> = tokensEnd.before;

		if (tokenStart == null || tokenEnd == null) {
			return false;
		}
		if (tokenStart.index >= tokenEnd.index) {
			return false;
		}

		final tokenEndLast:Null<TokenTree> = TokenTreeCheckUtils.getLastToken(tokenEnd);
		if (tokenEndLast == null) {
			return false;
		}
		if (tokenEnd.index != tokenEndLast.index) {
			return false;
		}

		// extracting only works if parent of start token is also grand…parent of end token
		return shareSameParent(tokenStart, tokenEnd);
	}

	public static function shareSameParent(tokenA:TokenTree, tokenB:TokenTree):Bool {
		var parentA = tokenA.parent;
		if (parentA == null) {
			return false;
		}
		switch (parentA.tok) {
			case POpen:
				final closeToken = parentA.access().firstOf(PClose).token;
				if (closeToken == null) {
					return false;
				}
				if (closeToken.index < tokenB.index) {
					return false;
				}
			case BrOpen:
				final closeToken = parentA.access().firstOf(BrClose).token;
				if (closeToken == null) {
					return false;
				}
				if (closeToken.index < tokenB.index) {
					return false;
				}
			case BkOpen:
				final closeToken = parentA.access().firstOf(BkClose).token;
				if (closeToken == null) {
					return false;
				}
				if (closeToken.index < tokenB.index) {
					return false;
				}
			default:
		}
		var parentB = tokenB.parent;
		var oldParentB = tokenB;
		while (true) {
			if (parentB == null) {
				return false;
			}
			if (parentA.index == parentB.index) {
				final lastToken = TokenTreeCheckUtils.getLastToken(oldParentB);
				if (lastToken == null) {
					return false;
				}
				return (lastToken.index <= tokenB.index);
			}
			oldParentB = parentB;
			parentB = parentB.parent;
		}
	}
}

typedef TokensAtPos = {
	var before:Null<TokenTree>;
	var after:Null<TokenTree>;
}
