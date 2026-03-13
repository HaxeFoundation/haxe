package refactor.refactor;

import refactor.discover.File;
import refactor.edits.Changelist;
import refactor.refactor.RefactorHelper.TokensAtPos;

class RewriteWrapWithTryCatch {
	public static function canRefactor(context:CanRefactorContext, isRangeSameScope:Bool):CanRefactorResult {
		if (!isRangeSameScope) {
			return Unsupported;
		}
		final extractData = makeRewriteWrapWithTryCatch(context);
		if (extractData == null) {
			return Unsupported;
		}
		return Supported('Wrap With Try…Catch');
	}

	public static function doRefactor(context:RefactorContext):Promise<RefactorResult> {
		final extractData = makeRewriteWrapWithTryCatch(context);
		if (extractData == null) {
			return Promise.reject("failed to collect data for rewrite wrap with try catch");
		}
		final changelist:Changelist = new Changelist(context);

		final selectedSnippet = RefactorHelper.extractText(context.converter, extractData.content, extractData.startToken.pos.min,
			extractData.endToken.pos.max);

		final wrappedSnippet = "try {\n" + selectedSnippet + "\n}\ncatch (e:haxe.Exception) {\n// TODO: handle exception\ntrace (e.details());\n}";

		changelist.addChange(context.what.fileName,
			ReplaceText(wrappedSnippet, {fileName: context.what.fileName, start: extractData.startToken.pos.min, end: extractData.endToken.pos.max},
				Format(extractData.snippetIndent, true)),
			null);
		return Promise.resolve(changelist.execute());
	}

	static function makeRewriteWrapWithTryCatch(context:CanRefactorContext):Null<RewriteWrapWithTryCatchData> {
		if (context.what.posStart >= context.what.posEnd) {
			return null;
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
			return null;
		}
		if (content == null) {
			return null;
		}

		final file:Null<File> = context.fileList.getFile(context.what.fileName);
		if (file == null) {
			return null;
		}
		// find corresponding tokens in tokentree, selection start/end in whitespace
		final tokensStart:TokensAtPos = RefactorHelper.findTokensAtPos(root, context.what.posStart);
		final tokensEnd:TokensAtPos = RefactorHelper.findTokensAtPos(root, context.what.posEnd);
		if (tokensStart.after == null || tokensEnd.before == null) {
			return null;
		}

		final tokenStart:Null<TokenTree> = tokensStart.after;
		final tokenEnd:Null<TokenTree> = tokensEnd.before;

		if (tokenStart == null || tokenEnd == null) {
			return null;
		}
		if (tokenStart.index >= tokenEnd.index) {
			return null;
		}

		final tokenEndLast:Null<TokenTree> = TokenTreeCheckUtils.getLastToken(tokenEnd);
		if (tokenEndLast == null) {
			return null;
		}
		if (tokenEnd.index != tokenEndLast.index) {
			return null;
		}

		// extracting only works if parent of start token is also grand…parent of end token
		if (!RefactorHelper.shareSameParent(tokenStart, tokenEnd)) {
			return null;
		}

		final snippetIndent:Int = RefactorHelper.calcIndentation(context, content, context.what.fileName, tokenStart.pos.min);

		return {
			content: content,
			root: root,
			startToken: tokenStart,
			endToken: tokenEnd,
			snippetIndent: snippetIndent,
		};
	}
}

typedef RewriteWrapWithTryCatchData = {
	var content:String;
	var root:TokenTree;
	var startToken:TokenTree;
	var endToken:TokenTree;
	var snippetIndent:Int;
}
