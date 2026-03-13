package refactor.refactor;

import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.edits.Changelist;
import refactor.refactor.RefactorHelper.TokensAtPos;

class ExtractConstructorParams {
	public static function canRefactor(context:CanRefactorContext, isRangeSameScope:Bool, asFinal:Bool):CanRefactorResult {
		final extractData = makeExtractConstructorParamsData(context);
		if (extractData == null) {
			return Unsupported;
		}
		if (asFinal) {
			return Supported('Extract Constructor Params as final');
		} else {
			return Supported('Extract Constructor Params as var');
		}
	}

	public static function doRefactor(context:RefactorContext, asFinal:Bool):Promise<RefactorResult> {
		final extractData = makeExtractConstructorParamsData(context);
		if (extractData == null) {
			return Promise.reject("failed to collect data for extract constructor params");
		}
		final changelist:Changelist = new Changelist(context);

		final newFields:StringBuf = new StringBuf();
		final newAssigns:StringBuf = new StringBuf();
		for (param in extractData.parameters) {
			newFields.add(makeField(context, extractData, param, asFinal));
			newAssigns.add(makeAssignment(extractData, param));
		}

		final fieldInsertPos = findFieldInsertPos(context, extractData);
		final assignmentInsertPos = findAssignmentInsertPos(context, extractData);
		if (fieldInsertPos <= 0 || assignmentInsertPos <= 0) {
			return Promise.reject("Extract Constructor Parameter cannot find positions for new fields");
		}

		changelist.addChange(context.what.fileName,
			InsertText(newFields.toString(), {fileName: context.what.fileName, start: fieldInsertPos, end: fieldInsertPos}, Format(1, false)), null);

		changelist.addChange(context.what.fileName,
			InsertText(newAssigns.toString(), {fileName: context.what.fileName, start: assignmentInsertPos, end: assignmentInsertPos}, Format(2, false)), null);

		return Promise.resolve(changelist.execute());
	}

	static function makeExtractConstructorParamsData(context:CanRefactorContext):Null<ExtractConstructorParamsData> {
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

		final tokensStart:TokensAtPos = RefactorHelper.findTokensAtPos(root, context.what.posStart);

		// find corresponding tokens in tokentree, selection start/end in whitespace
		if (tokensStart.after == null) {
			return null;
		}

		final tokenNew:Null<TokenTree> = tokensStart.after;

		if (tokenNew == null) {
			return null;
		}
		if (!tokenNew.matches(Kwd(KwdNew))) {
			return null;
		}
		final identifierNew = file.getIdentifier(tokenNew.pos.min);
		// find constructor parameters and see if there's at least one with no corresponding field
		var parameters = findParametersWithNoFields(context, identifierNew);
		if (parameters.length <= 0) {
			return null;
		}

		final superTokens = tokenNew.filterCallback(function(token, index) {
			return switch (token.tok) {
				case Const(CIdent("super")):
					return FoundSkipSubtree;
				default:
					GoDeeper;
			}
		});
		final superParams:Array<String> = [];
		var superToken:Null<TokenTree> = null;
		for (call in superTokens) {
			superToken = call;
			call.filterCallback(function(token, index) {
				switch (token.tok) {
					case Const(CIdent(s)):
						for (param in parameters) {
							if (param.name == s) {
								superParams.push(s);
								return GoDeeper;
							}
						}
					default:
				}
				return GoDeeper;
			});
		}
		parameters = parameters.filter(i -> !superParams.contains(i.name));
		if (parameters.length <= 0) {
			return null;
		}

		return {
			content: content,
			root: root,
			tokenNew: tokenNew,
			identifierNew: identifierNew,
			parameters: parameters,
			superToken: superToken,
			file: file,
		};
	}

	static function findParametersWithNoFields(context:CanRefactorContext, identifierNew:Identifier):Array<Identifier> {
		final params:Array<Identifier> = [];
		var paramCandidates:Array<Identifier> = [];
		if (identifierNew?.uses == null) {
			return [];
		}

		for (use in identifierNew.uses) {
			switch (use.type) {
				case ScopedLocal(_, _, Parameter(params)):
					paramCandidates = params;
					break;
				default:
			}
		}
		final type = identifierNew.defineType;
		for (candidate in paramCandidates) {
			var allUses = type.getIdentifiers(candidate.name);
			var found = false;
			for (use in allUses) {
				switch (use.type) {
					case FieldVar(_):
						found = true;
						break;
					default:
				}
			}
			if (found) {
				continue;
			}
			params.push(candidate);
		}

		return params;
	}

	static function makeField(context:RefactorContext, extractData:ExtractConstructorParamsData, param:Identifier, asFinal:Bool):String {
		final tokensParam:TokensAtPos = RefactorHelper.findTokensAtPos(extractData.root, param.pos.start);
		if (tokensParam.after == null) {
			return "";
		}
		final lastToken = TokenTreeCheckUtils.getLastToken(tokensParam.after);
		final fullPos = tokensParam.after.getPos();
		if (lastToken.matches(Comma)) {
			fullPos.max = lastToken.pos.min;
		}
		final name = RefactorHelper.extractText(context.converter, extractData.content, fullPos.min, fullPos.max);
		if (asFinal) {
			return 'final $name;\n';
		} else {
			return 'var $name;\n';
		}
	}

	static function makeAssignment(extractData:ExtractConstructorParamsData, param:Identifier):String {
		return 'this.${param.name} = ${param.name};\n';
	}

	static function findFieldInsertPos(context:RefactorContext, extractData:ExtractConstructorParamsData):Int {
		var parent = extractData.tokenNew.parent;
		if (parent == null) {
			return findFieldInsertPosFromType(extractData);
		}
		if (parent.previousSibling != null) {
			final fullPos = parent.previousSibling.getPos();
			return fullPos.max + 1;
		}
		parent = parent.parent;
		if (parent == null) {
			return findFieldInsertPosFromType(extractData);
		}
		return parent.pos.max + 1;
	}

	static function findFieldInsertPosFromType(extractData:ExtractConstructorParamsData):Int {
		final tokens:TokensAtPos = RefactorHelper.findTokensAtPos(extractData.root, extractData.identifierNew.defineType.name.pos.start);
		final typeName = tokens.after;
		if (typeName == null) {
			return -1;
		}
		final brOpen = typeName.access().firstOf(BrOpen).token;
		if (brOpen == null) {
			return -1;
		}
		return brOpen.pos.max + 1;
	}

	static function findAssignmentInsertPos(context:RefactorContext, extractData:ExtractConstructorParamsData):Int {
		if (extractData.superToken != null) {
			final fullPos = extractData.superToken.getPos();
			return fullPos.max + 1;
		}
		final brOpen = extractData.tokenNew.access().firstOf(BrOpen).token;
		if (brOpen == null) {
			return -1;
		}
		return brOpen.pos.max;
	}
}

typedef ExtractConstructorParamsData = {
	var content:String;
	var root:TokenTree;
	var tokenNew:TokenTree;
	var identifierNew:Identifier;
	var parameters:Array<Identifier>;
	var superToken:Null<TokenTree>;
	var file:File;
}
