package refactor.refactor;

import haxe.io.Path;
import js.lib.Promise;
import sys.FileSystem;
import tokentree.TokenTree;
import tokentree.utils.TokenTreeCheckUtils;
import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.discover.IdentifierPos;
import refactor.discover.Type;
import refactor.edits.Changelist;
import refactor.refactor.RefactorHelper.TokensAtPos;

class ExtractType {
	public static function canRefactor(context:CanRefactorContext, isRangeSameScope:Bool):CanRefactorResult {
		final extractData = makeExtractTypeData(context);
		if (extractData == null) {
			return Unsupported;
		}
		return Supported('Extract Type "${extractData.name}" to ${Path.withoutDirectory(extractData.newFileName)}');
	}

	public static function doRefactor(context:RefactorContext):Promise<RefactorResult> {
		final extractData = makeExtractTypeData(context);
		if (extractData == null) {
			return Promise.reject("failed to collect data for extract type");
		}
		// copy header + imports
		final fileHeader = makeHeader(extractData, context);

		// copy type body
		final typePos = extractData.typeToken.getPos();
		final docComment:Null<TokenTree> = TokenTreeCheckUtils.getDocComment(extractData.typeToken);
		if (docComment != null) {
			// expand pos.min to capture doc comment
			if (docComment.pos.min < typePos.min) {
				typePos.min = docComment.pos.min;
			}
		}
		final changelist:Changelist = new Changelist(context);
		final typeText = RefactorHelper.extractText(context.converter, extractData.content, typePos.min, typePos.max);

		// remove code from current file
		changelist.addChange(context.what.fileName, RemoveText({fileName: typePos.file, start: typePos.min, end: typePos.max}), null);

		// create new file
		changelist.addChange(extractData.newFileName, CreateFile(extractData.newFileName), null);

		// copy file header, type and doc comment into new file
		changelist.addChange(extractData.newFileName,
			InsertText(fileHeader + typeText, {fileName: extractData.newFileName, start: 0, end: 0}, Format(0, false)), null);

		// find all places using type and update their imports
		findImportLocations(context, extractData, changelist);

		return Promise.resolve(changelist.execute());
	}

	static function makeExtractTypeData(context:CanRefactorContext):Null<ExtractTypeData> {
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

		final tokens:TokensAtPos = RefactorHelper.findTokensAtPos(root, context.what.posStart);
		if (tokens.before == null || tokens.after == null || tokens.before.index != tokens.after.index) {
			return null;
		}
		final firstImport:Null<TokenTree> = RefactorHelper.findFirstImport(root);

		final typeToken:Null<TokenTree> = findTypeToken(tokens.before);
		if (typeToken == null) {
			return null;
		}

		if (insideConditional(typeToken)) {
			return null;
		}

		final path = new Path(context.what.fileName);

		final nameToken:Null<TokenTree> = typeToken.getFirstChild();
		if (nameToken == null) {
			return null;
		}
		final name:String = nameToken.toString();
		if (name == path.file || path.dir == null) {
			return null;
		}

		final file:Null<File> = context.fileList.getFile(context.what.fileName);
		if (file == null) {
			return null;
		}
		final type:Null<Type> = file.getType(name);
		if (type == null) {
			return null;
		}

		final newFileName:String = Path.join([path.dir, name + ".hx"]);
		if (FileSystem.exists(newFileName)) {
			return null;
		}

		return {
			content: content,
			root: root,
			firstImport: firstImport,
			typeToken: typeToken,
			name: name,
			newFileName: newFileName,
			oldFile: file,
			oldType: type,
		};
	}

	static function findTypeToken(token:Null<TokenTree>):Null<TokenTree> {
		if (token == null) {
			return null;
		}
		switch (token.tok) {
			case Kwd(KwdAbstract) | Kwd(KwdClass) | Kwd(KwdEnum) | Kwd(KwdInterface) | Kwd(KwdTypedef):
				return token;
			case Root:
				return null;
			default:
		}
		var token = token.parent;
		if (token == null) {
			return null;
		}
		switch (token.tok) {
			case Kwd(KwdAbstract) | Kwd(KwdClass) | Kwd(KwdEnum) | Kwd(KwdInterface) | Kwd(KwdTypedef):
				return token;
			default:
				return null;
		}
	}

	static function insideConditional(token:TokenTree):Bool {
		final parent:Null<TokenTree> = token.parent;
		if (parent == null) {
			return false;
		}
		return switch (parent.tok) {
			case Root:
				false;
			case Sharp(s):
				true;
			default:
				insideConditional(parent);
		}
	}

	static function makeHeader(extractData:ExtractTypeData, context:RefactorContext):String {
		var allUses = extractData.oldType.uses.map(use -> {
			final parts = use.name.split(".");
			return parts.shift();
		});
		final imports = RefactorHelper.makeNewFileImports(context, extractData.oldFile, allUses);

		if (extractData.firstImport == null) {
			final lastHeader = RefactorHelper.getLastHeaderToken(extractData.root);
			if (extractData.firstImport == null) {
				return "";
			}
			final pos = lastHeader.getPos();
			return RefactorHelper.extractText(context.converter, extractData.content, 0, pos.min - 1) + "\n";
		}

		final pos = extractData.firstImport.getPos();
		return RefactorHelper.extractText(context.converter, extractData.content, 0, pos.min - 1) + "\n" + imports;
	}

	static function findImportLocations(context:CanRefactorContext, extractData:ExtractTypeData, changelist:Changelist) {
		final oldFullName = extractData.oldType.fullModuleName;
		final oldPackageName = extractData.oldFile.packageIdentifier?.name + "." ?? "";
		final oldModulName = oldPackageName + extractData.oldFile.getMainModulName();
		final newFullName = oldPackageName + extractData.name;
		var allUses:Array<Identifier> = context.nameMap.getIdentifiers(oldFullName);
		for (use in allUses) {
			changelist.addChange(use.file.name, ReplaceText(newFullName, use.pos, NoFormat), use);
		}
		allUses = context.nameMap.getIdentifiers(extractData.name);
		var needsImport:Array<File> = [];
		for (use in allUses) {
			var file = use.file;
			for (imp in file.importList) {
				if (imp.moduleName.name == oldModulName) {
					if (!needsImport.contains(file)) {
						needsImport.push(file);
					}
					break;
				}
			}
		}
		final importNewModule = "import " + newFullName + ";\n";
		for (file in needsImport) {
			var pos:IdentifierPos = {fileName: file.name, start: file.importInsertPos, end: file.importInsertPos};
			changelist.addChange(file.name, InsertText(importNewModule, pos, NoFormat), null);
		}
	}
}

typedef ExtractTypeData = {
	var content:String;
	var root:TokenTree;
	var firstImport:Null<TokenTree>;
	var typeToken:TokenTree;
	var name:String;
	var newFileName:String;
	var oldFile:File;
	var oldType:Type;
}
