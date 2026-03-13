package refactor.discover;

import haxe.Exception;
import haxe.PosInfos;
import haxe.io.Path;
import byte.ByteData;
import haxeparser.HaxeLexer;
import hxparse.ParserError;
import tokentree.TokenTree;
import tokentree.TokenTreeBuilder;
import refactor.discover.File.Import;
import refactor.discover.IdentifierType.TypedefFieldType;

class UsageCollector {
	public function new() {}

	public function parseFile(content:ByteData, context:UsageContext) {
		if (isCached(context)) {
			return;
		}
		var root:Null<TokenTree> = null;
		try {
			var lexer = new HaxeLexer(content, context.fileName);
			var t:Token = lexer.token(haxeparser.HaxeLexer.tok);

			var tokens:Array<Token> = [];
			while (t.tok != Eof) {
				tokens.push(t);
				t = lexer.token(haxeparser.HaxeLexer.tok);
			}
			root = TokenTreeBuilder.buildTokenTree(tokens, content, TypeLevel);
			parseFileWithTokens(root, context);
		} catch (e:ParserError) {
			throw 'failed to parse ${context.fileName} - ParserError: $e (${e.pos})';
		} catch (e:LexerError) {
			throw 'failed to parse ${context.fileName} - LexerError: ${e.msg} (${e.pos})';
		} catch (e:Exception) {
			throw 'failed to parse ${context.fileName} - ${e.details()}';
		}
	}

	public function parseFileWithTokens(root:TokenTree, context:UsageContext) {
		if (isCached(context)) {
			return;
		}
		try {
			var file:File = new File(context.fileName);
			#if debug
			trace("[RefactorCache] parsing " + context.fileName);
			#end
			context.file = file;
			context.type = null;
			var packageName:Null<Identifier> = readPackageName(root, context);
			var imports:Array<Import> = readImports(root, context);
			file.initHeader(packageName, imports, findImportInsertPos(root));
			file.setTypes(readTypes(root, context));
			context.fileList.addFile(file);
			if (context.cache != null) {
				context.cache.storeFile(file);
			}
		} catch (e:Exception) {
			throw 'failed to parse ${context.fileName} - ${e.details()}';
		}
	}

	function isCached(context:UsageContext):Bool {
		if (context.cache != null) {
			var file:Null<File> = context.cache.getFile(context.fileName, context.nameMap);
			if (file != null) {
				return true;
			}
		}
		return false;
	}

	public function updateImportHx(context:UsageContext) {
		for (importHxFile in context.fileList.files) {
			var importHxPath:Path = new Path(importHxFile.name);
			if (importHxPath.file != "import") {
				continue;
			}
			var importHxFolder:String = importHxPath.dir;
			for (file in context.fileList.files) {
				if (file.name == importHxFile.name) {
					continue;
				}
				var path:Path = new Path(file.name);
				if (!path.dir.startsWith(importHxFolder)) {
					continue;
				}
				file.importHxFile = importHxFile;
			}
		}
	}

	function findImportInsertPos(root:TokenTree):Int {
		if (!root.hasChildren()) {
			return 0;
		}
		var pos:Int = 0;
		for (child in root.children) {
			switch (child.tok) {
				case Kwd(KwdPackage):
					pos = child.getPos().max + 1;
				case Kwd(KwdImport) | Kwd(KwdUsing):
					return child.pos.min;
				case Comment(_) | CommentLine(_):
					pos = child.pos.max + 1;
				default:
					return child.pos.min;
			}
		}
		return pos;
	}

	function readPackageName(root:TokenTree, context:UsageContext):Identifier {
		var packages:Array<TokenTree> = root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			return switch (token.tok) {
				case Kwd(KwdPackage):
					FoundSkipSubtree;
				default:
					SkipSubtree;
			}
		});
		if (packages.length != 1) {
			return null;
		}
		var token:TokenTree = packages[0].getFirstChild();
		return makeIdentifier(context, token, PackageName, null);
	}

	function readImports(root:TokenTree, context:UsageContext):Array<Import> {
		var imports:Array<Import> = [];

		var importTokens:Array<TokenTree> = root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			return switch (token.tok) {
				case Kwd(KwdImport) | Kwd(KwdUsing):
					FoundSkipSubtree;
				case Sharp(_):
					GoDeeper;
				default:
					SkipSubtree;
			}
		});
		for (importToken in importTokens) {
			imports.push(readImport(importToken, context));
		}
		return imports;
	}

	function readImport(token:TokenTree, context:UsageContext):Import {
		var pack:Array<String> = [];
		var alias:Null<Identifier> = null;
		var type:IdentifierType = switch (token.tok) {
			case Kwd(KwdImport):
				ImportModul;
			case Kwd(KwdUsing):
				UsingModul;
			default:
				null;
		}
		if (type == null) {
			return null;
		}
		var starImport:Bool = false;
		token = token.getFirstChild();
		var pos:IdentifierPos = makePosition(context.fileName, token);

		while (token != null) {
			switch (token.tok) {
				case Const(CIdent("as")) | Binop(OpIn):
					alias = makeIdentifier(context, token.getFirstChild(), ImportAlias, null);
					break;
				case Kwd(_) | Const(CIdent(_)):
					pack.push(token.toString());
					pos.end = token.pos.max;
				case Dot:
				case Binop(OpMult):
					starImport = true;
				case Semicolon:
					break;
				default:
					return null;
			}
			token = token.getFirstChild();
		}

		var importIdentifier:Identifier = new Identifier(type, pack.join("."), pos, context.nameMap, context.file, null);
		if (alias != null) {
			importIdentifier.addUse(alias.parent);
		}
		return {
			moduleName: importIdentifier,
			alias: alias,
			starImport: starImport
		}
	}

	function readTypes(root:TokenTree, context:UsageContext):Array<Type> {
		var typeTokens:Array<TokenTree> = root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			return switch (token.tok) {
				case Kwd(KwdAbstract) | Kwd(KwdClass) | Kwd(KwdEnum) | Kwd(KwdInterface) | Kwd(KwdTypedef) | Kwd(KwdVar) | Kwd(KwdFinal) | Kwd(KwdFunction):
					FoundSkipSubtree;
				case Sharp(_):
					GoDeeper;
				default:
					SkipSubtree;
			}
		});
		var types:Array<Type> = [];
		for (typeToken in typeTokens) {
			types.push(readType(typeToken, context));
		}
		return types;
	}

	function readType(token:TokenTree, context:UsageContext):Type {
		var type:IdentifierType = switch (token.tok) {
			case Kwd(KwdAbstract):
				Abstract;
			case Kwd(KwdClass):
				Class;
			case Kwd(KwdEnum):
				Enum;
			case Kwd(KwdInterface):
				Interface;
			case Kwd(KwdTypedef):
				Typedef;
			case Kwd(KwdVar) | Kwd(KwdFinal):
				ModuleLevelStaticVar;
			case Kwd(KwdFunction):
				ModuleLevelStaticMethod;
			default:
				null;
		}
		if (type == null) {
			return null;
		}
		var nameToken:TokenTree = token.getFirstChild();
		var newType:Type = new Type(context.file);
		context.type = newType;
		var identifier:Identifier = makeIdentifier(context, nameToken, type, null);
		if (identifier == null) {
			return null;
		}
		newType.name = identifier;
		context.typeList.addType(newType);

		switch (type) {
			case Abstract:
				addAbstractFields(context, identifier, nameToken);
			case Class:
				addClassInterface(context, identifier, nameToken);
			case Enum:
				readEnum(context, identifier, nameToken.getFirstChild());
			case Interface:
				addClassInterface(context, identifier, nameToken);
				if (identifier.uses != null) {
					for (use in identifier.uses) {
						switch (use.type) {
							case Property:
								use.type = InterfaceProperty;
							case FieldVar(_):
								use.type = InterfaceVar;
							case Method(_):
								use.type = InterfaceMethod;
							default:
						}
					}
				}
			case Typedef:
				readTypedef(context, identifier, nameToken);
			case ModuleLevelStaticVar:
				readVarInit(context, identifier, nameToken);
			case ModuleLevelStaticMethod:
				readMethod(context, identifier, nameToken);

			default:
		}
		readStrings(context, identifier, nameToken);

		return newType;
	}

	function readStrings(context:UsageContext, identifier:Identifier, token:TokenTree) {
		token.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			return switch (token.tok) {
				case Const(CString(s, DoubleQuotes)):
					var regEx:EReg = ~/^[a-z][a-zA-Z0-9]+(\.[a-z][a-zA-Z0-9]+)*(|\.[A-Z][a-zA-Z0-9]+)$/;
					if (regEx.match(s)) {
						var pos:IdentifierPos = {
							fileName: context.fileName,
							start: token.pos.min + 1,
							end: token.pos.max - 1
						};
						final newIdentifier = new Identifier(StringConst, s, pos, context.nameMap, context.file, context.type);
						final parentIdentifier = findParentIdentifier(context, token);
						if (parentIdentifier != null) {
							parentIdentifier.addUse(newIdentifier);
						}
						identifier.addUse(newIdentifier);
					}
					SkipSubtree;
				case Const(CString(s, SingleQuotes)):
					var parentIdentifier = findParentIdentifier(context, token);
					if (parentIdentifier == null) {
						parentIdentifier = identifier;
					}
					readStringInterpolation(context, parentIdentifier, token, s);
					SkipSubtree;
				default:
					GoDeeper;
			}
		});
	}

	function readStringInterpolation(context:UsageContext, identifier:Identifier, token:TokenTree, text:String) {
		var start:Int = 0;
		var index:Int;
		while ((index = text.indexOf("${", start)) >= 0) {
			if (isDollarEscaped(text, index)) {
				start = index + 1;
				continue;
			}
			start = index + 1;
			var indexEnd:Int = text.indexOf("}", index + 2);
			var fragment:String = text.substring(index + 2, indexEnd);
			if (fragment.indexOf("{") >= 0) {
				continue;
			}
			readInterpolatedFragment(context, identifier, fragment, token.pos.min + 1 + start + 1);
			start = indexEnd;
		}
		start = 0;
		var nameRegEx:EReg = ~/^[a-z][a-zA-Z0-9]*/;
		while ((index = text.indexOf("$", start)) >= 0) {
			if (index + 1 >= text.length) {
				break;
			}
			start = index + 1;
			if (nameRegEx.match(text.substr(start))) {
				var matchedText:String = nameRegEx.matched(0);
				var pos:IdentifierPos = {
					fileName: token.pos.file,
					start: token.pos.min + start + 1,
					end: token.pos.min + start + matchedText.length + 1
				};
				identifier.addUse(new Identifier(Access, matchedText, pos, context.nameMap, context.file, context.type));
			}
		}
	}

	function findParentIdentifier(context:UsageContext, stringToken:TokenTree):Null<Identifier> {
		var parent = stringToken.parent;
		while (parent != null) {
			switch (parent.tok) {
				case Kwd(KwdFunction) | Kwd(KwdVar) | Kwd(KwdFinal) | Kwd(KwdAbstract) | Kwd(KwdClass) | Kwd(KwdEnum) | Kwd(KwdInterface) | Kwd(KwdTypedef):
					var child = parent.getFirstChild();
					if (child != null) {
						return context.type.findIdentifier(child.pos.min);
					}
				case Root | null:
					break;
				default:
			}
			parent = parent.parent;
		}
		return null;
	}

	function isDollarEscaped(text:String, index:Int):Bool {
		var escaped:Bool = false;
		while (--index >= 0) {
			if (text.fastCodeAt(index) != "$".code) {
				return escaped;
			}
			escaped = !escaped;
		}
		return escaped;
	}

	function readInterpolatedFragment(context:UsageContext, identifier:Identifier, text:String, offset:Int) {
		var root:Null<TokenTree> = null;
		try {
			var content:ByteData = ByteData.ofString(text);
			var lexer = new HaxeLexer(content, context.fileName);
			var t:Token = lexer.token(haxeparser.HaxeLexer.tok);

			var tokens:Array<Token> = [];
			while (t.tok != Eof) {
				t.pos.min += offset;
				t.pos.max += offset;
				tokens.push(t);
				t = lexer.token(haxeparser.HaxeLexer.tok);
			}
			tokentree.TokenStream.MODE = Relaxed;
			root = TokenTreeBuilder.buildTokenTree(tokens, content, ExpressionLevel);
			readExpression(context, identifier, root);
		} catch (e:ParserError) {
			throw 'failed to parse ${context.fileName} - ParserError: $e (${e.pos})';
		} catch (e:LexerError) {
			throw 'failed to parse ${context.fileName} - LexerError: ${e.msg} (${e.pos})';
		} catch (e:Exception) {
			throw 'failed to parse ${context.fileName} - ${e.details()}';
		}
	}

	function readEnum(context:UsageContext, identifier:Identifier, token:TokenTree) {
		if (!token.hasChildren()) {
			return;
		}
		for (child in token.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Const(CIdent(_)):
					var enumField:Identifier = makeIdentifier(context, child, EnumField([]), identifier);
					if (enumField == null) {
						continue;
					}
					if (!child.hasChildren()) {
						continue;
					}
					var pOpen:TokenTree = child.getFirstChild();
					if (!pOpen.matches(POpen)) {
						continue;
					}
					var params:Array<Identifier> = readParameter(context, enumField, pOpen, pOpen.pos.max);
					enumField.type = EnumField(params);
					copyUsesToParent(identifier, enumField);
				case Sharp("if") | Sharp("elseif"):
					readExpression(context, identifier, child.getFirstChild());
					for (index in 1...child.children.length - 1) {
						switch (child.children[index].tok) {
							case Sharp(_):
							default:
								readEnum(context, identifier, child.children[index]);
						}
					}
				case Sharp("else"):
					readEnum(context, identifier, child);
				default:
					continue;
			}
		}
	}

	function readTypedef(context:UsageContext, identifier:Identifier, token:TokenTree) {
		if (!token.hasChildren()) {
			return;
		}
		var assignToken:Null<TokenTree> = token.getFirstChild();
		if (assignToken == null || !assignToken.tok.match(Binop(OpAssign)) || !assignToken.hasChildren()) {
			return;
		}
		for (child in assignToken.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case BrOpen:
					readAnonStructure(context, identifier, child);
				case Const(CIdent(_)):
					final ident = makeIdentifier(context, child, TypedefBase, identifier);
					for (identChild in findIdentifierChilds(child)) {
						switch (identChild.tok) {
							case Semicolon:
							case Binop(OpLt):
								addTypeParameter(context, ident, identChild);
							case Binop(OpAnd):
								readExpression(context, identifier, identChild);
							case Arrow:
								readTypeHint(context, ident, identChild, TypeHint);
							default:
						}
					}
				default:
			}
		}
	}

	function addClassInterface(context:UsageContext, identifier:Identifier, token:Null<TokenTree>) {
		if (token == null || !token.hasChildren()) {
			return;
		}
		for (child in token.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Kwd(KwdExtends):
					makeIdentifier(context, child.getFirstChild(), Extends, identifier);
				case Kwd(KwdImplements):
					makeIdentifier(context, child.getFirstChild(), Implements, identifier);
				case BrOpen:
					addFields(context, identifier, child);
				case Binop(OpLt):
					addTypeParameter(context, identifier, child);
				case At:
					readMetadata(context, identifier, child);
				case Kwd(KwdPrivate) | Kwd(KwdExtern) | Kwd(KwdAbstract):
				case Sharp(_):
					readExpression(context, identifier, child);
				default:
			}
		}
	}

	function addFields(context:UsageContext, identifier:Identifier, token:Null<TokenTree>) {
		if (token == null || !token.hasChildren()) {
			return;
		}
		var first = true;
		for (child in token.children) {
			if (first) {
				first = false;
				final parent = child.parent;
				if (parent != null) {
					switch (parent.tok) {
						case Sharp("if") | Sharp("elseif"):
							continue;
						default:
					}
				}
			}
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Sharp(_):
					addFields(context, identifier, child);
				case Kwd(KwdFunction):
					var nameToken:TokenTree = child.getFirstChild();
					var method:Identifier = makeIdentifier(context, nameToken, Method(nameToken.access().firstOf(Kwd(KwdStatic)).exists()), identifier);
					readMethod(context, method, nameToken);
					copyUsesToParent(identifier, method);
				case Kwd(KwdVar) | Kwd(KwdFinal):
					readVar(context, identifier, child, FieldVar(child.access().firstChild().firstOf(Kwd(KwdStatic)).exists()));
				case Kwd(KwdPublic) | Kwd(KwdPrivate) | Kwd(KwdInline) | Kwd(KwdStatic) | Kwd(KwdExtern) | Kwd(KwdOverride) | Kwd(KwdMacro) | Kwd(KwdAbstract):
				case BrClose:
				case Semicolon:
				case Binop(_):
					readExpression(context, identifier, child);
				case At:
					readMetadata(context, identifier, child);
				default:
			}
		}
	}

	function addAbstractFields(context:UsageContext, identifier:Identifier, token:Null<TokenTree>) {
		if (token == null || !token.hasChildren()) {
			return;
		}
		var staticVars:Bool = false;
		var block:Null<TokenTree> = null;
		for (child in token.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Kwd(KwdEnum):
					staticVars = true;
				case Const(CIdent("from")):
					makeIdentifier(context, child.getFirstChild(), AbstractFrom, identifier);
				case Const(CIdent("to")):
					makeIdentifier(context, child.getFirstChild(), AbstractTo, identifier);
				case POpen:
					readTypeHint(context, identifier, child, AbstractOver);
				case BrOpen:
					block = child;
					break;
				default:
			}
		}
		if (block == null) {
			return;
		}
		for (child in block.children) {
			switch (child.tok) {
				case Kwd(KwdFunction):
					var nameToken:TokenTree = child.getFirstChild();
					var method:Identifier = makeIdentifier(context, nameToken, Method(nameToken.access().firstOf(Kwd(KwdStatic)).exists()), identifier);
					readMethod(context, method, nameToken);
					copyUsesToParent(identifier, method);
				case Kwd(KwdVar) | Kwd(KwdFinal):
					readVar(context, identifier, child, FieldVar(child.access().firstChild().firstOf(Kwd(KwdStatic)).exists()));
				default:
			}
		};
	}

	function readVar(context:UsageContext, identifier:Identifier, child:Null<TokenTree>, type:IdentifierType):Void {
		var nameToken:TokenTree = child.getFirstChild();
		while (nameToken != null) {
			var variable:Identifier = makeIdentifier(context, nameToken, type, identifier);
			if (variable == null) {
				return;
			}
			if (!nameToken.hasChildren()) {
				copyUsesToParent(identifier, variable);
				return;
			}
			for (nameChild in nameToken.children) {
				switch (nameChild.tok) {
					case POpen:
						variable.type = Property;
					case DblDot:
						readTypeHint(context, variable, nameChild, TypeHint);
					case Binop(OpAssign) | Binop(OpAssignOp(_)):
						readExpression(context, variable, nameChild);
					case Kwd(KwdPublic) | Kwd(KwdPrivate) | Kwd(KwdInline) | Kwd(KwdStatic) | Kwd(KwdExtern) | Kwd(KwdOverride) | Kwd(KwdMacro) |
						Kwd(KwdAbstract):
					case At:
						readMetadata(context, variable, nameChild);
					case Comma:
					case Semicolon:
					case Comment(_) | CommentLine(_):
					default:
				}
			}
			copyUsesToParent(identifier, variable);
			nameToken = nameToken.nextSibling;
		}
	}

	function readMetadata(context:UsageContext, identifier:Identifier, token:TokenTree) {
		if (token == null) {
			return;
		}
		token = token.getFirstChild();
		if (token == null) {
			return;
		}
		switch (token.tok) {
			case Const(_):
				var metadata:Identifier = makeIdentifier(context, token, Meta, identifier);
				readExpression(context, metadata, findIdentifierChild(token));
				copyUsesToParent(identifier, metadata);
			case Kwd(KwdFinal):
			case DblDot:
				readMetadata(context, identifier, token);
			default:
		}
	}

	function readVarInit(context:UsageContext, identifier:Identifier, token:TokenTree) {
		for (child in token.children) {
			switch (child.tok) {
				case Binop(OpAssign):
					readExpression(context, identifier, child.getFirstChild());
				case DblDot:
					switch (TokenTreeCheckUtils.getColonType(child)) {
						case TypeHint:
							readTypeHint(context, identifier, child, TypeHint);
						case SwitchCase | TypeCheck | Ternary | ObjectLiteral | At | Unknown:
					}
				default:
			}
		}
	}

	function readMethod(context:UsageContext, identifier:Identifier, token:TokenTree) {
		var ignore:Bool = true;
		var fullPos:Position = token.getPos();
		for (child in token.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Binop(OpLt):
					addTypeParameter(context, identifier, child);
				case POpen:
					readParameter(context, identifier, child, fullPos.max);
					ignore = false;
				case DblDot:
					readTypeHint(context, identifier, child, TypeHint);
				case BrOpen:
					if (ignore) {
						continue;
					}
					readBlock(context, identifier, child);
				case At:
					readMetadata(context, identifier, child);
				case Kwd(KwdPublic) | Kwd(KwdPrivate) | Kwd(KwdInline) | Kwd(KwdStatic) | Kwd(KwdExtern) | Kwd(KwdOverride) | Kwd(KwdMacro) | Kwd(KwdAbstract):
				default:
					if (ignore) {
						continue;
					}
					readExpression(context, identifier, child);
			}
		}
	}

	function readObjectLiteral(context:UsageContext, identifier:Identifier, token:TokenTree) {
		if (!token.hasChildren()) {
			return;
		}
		var names:Array<String> = [];
		for (child in token.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Const(CIdent(s)):
					names.push(s);
					var field:Identifier = makeIdentifier(context, child, StructureField(names), identifier);
					readExpression(context, field, findIdentifierChild(child));
					copyUsesToParent(identifier, field);
				case BrClose:
					break;
				default:
					break;
			}
		}
	}

	function readBlock(context:UsageContext, identifier:Identifier, token:TokenTree) {
		if (!token.hasChildren()) {
			return;
		}

		var fullPos:Position = token.getPos();
		var scopeEnd:Int = fullPos.max;
		for (child in token.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Kwd(KwdVar) | Kwd(KwdFinal):
					readVar(context, identifier, child, ScopedLocal(child.getPos().max, scopeEnd, Var));
				case Kwd(KwdFunction):
					child = child.getFirstChild();
					var method:Identifier = makeIdentifier(context, child, ScopedLocal(child.pos.min, scopeEnd, Var), identifier);
					if (method == null) {
						readMethod(context, identifier, child);
					} else {
						readMethod(context, method, child);
						copyUsesToParent(identifier, method);
					}
				case Dot | QuestionDot:
				case Semicolon:
				default:
					readExpression(context, identifier, child);
			}
		}
	}

	function readIdentifier(context:UsageContext, identifier:Identifier, token:Null<TokenTree>, ?pos:PosInfos) {
		if (token == null) {
			return;
		}
		var parent:TokenTree = token.parent;
		switch (parent.tok) {
			case Dot | QuestionDot:
				var prev:Null<TokenTree> = parent.previousSibling;
				if (prev != null) {
					switch (prev.tok) {
						case BkOpen | BkClose | POpen | PClose:
							var accessIdent:Identifier = null;
							var accessParent = parent?.parent?.parent;
							if (accessParent != null) {
								accessIdent = context.type.findIdentifier(accessParent.pos.min);
							}
							var identType:IdentifierType = Access;
							if (accessIdent != null) {
								identType = ArrayAccess(accessIdent);
							}
							makeIdentifier(context, token, identType, identifier);
							for (identChild in findIdentifierChilds(token)) {
								switch (identChild.tok) {
									case POpen:
										readCallParams(context, identifier, identChild);
									case BkOpen:
										readCallParams(context, identifier, identChild);
									case Binop(_):
										readExpression(context, identifier, identChild);
									case Dot | QuestionDot:
										readExpression(context, identifier, identChild);
									case Question:
										if (TokenTreeCheckUtils.isTernary(identChild)) {
											readExpression(context, identifier, identChild);
										}
									case Comment(_) | CommentLine(_):
									case Unop(_):
									default:
								}
							}
						default:
					}
				}
			case POpen:
				switch (TokenTreeCheckUtils.getPOpenType(parent)) {
					case Parameter:
						var posScope = parent.getPos();
						final parameterIdent = makeIdentifier(context, token, ScopedLocal(posScope.min, posScope.max, Parameter([])), identifier);
						for (identChild in findIdentifierChilds(token)) {
							switch (identChild.tok) {
								case Comma:
								case DblDot:
									switch (TokenTreeCheckUtils.getColonType(identChild)) {
										case TypeHint:
											readTypeHint(context, parameterIdent, identChild, TypeHint);
											copyUsesToParent(identifier, parameterIdent);
										case TypeCheck:
											readExpression(context, identifier, identChild);
										case SwitchCase | Ternary | ObjectLiteral | At:
										case Unknown:
											readTypeHint(context, parameterIdent, identChild, TypeHint);
											copyUsesToParent(identifier, parameterIdent);
									}
								case Binop(OpLt):
									addTypeParameter(context, parameterIdent, identChild);
									copyUsesToParent(identifier, parameterIdent);
								default:
							}
						}
						return;

					case At | Call | SwitchCondition | WhileCondition | IfCondition | SharpCondition | Catch | ForLoop | Expression:
						final accessIdent = makeIdentifier(context, token, Access, identifier);
						for (identChild in findIdentifierChilds(token)) {
							switch (identChild.tok) {
								case Comment(_) | CommentLine(_):
								case Comma | Dot | QuestionDot | Unop(_):
								case Binop(_):
									readExpression(context, identifier, identChild);
								case Arrow:
									readExpression(context, identifier, identChild);
								case POpen:
									readCallParams(context, identifier, identChild);
								case BkOpen:
									readCallParams(context, identifier, identChild);
								case DblDot:
									switch (TokenTreeCheckUtils.getColonType(identChild)) {
										case TypeHint:
											readTypeHint(context, accessIdent, identChild, TypeHint);
											copyUsesToParent(identifier, accessIdent);
										case TypeCheck:
											readExpression(context, identifier, identChild);
										case SwitchCase | Ternary | ObjectLiteral | At:
										case Unknown:
											readTypeHint(context, accessIdent, identChild, TypeHint);
											copyUsesToParent(identifier, accessIdent);
									}
								case Const(CIdent("is")):
									final child = identChild.getFirstChild();
									if (child != null) {
										readExpression(context, identifier, child);
									}
								case Question:
									if (TokenTreeCheckUtils.isTernary(identChild)) {
										readExpression(context, identifier, identChild);
									}
								default:
							}
						}
						return;
				}
			default:
				var ident = makeIdentifier(context, token, Access, identifier);
				if (ident == null) {
					ident = identifier;
				}
				var directChildrenDone:Bool = false;
				for (identChild in findIdentifierChilds(token)) {
					if (identChild == token.getFirstChild()) {
						directChildrenDone = true;
					}
					switch (identChild.tok) {
						case Comment(_) | CommentLine(_):
						case Unop(_) | Semicolon | Comma:
						case POpen | BkOpen | Dot | QuestionDot:
							readCallParams(context, ident, identChild);
							copyUsesToParent(identifier, ident);
						case Binop(_):
							readExpression(context, ident, identChild);
							copyUsesToParent(identifier, ident);
						case DblDot:
							switch (TokenTreeCheckUtils.getColonType(identChild)) {
								case SwitchCase | Ternary | ObjectLiteral | At:
								case TypeHint:
									readTypeHint(context, ident, identChild, TypeHint);
									copyUsesToParent(identifier, ident);
								case TypeCheck:
									readExpression(context, identifier, identChild);
								case Unknown:
									readTypeHint(context, ident, identChild, TypeHint);
									copyUsesToParent(identifier, ident);
							}
						case Question:
							if (TokenTreeCheckUtils.isTernary(identChild)) {
								readExpression(context, identifier, identChild);
							}
						case Spread:
							readExpression(context, identifier, identChild);
						case Arrow:
							readExpression(context, identifier, identChild);
						default:
					}
				}
				if (directChildrenDone) {
					return;
				}
		}
		if (token.hasChildren()) {
			for (child in token.children) {
				switch (child.tok) {
					case Comment(_) | CommentLine(_):
					case Dot | QuestionDot | Comma:
					case POpen:
						readCallParams(context, identifier, child);
					case Binop(OpAssign):
					default:
						readExpression(context, identifier, child);
				}
			}
		}
	}

	function readCallParams(context:UsageContext, identifier:Identifier, token:Null<TokenTree>) {
		if (token == null) {
			return;
		}
		if (!token.hasChildren()) {
			return;
		}
		for (child in token.children) {
			readExpression(context, identifier, child);
		}
	}

	function readExpression(context:UsageContext, identifier:Identifier, token:Null<TokenTree>, ?pos:PosInfos) {
		if (token == null) {
			return;
		}

		switch (token.tok) {
			case Comment(_) | CommentLine(_):
			case Const(CIdent(_)):
				readIdentifier(context, identifier, token);
				return;
			case Binop(_):
			case Kwd(KwdVar):
				final fullPos:Position = token.parent.getPos();
				readVar(context, identifier, token, ScopedLocal(token.getPos().max, fullPos.max, Var));
				return;
			case Kwd(KwdFunction):
				var fullPos:Position = token.parent.getPos();
				var scopeEnd:Int = fullPos.max;
				var child:TokenTree = token.getFirstChild();
				switch (child.tok) {
					case Const(_):
						var method:Null<Identifier> = makeIdentifier(context, child, ScopedLocal(child.pos.min, scopeEnd, Var), identifier);
						readMethod(context, method, child);
						copyUsesToParent(identifier, method);
					default:
						readMethod(context, identifier, token);
				}
				return;
			case Kwd(KwdThis):
				final thisIdent = makeIdentifier(context, token, Access, identifier);
				for (identChild in findIdentifierChilds(token)) {
					switch (identChild.tok) {
						case Binop(_):
							readExpression(context, identifier, identChild);
						case Unop(_):
						case POpen:
							readCallParams(context, thisIdent, identChild);
							copyUsesToParent(identifier, thisIdent);
						case BkOpen:
							readCallParams(context, thisIdent, identChild);
							copyUsesToParent(identifier, thisIdent);
						case Dot | QuestionDot:
							readIdentifier(context, thisIdent, identChild);
							copyUsesToParent(identifier, thisIdent);
						case Comment(_) | CommentLine(_):
						case Semicolon:
						case Comma:
						case DblDot:
							readTypeHint(context, thisIdent, identChild, TypeHint);
							copyUsesToParent(identifier, thisIdent);
						case Question:
							if (TokenTreeCheckUtils.isTernary(identChild)) {
								readExpression(context, identifier, identChild);
							}
						default:
					}
				}
				return;
			case BrOpen:
				switch (TokenTreeCheckUtils.getBrOpenType(token)) {
					case Block:
						readBlock(context, identifier, token);
					case TypedefDecl:
						readBlock(context, identifier, token);
					case ObjectDecl:
						readObjectLiteral(context, identifier, token);
					case AnonType:
						readBlock(context, identifier, token);
					case Unknown:
						readBlock(context, identifier, token);
				}
				return;
			case Kwd(KwdSwitch):
				readSwitch(context, identifier, token);
				return;
			case Kwd(KwdFor):
				readFor(context, identifier, token);
				return;
			case Semicolon:
				return;
			default:
		}
		if (token.hasChildren()) {
			for (child in token.children) {
				readExpression(context, identifier, child);
			}
		}
	}

	function readSwitch(context:UsageContext, identifier:Identifier, token:TokenTree) {
		if (!token.hasChildren()) {
			return;
		}
		var index:Int = 0;
		if (identifier != null && identifier.uses != null) {
			index = identifier.uses.length;
		}
		readExpression(context, identifier, token.getFirstChild());
		var switchIdent:Null<Identifier> = identifier;
		if (identifier != null && identifier.uses != null && (identifier.uses.length > index)) {
			switchIdent = identifier.uses[index];
		}
		var brOpen:Null<TokenTree> = token.access().firstOf(BrOpen).token;
		if ((brOpen == null) || (!brOpen.hasChildren())) {
			return;
		}
		for (child in brOpen.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Kwd(KwdCase):
					readCase(context, switchIdent, child);
				case Kwd(KwdDefault):
					if (child.hasChildren()) {
						readBlock(context, switchIdent, child.getFirstChild());
					}
				case Sharp(_):
					readExpression(context, identifier, child.getFirstChild());
					for (index in 1...child.children.length - 1) {
						switch (child.children[index].tok) {
							case Sharp(_):
							case Kwd(KwdCase):
								readCase(context, switchIdent, child.children[index]);
							case Kwd(KwdDefault):
								if (child.hasChildren()) {
									readBlock(context, switchIdent, child.children[index].getFirstChild());
								}
							default:
						}
					}
				default:
					break;
			}
		}
		copyUsesToParent(identifier, switchIdent);
	}

	function readFor(context:UsageContext, identifier:Identifier, token:TokenTree) {
		if (!token.hasChildren()) {
			return;
		}
		var skip:Bool = true;
		for (child in token.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case POpen:
					var fullPos:Position = token.getPos();
					var scopeEnd:Int = fullPos.max;
					readForIteration(context, identifier, child.getFirstChild(), scopeEnd);
					skip = false;
				default:
					if (skip) {
						continue;
					}
					readExpression(context, identifier, child);
			}
		}
	}

	function readForIteration(context:UsageContext, identifier:Identifier, token:TokenTree, scopeEnd:Int) {
		var loopIdentifiers:Array<Identifier> = [];

		var pClose:Null<TokenTree> = token.access().parent().firstOf(PClose).token;
		var scopeStart:Int = token.pos.min;
		if (pClose != null) {
			scopeStart = pClose.pos.max;
		}
		var ident:Identifier = makeIdentifier(context, token, ScopedLocal(scopeStart, scopeEnd, ForLoop(loopIdentifiers)), identifier);
		loopIdentifiers.push(ident);
		if (!token.hasChildren()) {
			return;
		}
		for (child in token.children) {
			switch (child.tok) {
				case Binop(OpArrow):
					ident = makeIdentifier(context, child.getFirstChild(), ScopedLocal(scopeStart, scopeEnd, ForLoop(loopIdentifiers)), identifier);
					loopIdentifiers.push(ident);
				default:
					readExpression(context, ident, child);
					copyUsesToParent(identifier, ident);
					if (ident?.uses != null) {
						for (use in ident.uses) {
							switch (use.type) {
								case Access:
									use.type = ForIterator;
								default:
							}
							loopIdentifiers.push(use);
						}
					}
			}
		}
	}

	function readCase(context:UsageContext, identifier:Identifier, token:TokenTree) {
		if (!token.hasChildren()) {
			return;
		}
		var fullPos:Position = token.getPos();
		var scopeEnd:Int = fullPos.max;

		for (child in token.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Const(CIdent(_)):
					readCaseConst(context, identifier, child, scopeEnd);
				case Kwd(KwdVar):
					child = child.getFirstChild();
					makeIdentifier(context, child, ScopedLocal(child.pos.min, scopeEnd, CaseCapture(null, 0)), identifier);
				case BkOpen:
					readCaseArray(context, identifier, child, scopeEnd);
				case BrOpen:
					readCaseStructure(context, identifier, child, scopeEnd);
				case DblDot:
					readBlock(context, identifier, child);
					break;
				default:
			}
		}
	}

	function readCaseConst(context:UsageContext, identifier:Identifier, token:TokenTree, scopeEnd:Int) {
		var caseIdent:Identifier = makeIdentifier(context, token, CaseLabel(identifier), identifier);
		if (!token.hasChildren()) {
			return;
		}
		final pOpen = findIdentifierChild(token);
		if (pOpen == null) {
			copyUsesToParent(identifier, caseIdent);
			return;
		}
		readCasePOpen(context, caseIdent, pOpen, scopeEnd);
		if (caseIdent != null) {
			copyUsesToParent(identifier, caseIdent);
		}
	}

	function readCasePOpen(context:UsageContext, caseIdent:Identifier, pOpen:TokenTree, scopeEnd:Int) {
		switch (pOpen.tok) {
			case BkOpen:
				readCaseArray(context, caseIdent, pOpen, scopeEnd);
			case POpen:
				readCaseParameter(context, caseIdent, pOpen, scopeEnd);
			default:
		}
	}

	function readCaseParameter(context:UsageContext, identifier:Identifier, token:TokenTree, scopeEnd:Int):Array<Identifier> {
		var params:Array<Identifier> = [];
		var index:Int = 0;
		for (child in token.children) {
			switch (child.tok) {
				case Question:
					child = child.getFirstChild();
					var paramIdent:Identifier = makeIdentifier(context, child, ScopedLocal(child.pos.min, scopeEnd, Parameter(params)), identifier);
					params.push(paramIdent);
				case Const(CIdent(s)):
					var paramIdent:Identifier = makeIdentifier(context, child, ScopedLocal(child.pos.min, scopeEnd, CaseCapture(identifier, index)),
						identifier);
					params.push(paramIdent);
					index++;
					final pOpen = findIdentifierChild(child);
					if (pOpen == null) {
						continue;
					}
					if (pOpen.parent == child) {
						continue;
					}
					readCasePOpen(context, paramIdent, pOpen, scopeEnd);
				case Const(_):
					index++;
				case BkOpen:
					readCaseArray(context, identifier, child, scopeEnd);
				case PClose:
					break;
				default:
			}
		}
		return params;
	}

	function readCaseArray(context:UsageContext, identifier:Identifier, token:TokenTree, scopeEnd:Int) {
		if (!token.hasChildren()) {
			return;
		}
		var index = 0;
		for (child in token.children) {
			readCaseConst(context, identifier, child, scopeEnd);
			index++;
		}
	}

	function readCaseStructure(context:UsageContext, identifier:Identifier, token:TokenTree, scopeEnd:Int) {
		if (!token.hasChildren()) {
			return;
		}
		for (child in token.children) {
			switch (child.tok) {
				case Const(_):
					var field:Null<Identifier> = makeIdentifier(context, child, StructureField([]), identifier);
					if (field == null) {
						continue;
					}
					if (!child.hasChildren()) {
						continue;
					}
					var valueChild:TokenTree = child.getFirstChild();
					switch (valueChild.tok) {
						case Kwd(_) | Const(_) | Dollar(_) | Unop(_) | Binop(_):
							readExpression(context, field, valueChild);
							copyUsesToParent(identifier, field);
						case BkOpen | BrOpen:
							readCaseStructure(context, field, valueChild, scopeEnd);
							copyUsesToParent(identifier, field);
							continue;
						default:
					}
					if (field.uses != null) {
						for (use in field.uses) {
							use.type = ScopedLocal(use.pos.start, scopeEnd, CaseCapture(identifier, 0));
						}
					}
				default:
					break;
			}
		}
	}

	function readParameter(context:UsageContext, identifier:Identifier, token:TokenTree, scopeEnd:Int):Array<Identifier> {
		var params:Array<Identifier> = [];
		for (child in token.children) {
			switch (child.tok) {
				case Question:
					child = child.getFirstChild();
					final paramIdent:Identifier = makeIdentifier(context, child, ScopedLocal(child.pos.min, scopeEnd, Parameter(params)), identifier);
					for (identChild in findIdentifierChilds(child)) {
						switch (identChild.tok) {
							case DblDot:
								switch (TokenTreeCheckUtils.getColonType(identChild)) {
									case TypeHint:
										readTypeHint(context, paramIdent, identChild, TypeHint);
										copyUsesToParent(identifier, paramIdent);
									case TypeCheck:
										readExpression(context, identifier, identChild);
									case Ternary:
										readExpression(context, identifier, identChild);
									case SwitchCase | ObjectLiteral | At | Unknown:
								}
							case Binop(OpAssign):
								readExpression(context, identifier, identChild);
							case Comma:
							default:
						}
					}
					copyUsesToParent(identifier, paramIdent);
					params.push(paramIdent);
				case Const(CIdent(_)):
					var paramIdent:Identifier = makeIdentifier(context, child, ScopedLocal(child.pos.min, scopeEnd, Parameter(params)), identifier);
					params.push(paramIdent);
					for (identChild in findIdentifierChilds(child)) {
						switch (identChild.tok) {
							case DblDot:
								readTypeHint(context, paramIdent, identChild, TypeHint);
							case Comma:
								if (identChild.parent == child) {
									break;
								}
							case Binop(OpAssign):
								readExpression(context, paramIdent, identChild);
							default:
						}
					}
					copyUsesToParent(identifier, paramIdent);
				default:
			}
		}
		return params;
	}

	function makePosition(fileName:String, token:TokenTree):IdentifierPos {
		return {
			fileName: fileName,
			start: token.pos.min,
			end: token.pos.max
		}
	}

	function makeIdentifier(context:UsageContext, nameToken:TokenTree, type:IdentifierType, parentIdentifier:Null<Identifier>):Null<Identifier> {
		if (nameToken == null) {
			return null;
		}
		switch (nameToken.tok) {
			case Const(CIdent("is")):
				return null;
			case Kwd(KwdNew) | Kwd(KwdThis) | Kwd(KwdNull) | Const(CIdent(_)) | Dollar(_):
			case Question:
				nameToken = nameToken.getFirstChild();
				if (nameToken == null) {
					return null;
				}
			default:
				return null;
		}
		var pos:IdentifierPos = makePosition(context.fileName, nameToken);
		var pack:Array<String> = [];

		var parent:TokenTree = nameToken.parent;

		var lastNamePart:TokenTree = nameToken;
		var parameterList:Array<Identifier> = [];

		var needsDot:Bool = true;
		function findAllNames(parentPart:TokenTree) {
			if (!parentPart.hasChildren()) {
				return;
			}
			for (child in parentPart.children) {
				switch (child.tok) {
					case Const(CIdent("is")):
						return;
					case Kwd(KwdNew) | Kwd(KwdThis) | Kwd(KwdNull) | Kwd(KwdMacro) | Const(_) | Dollar(_):
						if (needsDot) {
							return;
						}
						pack.push(child.toString());
						needsDot = true;
					case Dot:
						pack.push(".");
						needsDot = false;
					case QuestionDot:
						pack.push("?.");
						needsDot = false;
					case POpen:
						switch (parent.tok) {
							case Kwd(KwdVar) | Kwd(KwdFinal):
								type = Property;
							case Kwd(KwdFunction):
							default:
						}
						return;
					case Unop(_) | Binop(_):
						return;
					case At:
						continue;
					default:
						continue;
				}
				lastNamePart = child;
				findAllNames(child);
			}
		}
		pack.push(nameToken.toString());
		findAllNames(nameToken);
		pos.end = lastNamePart.pos.max;
		if (lastNamePart.hasChildren()) {
			for (child in lastNamePart.children) {
				switch (child.tok) {
					case Arrow:
						var scopePos = child.getPos();
						type = ScopedLocal(nameToken.pos.min, scopePos.max, Parameter(parameterList));
					case POpen:
						if (type.match(Access)) {
							if (parent.matches(Kwd(KwdNew))) {
								type = Call(true);
							} else {
								type = Call(false);
							}
						}
					default:
				}
			}
		}

		if (pack.length <= 0) {
			return null;
		}
		var name:String = pack.join("");
		var identifier:Null<Identifier> = context.nameMap.getIdentifier(name, context.file.name, pos.start);
		if (identifier == null) {
			identifier = new Identifier(type, name, pos, context.nameMap, context.file, context.type);
		}
		parameterList.push(identifier);

		if (parentIdentifier != null) {
			parentIdentifier.addUse(identifier);
		}

		return identifier;
	}

	function addTypeParameter(context:UsageContext, identifier:Identifier, token:TokenTree) {
		if (!token.hasChildren()) {
			return;
		}
		for (child in token.children) {
			switch (child.tok) {
				case Const(CIdent(_)) | Dollar(_):
					makeIdentifier(context, child, TypedParameter, identifier);
					for (identChild in findIdentifierChilds(child)) {
						switch (identChild.tok) {
							case Binop(OpLt):
								addTypeParameter(context, identifier, identChild);
							case Arrow:
								addTypeParameter(context, identifier, identChild);
							default:
						}
					}
				case POpen:
					readParameter(context, identifier, child, token.getPos().max);
				case BrOpen:
					readBlock(context, identifier, child);
				case Binop(OpGt):
					break;
				case Binop(OpAnd):
					addTypeParameter(context, identifier, child);
				case DblDot:
					readTypeHint(context, identifier, child, TypeHint);
				case Sharp(_):
					readExpression(context, identifier, child);
				case Comma:
				case Semicolon:
				case Const(_):
				default:
			}
		}
	}

	function readTypeHint(context:UsageContext, identifier:Identifier, token:TokenTree, type:IdentifierType) {
		if (token == null) {
			return;
		}
		if (!token.hasChildren()) {
			return;
		}
		for (child in token.children) {
			switch (child.tok) {
				case Const(CIdent(_)) | Dollar(_):
					makeIdentifier(context, child, type, identifier);
					if (token.matches(DblDot) && identifier != null) {
						identifier.setTypeHint(TypeHintFromTree.makeTypeHint(child));
					}
					for (identChild in findIdentifierChilds(child)) {
						switch (identChild.tok) {
							case Comment(_) | CommentLine(_):
							case Semicolon:
							case Arrow:
								readTypeHint(context, identifier, identChild, TypeHint);
							case Binop(OpLt):
								addTypeParameter(context, identifier, identChild);
							case PClose:
							case Sharp(_):
								readExpression(context, identifier, identChild);
							default:
						}
					}
				case BrOpen:
					readAnonStructure(context, identifier, child);
					if (identifier != null) {
						identifier.setTypeHint(TypeHintFromTree.makeTypeHint(child));
					}
					break;
				case POpen:
					readExpression(context, identifier, child);
					if (identifier != null) {
						identifier.setTypeHint(TypeHintFromTree.makeTypeHint(child));
					}
					break;
				case Sharp(_):
					readExpression(context, identifier, child);
				case Semicolon:
				case PClose:
				default:
			}
		}
	}

	function readAnonStructure(context:UsageContext, identifier:Identifier, token:TokenTree) {
		if (!token.hasChildren()) {
			return;
		}
		var fields:Array<TypedefFieldType> = [];
		for (child in token.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Const(CIdent(_)):
					var ident:Identifier = makeIdentifier(context, child, TypedefField(fields), identifier);
					if (child.access()
						.firstOf(At)
						.firstOf(DblDot)
						.firstOf(Const(CIdent("optional")))
						.exists()) {
						fields.push(Optional(ident));
					} else {
						fields.push(Required(ident));
					}
					final afterIdent = findIdentifierChild(child);
					readTypeHint(context, ident, afterIdent, TypeHint);
				case Kwd(KwdVar) | Kwd(KwdFinal):
					final nameToken:TokenTree = child.getFirstChild();
					if (nameToken == null) {
						continue;
					}
					var ident:Identifier = makeIdentifier(context, nameToken, TypedefField(fields), identifier);
					if (nameToken.access()
						.firstOf(At)
						.firstOf(DblDot)
						.firstOf(Const(CIdent("optional")))
						.exists()) {
						fields.push(Optional(ident));
					} else {
						fields.push(Required(ident));
					}
					final afterIdent = findIdentifierChild(nameToken);
					readTypeHint(context, ident, afterIdent, TypeHint);
				case Question:
					final question:TokenTree = child.getFirstChild();
					if (question == null) {
						continue;
					}
					var ident:Identifier = makeIdentifier(context, question, TypedefField(fields), identifier);
					fields.push(Optional(ident));
					final afterIdent = findIdentifierChild(question);
					readTypeHint(context, ident, afterIdent, TypeHint);
				case Binop(OpAnd):
					readAnonStructure(context, identifier, child);
				case BrClose:
				case Semicolon:
				default:
			}
		}
	}

	function findIdentifierChild(token:TokenTree):Null<TokenTree> {
		while (token != null) {
			token = token.getFirstChild();
			if (token == null) {
				return null;
			}
			if (token.matches(At)) {
				token = token.nextSibling;
			}
			if (token == null) {
				return null;
			}
			switch (token.tok) {
				case Const(CIdent(_)) | Dot | QuestionDot | Kwd(KwdNew) | Kwd(KwdMacro) | Dollar(_):
				default:
					return token;
			}
		}
		return null;
	}

	function findIdentifierChilds(token:TokenTree):Array<TokenTree> {
		var childs:Array<TokenTree> = [];
		while (token != null) {
			if (!token.hasChildren()) {
				return childs;
			}
			var identifierPart:Null<TokenTree> = null;
			var first:Bool = true;
			for (child in token.children) {
				switch (child.tok) {
					case Const(CIdent("is")):
						childs.push(child);
					case Const(CIdent(_)) | Dot | QuestionDot | Kwd(KwdNew) | Kwd(KwdMacro) | Dollar(_):
						if (first) {
							if (identifierPart == null) {
								identifierPart = child;
							}
							continue;
						}
						childs.push(child);
					case At | Semicolon:
					default:
						childs.push(child);
				}
				first = false;
			}
			token = identifierPart;
		}
		return childs;
	}

	function copyUsesToParent(identifier:Null<Identifier>, child:Identifier):Void {
		if (identifier == null) {
			return;
		}
		if (child == null) {
			return;
		}
		if (child.uses == null) {
			return;
		}
		if (identifier == child) {
			return;
		}
		for (use in child.uses) {
			identifier.addUse(use);
		}
	}
}
