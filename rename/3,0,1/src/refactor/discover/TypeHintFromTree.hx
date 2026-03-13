package refactor.discover;

class TypeHintFromTree {
	public static function makeTypeHint(token:Null<TokenTree>):TypeHintType {
		if (token == null) {
			return null;
		}
		switch (token.tok) {
			case Kwd(KwdNull) | Const(CIdent(_)) | Dollar(_):
				final arrowChild = findArrow(token);
				if (arrowChild != null) {
					final arrowType = TokenTreeCheckUtils.getArrowType(arrowChild);
					switch (arrowType) {
						case ArrowFunction:
							return null;
						case OldFunctionType:
							return makeOldFunctionTypeHint(token);
						case NewFunctionType:
							return makeFunctionTypeHint(token);
					}
				}
				return makeLibTypeHint(token);
			case BrOpen:
				return makeStructTypeHint(token);
			case POpen:
				return makeFunctionTypeHint(token);
			default:
				return null;
		}
	}

	static function findArrow(token:TokenTree):Null<TokenTree> {
		while (token != null) {
			switch (token.tok) {
				case Const(CIdent(_)) | Kwd(KwdNull) | Kwd(KwdMacro) | Dollar(_):
					token = token.getFirstChild();
				case Dot:
					token = token.getFirstChild();
				case Binop(OpLt):
					token = token.nextSibling;
				case DblDot:
					token = token.getFirstChild();
				case Arrow:
					return token;
				case Comment(_) | CommentLine(_):
					return null;
				case Comma, Semicolon:
					return null;
				case Binop(OpAssign) | Binop(OpAssignOp(_)):
					return null;
				default:
					return null;
			}
		}

		return null;
	}

	static function makeLibTypeHint(token:Null<TokenTree>):TypeHintType {
		if (token == null) {
			return null;
		}

		var parts:Array<String> = [];
		while (token != null) {
			switch (token.tok) {
				case Const(CIdent(name)):
					parts.push(name);
				case Dollar(name):
					parts.push(name);
				case Kwd(KwdNull):
					parts.push("Null");
				case Kwd(KwdMacro):
					parts.push("macro");
				case Comma:
					break;
				case Semicolon:
					break;
				default:
			}
			token = token.getFirstChild();
			if (token == null) {
				break;
			}
			switch (token.tok) {
				case Dot:
					token = token.getFirstChild();
					continue;
				case Binop(OpLt):
					break;
				case Comma | Semicolon:
					break;
				case Arrow:
					break;
				case DblDot:
					break;
				case Binop(OpAssign) | Binop(OpAssignOp(_)):
					break;
				default:
			}
		}
		if (parts.length <= 0) {
			return null;
		}
		final fullName = parts.join(".");
		final name = fullName; // parts.pop();

		if (token == null) {
			return LibType(name, fullName, []);
		}
		return switch (token.tok) {
			case Binop(OpLt):
				LibType(name, fullName, makeTypeHintParams(token));
			case DblDot:
				NamedType(name, makeTypeHint(token.getFirstChild()));
			default:
				LibType(name, fullName, []);
		}
	}

	static function makeTypeHintParams(token:Null<TokenTree>):Array<TypeHintType> {
		var params:Array<TypeHintType> = [];
		for (child in token.children) {
			switch (child.tok) {
				case Const(CIdent(paramName)) | Dollar(paramName):
					final firstChild = child.getFirstChild();
					if (firstChild == null) {
						params.push(LibType(paramName, paramName, []));
						continue;
					}
					switch (firstChild.tok) {
						case Dot:
							final accessTypeHint = makeTypeHint(child);
							params.push(accessTypeHint);
							continue;
						case Binop(OpLt):
							params.push(LibType(paramName, paramName, makeTypeHintParams(firstChild)));
						default:
					}
				case Comma:
				case Binop(OpGt):
				default:
			}
		}
		return params;
	}

	static function makeStructTypeHint(token:Null<TokenTree>):TypeHintType {
		if (token == null) {
			return null;
		}
		if (!token.hasChildren()) {
			return null;
		}
		var fields:Array<TypeHintType> = [];
		for (child in token.children) {
			switch (child.tok) {
				case Const(CIdent(name)) | Dollar(name):
					final fieldType = makeTypeHint(child.access().firstOf(DblDot).firstChild().token);
					fields.push(NamedType(name, fieldType));
				case Kwd(KwdVar):
					var nameToken = child.getFirstChild();
					if (nameToken == null) {
						continue;
					}
					final name = switch (nameToken.tok) {
						case Kwd(KwdNull):
							"NUll";
						case Const(CIdent(name)):
							name;
						default:
							'$nameToken';
					}
					final fieldType = makeTypeHint(nameToken.access().firstOf(DblDot).firstChild().token);
					fields.push(NamedType(name, fieldType));
				case BrClose:
					break;
				default:
			}
		}
		return StructType(fields);
	}

	static function makeOldFunctionTypeHint(token:Null<TokenTree>):TypeHintType {
		if (token == null) {
			return null;
		}
		if (!token.hasChildren()) {
			return null;
		}
		final args:Array<TypeHintType> = [];
		var pack:Array<String> = [];
		while (token != null) {
			if (token.matches(Arrow)) {
				token = token.getFirstChild();
				continue;
			}
			final typeHint = makeLibTypeHint(token);
			if (typeHint != null) {
				args.push(typeHint);
			}
			while (token != null) {
				if (!token.hasChildren()) {
					token = null;
					break;
				}
				var newToken:TokenTree = null;
				for (child in token.children) {
					switch (child.tok) {
						case Const(CIdent(_)) | Dot:
							newToken = child;
							break;
						case Binop(OpLt):
						case Arrow:
							newToken = child;
							break;
						case Semicolon:
							break;
						default:
					}
				}
				token = newToken;

				if (token == null) {
					break;
				}
				if (token.matches(Arrow)) {
					break;
				}
			}
		}
		if (args.length < 2) {
			return null;
		}
		final retVal = args.pop();
		return FunctionType(args, retVal);
	}

	static function makeFunctionTypeHint(token:Null<TokenTree>):TypeHintType {
		if (token == null) {
			return null;
		}
		if (!token.hasChildren()) {
			return null;
		}
		final args:Array<TypeHintType> = [];
		for (child in token.children) {
			switch (child.tok) {
				case Const(CIdent(_)):
					args.push(makeTypeHint(child));
				case PClose:
					break;
				default:
			}
		}
		var retToken = token.access().firstOf(Arrow).firstChild().token;
		return FunctionType(args, makeTypeHint(retToken));
	}

	public static function resolveTypeHint(unresolvedTypeHint:TypeHintType, types:TypeList, file:File):TypeHintType {
		return switch (unresolvedTypeHint) {
			case ClasspathType(type, typeParams):
				final newParams:Array<TypeHintType> = [];
				for (param in typeParams) {
					newParams.push(resolveTypeHint(param, types, file));
				}
				ClasspathType(type, newParams);
			case LibType(name, fullName, typeParams):
				final newParams:Array<TypeHintType> = [];
				for (param in typeParams) {
					newParams.push(resolveTypeHint(param, types, file));
				}
				final type = findTypeFromImports(fullName, types, file);
				return if (type == null) {
					LibType(name, fullName, newParams);
				} else {
					ClasspathType(type, newParams);
				}
			case FunctionType(args, retVal):
				final newArgs:Array<TypeHintType> = [];
				for (arg in args) {
					newArgs.push(resolveTypeHint(arg, types, file));
				}
				FunctionType(newArgs, resolveTypeHint(retVal, types, file));
			case StructType(fields):
				final newFields:Array<TypeHintType> = [];
				for (field in fields) {
					newFields.push(resolveTypeHint(field, types, file));
				}
				StructType(newFields);
			case NamedType(name, typeHint):
				NamedType(name, resolveTypeHint(typeHint, types, file));
			case UnknownType(name):
				unresolvedTypeHint;
		}
	}

	public static function findTypeFromImports(fullName:String, types:TypeList, file:File):Null<Type> {
		var type = types.getType(fullName);
		if (type != null) {
			return type;
		}

		var typeCandidates = types.findTypeName(fullName);
		for (importItem in file.importList) {
			if (importItem.alias == null) {
				continue;
			}
			if (importItem.alias.name == fullName) {
				final type = types.getType(importItem.moduleName.name);
				if (type != null) {
					typeCandidates.push(type);
				}
			}
		}
		for (candidateType in typeCandidates) {
			switch (file.importsModule(candidateType.file.getPackage(), candidateType.file.getMainModulName(), candidateType.name.name)) {
				case None:
				case Global | ParentPackage | SamePackage | Imported | StarImported | ImportedWithAlias(_):
					return candidateType;
			}
		}
		return null;
	}
}
