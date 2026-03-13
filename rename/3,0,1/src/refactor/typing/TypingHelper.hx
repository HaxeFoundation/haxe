package refactor.typing;

import refactor.discover.Identifier;
import refactor.discover.Type;
import refactor.discover.TypeHintFromTree;
import refactor.discover.TypeList;

class TypingHelper {
	public static function findDescendantTypes(context:CacheAndTyperContext, packName:String, baseType:Type):Array<Type> {
		var types:Array<Type> = [];
		var fullModulName:String = '$packName.${baseType.name.name}';

		function pushType(newType:Type) {
			for (type in types) {
				if ((type.file.name == newType.file.name) && (type.name.name == newType.name.name)) {
					return;
				}
			}
			types.push(newType);
		}
		function searchImplementingTypes(types:Array<Type>, search:String) {
			for (type in types) {
				for (use in type.getIdentifiers(search)) {
					switch (use.type) {
						case Extends | Implements | AbstractOver:
							pushType(use.defineType);
						default:
					}
				}
			}
		}

		var allUses:Array<Identifier> = context.nameMap.getIdentifiers(fullModulName);
		for (use in allUses) {
			switch (use.type) {
				case ImportModul:
					var search:String = switch (use.file.importsModule(baseType.file.getPackage(), baseType.file.getMainModulName(), baseType.name.name)) {
						case None:
							continue;
						case Global | ParentPackage | SamePackage | Imported | StarImported:
							baseType.name.name;
						case ImportedWithAlias(alias):
							alias;
					}
					searchImplementingTypes(use.file.typeList, search);
				case Extends | Implements | AbstractOver:
					pushType(use.defineType);
				default:
			}
		}
		allUses = context.nameMap.getIdentifiers(baseType.name.name);
		for (use in allUses) {
			switch (use.type) {
				case Extends | Implements | AbstractOver:
					pushType(use.defineType);
				default:
			}
		}

		for (type in types) {
			for (t in findDescendantTypes(context, type.file.getPackage(), type)) {
				pushType(t);
			}
		}
		return types;
	}

	public static function matchesType(context:CacheAndTyperContext, searchTypeOf:SearchTypeOf, searchType:TypeHintType):Promise<Bool> {
		return findTypeOfIdentifier(context, searchTypeOf).then(function(identifierType:Null<TypeHintType>):Bool {
			if (identifierType == null) {
				return false;
			}
			return typeHintsEqual(identifierType, searchType);
		});
	}

	static function typeHintsEqual(typeHint1:TypeHintType, typeHint2:TypeHintType):Bool {
		switch ([typeHint1, typeHint2]) {
			case [LibType(name1, fullName1, params1), LibType(name2, fullName2, params2)]:
				if (fullName1 != fullName2) {
					return false;
				}
				if (params1.length != params2.length) {
					return false;
				}
				for (i in 0...params1.length) {
					if (!typeHintsEqual(params1[i], params2[i])) {
						return false;
					}
				}
				return true;
			case [LibType("Null", _, [paramType1]), _]:
				return typeHintsEqual(paramType1, typeHint2);
			case [_, LibType("Null", _, [paramType2])]:
				return typeHintsEqual(typeHint1, paramType2);
			case [ClasspathType(type1, params1), ClasspathType(type2, params2)]:
				if (type1.fullModuleName != type2.fullModuleName) {
					return false;
				}
				if (params1.length != params2.length) {
					return false;
				}
				for (i in 0...params1.length) {
					if (!typeHintsEqual(params1[i], params2[i])) {
						return false;
					}
				}
				return true;
			case [FunctionType(args1, retVal1), FunctionType(args2, retVal2)]:
				if (args1.length != args2.length) {
					return false;
				}
				for (i in 0...args1.length) {
					if (!typeHintsEqual(args1[i], args2[i])) {
						return false;
					}
				}
				return typeHintsEqual(retVal1, retVal2);
			case [StructType(fields1), StructType(fields2)]:
				if (fields1.length != fields2.length) {
					return false;
				}
				for (i in 0...fields1.length) {
					if (!typeHintsEqual(fields1[i], fields2[i])) {
						return false;
					}
				}
				return true;
			case [NamedType(name1, hint1), NamedType(name2, hint2)]:
				if (name1 != name2) {
					return false;
				}
				return typeHintsEqual(hint1, hint2);
			case [UnknownType(name1), UnknownType(name2)]:
				return (name1 == name2);
			default:
				return false;
		}
	}

	public static function findTypeWithTyper(context:CacheAndTyperContext, fileName:String, pos:Int):Promise<Null<TypeHintType>> {
		if (context.typer == null) {
			return Promise.reject("no typer for " + fileName + "@" + pos);
		}
		switch (context.fileReader(fileName)) {
			case Text(text):
				pos = context.converter(text, pos);
			case Token(root, text):
				pos = context.converter(text, pos);
		}
		return context.typer.resolveType(fileName, pos);
	}

	public static function findTypeOfIdentifier(context:CacheAndTyperContext, searchTypeOf:SearchTypeOf):Promise<TypeHintType> {
		var parts:Array<String> = searchTypeOf.name.split(".");
		if (parts.length > 1) {
			final type = context.typeList.getType(searchTypeOf.name);
			if (type != null) {
				return Promise.resolve(ClasspathType(type, []));
			}
		}
		var part:String = parts.shift();
		switch (part) {
			case "super":
				final containerType = searchTypeOf.defineType;
				if (containerType == null) {
					return Promise.reject("cannot resolve super");
				}
				final baseClasses = containerType.findAllIdentifiers(i -> i.type == Extends);
				if (baseClasses.length != 1) {
					return Promise.reject("cannot resolve super");
				}
				final type = TypeHintFromTree.findTypeFromImports(baseClasses[0].name, context.typeList, containerType.file);
				if (type == null) {
					return Promise.reject("cannot resolve super");
				}
				return Promise.resolve(ClasspathType(type, []));
			case "this":
				if (searchTypeOf.defineType == null) {
					return Promise.reject("cannot resolve this");
				}
				if (parts.length > 0) {
					return findTypeOfIdentifier(context, {
						name: parts.join("."),
						pos: searchTypeOf.pos,
						defineType: searchTypeOf.defineType
					});
				}
				return Promise.resolve(ClasspathType(searchTypeOf.defineType, []));
			default:
		}

		return findFieldOrScopedLocal(context, searchTypeOf.defineType, part, searchTypeOf.pos).then(function(type:Null<TypeHintType>):Promise<TypeHintType> {
			var index:Int = 0;
			function findFieldForPart(partType:TypeHintType):Promise<TypeHintType> {
				if (index >= parts.length) {
					return Promise.resolve(partType);
				}

				var part:String = parts[index++];
				switch (partType) {
					case null:
						context.verboseLog('unable to determine type of "$part" in ${searchTypeOf.defineType?.file.name}@${searchTypeOf.pos}');
						return Promise.reject('unable to determine type of "$part" in ${searchTypeOf.defineType?.file.name}@${searchTypeOf.pos}');
					case ClasspathType(t, params):
						return findField(context, t, part).then(findFieldForPart);
					case LibType("Null", _, [nullType]):
						return findFieldForPart(nullType);
					case LibType(t, _, params):
						return Promise.reject('unable to determine type of "$part" in ${searchTypeOf.defineType.name.name}@${searchTypeOf.pos}');
					case StructType(fields):
						for (field in fields) {
							switch (field) {
								case NamedType(name, typeHint):
									if (name == part) {
										return findFieldForPart(typeHint);
									}
								default:
							}
						}
						return Promise.reject('unable to determine type of "$part" in ${searchTypeOf.defineType.name.name}@${searchTypeOf.pos}');
					case FunctionType(_, retVal):
						return findFieldForPart(retVal);
					case UnknownType(name):
						return Promise.reject('unable to determine type of "$part" in ${searchTypeOf.defineType.name.name}@${searchTypeOf.pos}');
					case NamedType(_):
						return Promise.reject('unable to determine type of "$part" in ${searchTypeOf.defineType.name.name}@${searchTypeOf.pos}');
				}
			}

			return findFieldForPart(type).then(function(typeHint) {
				return Promise.resolve(typeHint);
			});
		});
	}

	public static function findFieldOrScopedLocal(context:CacheAndTyperContext, containerType:Type, name:String, pos:Int):Promise<TypeHintType> {
		if (containerType == null) {
			return Promise.resolve(null);
		}

		return findTypeWithBuiltIn(containerType, name, pos, context).then(function(hint) {
			return Promise.resolve(hint);
		}).catchError(function(msg):Promise<TypeHintType> {
			// built-in failed, let's try external typer
			return findTypeWithTyper(context, containerType.file.name, pos).catchError(function(msg):Promise<TypeHintType> {
				#if debug
				trace("built-in and external typers failed for " + '$name in ${containerType.file.name} @$pos');
				#end
				return Promise.resolve(null);
			});
		});
	}

	static function findTypeWithBuiltIn(containerType:Type, name:String, pos:Int, context:CacheAndTyperContext):Promise<TypeHintType> {
		if (containerType == null) {
			return Promise.reject("missing containing type");
		}
		var allUses:Array<Identifier> = containerType.getIdentifiers(name);
		var candidate:Null<Identifier> = null;
		var fieldCandidate:Null<Identifier> = null;
		for (use in allUses) {
			switch (use.type) {
				case Property | FieldVar(_) | Method(_):
					fieldCandidate = use;
				case TypedefField(_):
					fieldCandidate = use;
				case EnumField(_):
					return Promise.resolve(ClasspathType(use.defineType, []));
				case ScopedLocal(scopeStart, scopeEnd, _):
					if ((pos >= scopeStart) && (pos <= scopeEnd)) {
						candidate = use;
					}
					if (pos == use.pos.start) {
						candidate = use;
					}
				case CaseLabel(switchIdentifier):
					if (use.pos.start <= pos && use.pos.end > pos) {
						return findTypeOfIdentifier(context, {
							name: switchIdentifier.name,
							pos: switchIdentifier.pos.start,
							defineType: containerType
						});
					}
				case Call(true):
					if (use.pos.start <= pos && use.pos.end > pos) {
						var typeCandidates = context.typeList.findTypeName(name);
						for (candidateType in typeCandidates) {
							switch (containerType.file.importsModule(candidateType.file.getPackage(), candidateType.file.getMainModulName(),
								candidateType.name.name)) {
								case None:
								case ImportedWithAlias(_):
								case Global | ParentPackage | SamePackage | Imported | StarImported:
									return Promise.resolve(ClasspathType(candidateType, []));
							}
						}
					}
				default:
			}
		}
		if (candidate == null) {
			candidate = fieldCandidate;
		}
		if (candidate == null) {
			return findGlobalIdentifiers(context, name, containerType);
		}

		final candidateTypeHint = candidate.getTypeHintNew(context.typeList);

		if (candidateTypeHint != null) {
			return Promise.resolve(candidateTypeHint);
		}
		// return Promise.reject("cannot find type hint");

		var typeHint:Null<Identifier> = candidate.getTypeHint();
		switch (candidate.type) {
			case ScopedLocal(_, _, ForLoop(loopIdent)):
				var index:Int = loopIdent.indexOf(candidate);
				var changes:Array<Promise<TypeHintType>> = [];
				for (child in loopIdent) {
					switch (child.type) {
						case ScopedLocal(_, _, ForLoop(_)):
							continue;
						default:
							changes.push(findTypeOfIdentifier(context, {
								name: child.name,
								pos: child.pos.start,
								defineType: containerType
							}).then(function(data:TypeHintType):Promise<TypeHintType> {
								switch (data) {
									case null:
									case ClasspathType(_, typeParams) | LibType(_, _, typeParams):
										if (typeParams.length <= index) {
											return Promise.reject("not enough type parameters");
										}
										return Promise.resolve(typeParams[index]);
									case UnknownType(_) | StructType(_) | FunctionType(_) | NamedType(_, _):
								}
								return Promise.reject("not found");
							}));
					}
				}

				var winner:Promise<TypeHintType> = cast Promise.race(changes);
				return winner.catchError(function(data:TypeHintType):Promise<TypeHintType> {
					if (typeHint != null) {
						return typeFromTypeHint(context, typeHint);
					}
					return Promise.reject("type not found");
				});
			case ScopedLocal(_, _, Parameter(params)):
				if (typeHint != null) {
					return typeFromTypeHint(context, typeHint).then(function(hint) {
						return Promise.resolve(hint);
					});
				}
				var index:Int = params.indexOf(candidate);
				switch (candidate.parent.type) {
					case CaseLabel(switchIdentifier):
						return findFieldOrScopedLocal(context, containerType, switchIdentifier.name,
							switchIdentifier.pos.start).then(function(enumType:TypeHintType) {
							switch (enumType) {
								case null:
									return Promise.resolve(null);
								case ClasspathType(type, typeParams):
									switch (type.name.type) {
										case Enum:
											var enumFields:Array<Identifier> = type.findAllIdentifiers((i) -> i.name == candidate.parent.name);
											for (field in enumFields) {
												switch (field.type) {
													case EnumField(params):
														if (params.length <= index) {
															return Promise.resolve(null);
														}
														return Promise.resolve(params[index].getTypeHintNew(context.typeList));
													default:
														return Promise.reject("not an enum field");
												}
											}
										default:
									}
								case LibType(_, _, _):
									return Promise.resolve(enumType);
								case FunctionType(_, _):
									return Promise.reject("");
								case StructType(_):
									return Promise.reject("");
								case UnknownType(_):
									return Promise.reject("");
								case NamedType(_):
									return Promise.reject("");
							}
							return Promise.resolve(enumType);
						});
					default:
				}
			case ScopedLocal(_, _, CaseCapture(origin, index)):
				return findFieldOrScopedLocal(context, containerType, origin.name, origin.pos.start).then(function(enumType:TypeHintType) {
					switch (enumType) {
						case ClasspathType(type, typeParams):
							var fieldName = origin.name;
							if (fieldName.startsWith(type.name.name + ".")) {
								fieldName = fieldName.substr(type.name.name.length + 1);
							}
							if (fieldName.startsWith(type.fullModuleName + ".")) {
								fieldName = fieldName.substr(type.fullModuleName.length + 1);
							}
							var uses = type.getIdentifiers(fieldName);
							for (use in uses) {
								switch (use.type) {
									case EnumField(params):
										if (params.length < index) {
											return Promise.reject("not found");
										}
										return Promise.resolve(params[index].getTypeHintNew(context.typeList));
									default:
								}
							}
						default:
					}
					return Promise.reject("cannot resolve type of " + origin.name);
				});

			default:
		}
		if (typeHint != null) {
			return typeFromTypeHint(context, typeHint);
		}
		return Promise.reject("cannot resolve type of " + name);
	}

	static function findGlobalIdentifiers(context:CacheAndTyperContext, name:String, containerType:Type):Promise<TypeHintType> {
		final matches:Array<TypeHintType> = [];
		var typeCandidates = context.typeList.findTypeName(name);
		for (candidateType in typeCandidates) {
			switch (containerType.file.importsModule(candidateType.file.getPackage(), candidateType.file.getMainModulName(), candidateType.name.name)) {
				case None:
				case ImportedWithAlias(_):
				case Global | ParentPackage | SamePackage | Imported | StarImported:
					matches.push(ClasspathType(candidateType, []));
					break;
			}
		}
		final allUses = context.nameMap.getIdentifiers(name);
		for (use in allUses) {
			switch (use.type) {
				case ModuleLevelStaticVar:
				case ModuleLevelStaticMethod:
				case FieldVar(false) | Method(false):
					// TODO: is it a base class of using type?
				case EnumField(_):
					switch (containerType.file.importsModule(use.file.getPackage(), use.file.getMainModulName(), use.defineType.name.name)) {
						case None:
						case ImportedWithAlias(_):
						case Global | ParentPackage | SamePackage | Imported | StarImported:
							matches.push(ClasspathType(use.defineType, []));
							break;
					}
				default:
			}
		}
		return switch (matches.length) {
			case 0:
				Promise.reject("no candidate found");
			case 1:
				Promise.resolve(matches[0]);
			default:
				Promise.reject("too many candidates found " + matches.length);
		}
	}

	public static function findField(context:CacheAndTyperContext, containerType:Type, name:String):Promise<TypeHintType> {
		var allUses:Array<Identifier> = containerType.getIdentifiers(name);
		var candidate:Null<Identifier> = null;
		for (use in allUses) {
			switch (use.type) {
				case Property | FieldVar(_) | Method(_) | TypedefField(_) | EnumField(_):
					candidate = use;
					break;
				default:
			}
		}
		if ((candidate == null) || (candidate.uses == null)) {
			switch (containerType.name.type) {
				case Class:
					var baseType:Null<Type> = findBaseClass(context.typeList, containerType);
					if (baseType == null) {
						return Promise.resolve(null);
					}

					return findField(context, baseType, name);
				case Typedef:
				default:
			}
			return Promise.resolve(null);
		}
		final typeHint = candidate.getTypeHintNew(context.typeList);
		if (typeHint != null) {
			return Promise.resolve(typeHint);
		}
		for (use in candidate.uses) {
			switch (use.type) {
				case TypeHint:
					return typeFromTypeHint(context, use);
				default:
			}
		}
		return Promise.resolve(null);
	}

	public static function findBaseClass(typeList:TypeList, type:Type):Null<Type> {
		var baseClasses:Array<Identifier> = type.findAllIdentifiers((i) -> i.type.match(Extends));
		for (base in baseClasses) {
			var candidateTypes:Array<Type> = typeList.findTypeName(base.name);
			for (candidate in candidateTypes) {
				switch (type.file.importsModule(candidate.file.getPackage(), candidate.file.getMainModulName(), candidate.name.name)) {
					case None:
					case ImportedWithAlias(_):
					case Global | ParentPackage | SamePackage | Imported | StarImported:
						return candidate;
				}
			}
		}
		return null;
	}

	public static function typeFromTypeHint(context:CacheAndTyperContext, hint:Identifier):Promise<TypeHintType> {
		if (hint.name == "Null") {
			if ((hint.uses == null) || (hint.uses.length <= 0)) {
				return Promise.reject();
			}
			return typeFromTypeHint(context, hint.uses[0]).then(function(typeHint) {
				return Promise.resolve(LibType("Null", "Null", [typeHint]));
			});
		}

		var parts:Array<String> = hint.name.split(".");
		var typeName:String = parts.pop();

		var typeParams:Array<TypeHintType> = [];
		if (hint.uses != null) {
			for (use in hint.uses) {
				switch (use.type) {
					case TypedParameter:
						var allTypes:Array<Type> = context.typeList.findTypeName(use.name);
						if (allTypes.length <= 0) {
							typeParams.push(LibType(use.name, use.name, []));
							continue;
						}
						for (type in allTypes) {
							switch (hint.file.importsModule(type.file.getPackage(), type.file.getMainModulName(), type.name.name)) {
								case None:
								case ImportedWithAlias(_):
								case Global | ParentPackage | SamePackage | Imported | StarImported:
									// TODO recursive type params!!!
									typeParams.push(ClasspathType(type, []));
							}
						}
					default:
				}
			}
		}

		var allTypes:Array<Type> = context.typeList.findTypeName(typeName);

		if (parts.length > 0) {
			for (type in allTypes) {
				if (type.fullModuleName == hint.name) {
					return Promise.resolve(ClasspathType(type, typeParams));
				}
			}

			return Promise.resolve(LibType(hint.name, hint.name, typeParams));
		}
		for (type in allTypes) {
			switch (hint.file.importsModule(type.file.getPackage(), type.file.getMainModulName(), type.name.name)) {
				case None:
				case ImportedWithAlias(_):
				case Global | ParentPackage | SamePackage | Imported | StarImported:
					return Promise.resolve(ClasspathType(type, typeParams));
			}
		}
		// TODO if there's no type found maybe it's because of an import alias
		return Promise.resolve(LibType(hint.name, hint.name, typeParams));
	}
}

typedef SearchTypeOf = {
	var name:String;
	var pos:Int;
	var defineType:Type;
}

typedef TypeParameterList = Array<Identifier>;
