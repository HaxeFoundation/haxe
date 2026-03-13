package refactor.rename;

import refactor.RefactorResult;
import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.discover.IdentifierPos;
import refactor.discover.Type;
import refactor.edits.Changelist;
import refactor.rename.RenameContext;

class RenameField {
	public static function refactorField(context:RenameContext, file:File, identifier:Identifier, isStatic:Bool):Promise<RefactorResult> {
		var changelist:Changelist = new Changelist(context);

		var packName:String = file.getPackage();
		var types:Array<Type> = TypingHelper.findDescendantTypes(context, packName, identifier.defineType);

		types.push(identifier.defineType);
		var changes:Array<Promise<Void>> = [];
		for (type in types) {
			// use of field inside interfaces / classes (self + extending / implementing)
			replaceInType(changelist, type, "", identifier.name, context.what.toName);
			replaceInTypeWithFieldAccess(changelist, type, "", identifier.name, context.what.toName);

			// super calls inside types
			replaceInType(changelist, type, "super.", identifier.name, context.what.toName);

			// this calls inside types
			replaceInType(changelist, type, "this.", identifier.name, context.what.toName);
			replaceInTypeWithFieldAccess(changelist, type, "this.", identifier.name, context.what.toName);

			// property setters / getters
			switch (identifier.type) {
				case InterfaceProperty:
					replaceInType(changelist, type, "set_", identifier.name, context.what.toName);
					replaceInType(changelist, type, "get_", identifier.name, context.what.toName);
				default:
			}

			if (isStatic) {
				changes.push(replaceStaticUse(context, changelist, type, identifier.name));
				switch (identifier.type) {
					case Method(true):
						changes.push(RenameHelper.replaceStaticExtension(context, changelist, identifier));
					default:
				}
			}

			// TODO imports with alias
		}
		changes.push(replaceAccessOrCalls(context, changelist, identifier, types));
		return Promise.all(changes).then(function(_):RefactorResult {
			return changelist.execute();
		});
	}

	public static function replaceAccessOrCalls(context:RenameContext, changelist:Changelist, identifier:Identifier, types:Array<Type>):Promise<Void> {
		var allUses:Array<Identifier> = context.nameMap.matchIdentifierPart(identifier.name, true);
		var changes:Array<Promise<Void>> = [];
		for (use in allUses) {
			changes.push(RenameHelper.replaceSingleAccessOrCall(context, changelist, use, identifier.name, types));
		}
		return Promise.all(changes).then(null);
	}

	static function replaceInType(changelist:Changelist, type:Type, prefix:String, from:String, to:String) {
		var allUses:Array<Identifier> = type.getIdentifiers(prefix + from);
		var innerScopeStart:Int = 0;
		var innerScopeEnd:Int = -1;
		for (use in allUses) {
			if ((innerScopeStart < use.pos.start) && (use.pos.start < innerScopeEnd)) {
				continue;
			}

			switch (use.type) {
				case ScopedLocal(start, end, _):
					innerScopeStart = start;
					innerScopeEnd = end;
					continue;
				case StructureField(_):
					continue;
				default:
			}
			RenameHelper.replaceTextWithPrefix(use, prefix, to, changelist);
		}
	}

	static function replaceInTypeWithFieldAccess(changelist:Changelist, type:Type, prefix:String, from:String, to:String) {
		var allUses:Array<Identifier> = type.getIdentifiers(prefix + from);
		var allAccess:Array<Identifier> = type.getStartsWith('$prefix$from.');
		allAccess = allAccess.concat(type.getStartsWith('$prefix$from?.'));
		var shadowed:Bool = false;
		for (access in allAccess) {
			for (use in allUses) {
				if (use.pos.start > access.pos.start) {
					break;
				}
				switch (use.type) {
					case ScopedLocal(_, end, _):
						if (end > access.pos.start) {
							shadowed = true;
							break;
						}
					case StructureField(_):
					default:
				}
			}
			if (shadowed) {
				continue;
			}
			var pos:IdentifierPos = {
				fileName: access.pos.fileName,
				start: access.pos.start + prefix.length,
				end: access.pos.start + prefix.length + from.length
			};
			changelist.addChange(access.pos.fileName, ReplaceText(to, pos, NoFormat), access);
		}
	}

	static function replaceStaticUse(context:RenameContext, changelist:Changelist, type:Type, fromName:String):Promise<Void> {
		var packName:String = type.file.getPackage();
		var allUses:Array<Identifier> = context.nameMap.getIdentifiers('${type.name.name}.$fromName');
		for (use in allUses) {
			switch (use.file.importsModule(packName, type.file.getMainModulName(), type.name.name)) {
				case None:
					continue;
				case ImportedWithAlias(_):
					continue;
				case Global | ParentPackage | SamePackage | Imported | StarImported:
			}
			RenameHelper.replaceTextWithPrefix(use, '${type.name.name}.', context.what.toName, changelist);
		}

		var fullModuleName:String = type.fullModuleName;
		allUses = context.nameMap.getIdentifiers('$fullModuleName.$fromName');
		for (use in allUses) {
			RenameHelper.replaceTextWithPrefix(use, '$fullModuleName.', context.what.toName, changelist);
		}
		var changes:Array<Promise<Void>> = [];
		switch (type.name.type) {
			case Abstract:
				allUses = context.nameMap.matchIdentifierPart(fromName, true);
				for (use in allUses) {
					switch (use.type) {
						case CaseLabel(switchIdentifier):
							changes.push(TypingHelper.matchesType(context, {
								name: switchIdentifier.name,
								pos: switchIdentifier.pos.start,
								defineType: switchIdentifier.defineType
							}, ClasspathType(type, [])).then(function(matched:Bool) {
								if (matched) {
									RenameHelper.replaceTextWithPrefix(use, "", context.what.toName, changelist);
								}
							}));
						default:
					}
				}
			default:
		}
		return Promise.all(changes).then(null);
	}
}
