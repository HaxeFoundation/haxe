package refactor.rename;

import refactor.RefactorResult;
import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.edits.Changelist;
import refactor.rename.RenameContext;

class RenameEnumField {
	public static function refactorEnumField(context:RenameContext, file:File, identifier:Identifier):Promise<RefactorResult> {
		var changelist:Changelist = new Changelist(context);
		changelist.addChange(identifier.pos.fileName, ReplaceText(context.what.toName, identifier.pos, NoFormat), identifier);

		var packName:String = file.getPackage();
		var mainModuleName:String = file.getMainModulName();
		var typeName:String = identifier.defineType.name.name;
		var fullModuleTypeName:String = identifier.defineType.fullModuleName;
		var allUses:Array<Identifier> = context.nameMap.getIdentifiers('$typeName.${identifier.name}');
		for (use in allUses) {
			switch (use.file.importsModule(packName, mainModuleName, typeName)) {
				case None:
					continue;
				case ImportedWithAlias(alias):
					if (alias != typeName) {
						continue;
					}
				case Global | ParentPackage | SamePackage | Imported | StarImported:
			}
			RenameHelper.replaceTextWithPrefix(use, typeName + ".", context.what.toName, changelist);
		}

		allUses = context.nameMap.getIdentifiers('$fullModuleTypeName.${identifier.name}');
		for (use in allUses) {
			RenameHelper.replaceTextWithPrefix(use, fullModuleTypeName + ".", context.what.toName, changelist);
		}

		allUses = context.nameMap.matchIdentifierPart(identifier.name, true);

		var changes:Array<Promise<Void>> = [];
		for (use in allUses) {
			switch (use.type) {
				case CaseLabel(switchIdentifier):
					changes.push(TypingHelper.matchesType(context, {
						name: switchIdentifier.name,
						pos: switchIdentifier.pos.start,
						defineType: switchIdentifier.defineType
					}, ClasspathType(identifier.defineType, [])).then(function(matched:Bool) {
						if (matched) {
							RenameHelper.replaceTextWithPrefix(use, "", context.what.toName, changelist);
						}
					}));
					continue;
				case Access | Call(false):
					switch (use.parent.type) {
						case Call(_):
						default:
							continue;
					}
					switch (use.file.importsModule(packName, mainModuleName, typeName)) {
						case None:
							continue;
						case ImportedWithAlias(alias):
							if (alias != typeName) {
								continue;
							}
						case Global | ParentPackage | SamePackage | Imported | StarImported:
					}
				default:
					continue;
			}
			RenameHelper.replaceTextWithPrefix(use, "", context.what.toName, changelist);
		}
		return Promise.all(changes).then(function(_):RefactorResult {
			return changelist.execute();
		});
	}
}
