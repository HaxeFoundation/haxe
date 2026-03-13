package refactor.rename;

import refactor.RefactorResult;
import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.discover.IdentifierPos;
import refactor.edits.Changelist;
import refactor.rename.RenameContext;

class RenameModuleLevelStatic {
	public static function refactorModuleLevelStatic(context:RenameContext, file:File, identifier:Identifier):Promise<RefactorResult> {
		var changelist:Changelist = new Changelist(context);

		var packageName:String = file.getPackage();
		var mainModulName:String = file.getMainModulName();

		var filesWithStaticImport:Array<String> = [];
		// all uses with module.identifier
		var withModul:String = '$mainModulName.${identifier.name}';
		var newWithModul:String = '$mainModulName.${context.what.toName}';
		refactorIdentifier(context, changelist, withModul, newWithModul, filesWithStaticImport);

		if (packageName.length > 0) {
			// all uses with pack.module.identifier
			var fullQualified:String = '$packageName.$mainModulName.${identifier.name}';
			var newFullQualified:String = '$packageName.$mainModulName.${context.what.toName}';
			refactorIdentifier(context, changelist, fullQualified, newFullQualified, filesWithStaticImport);
		}
		// all uses without module in own file or files with static import
		var allUses:Array<Identifier> = context.nameMap.getIdentifiers(identifier.name);
		for (use in allUses) {
			if ((use.pos.fileName != file.name) && (!filesWithStaticImport.contains(use.pos.fileName))) {
				continue;
			}
			changelist.addChange(use.pos.fileName, ReplaceText(context.what.toName, use.pos, NoFormat), use);
		}

		// all uses in files importing with package.mainmodul
		var importModul:String = '$packageName.$mainModulName';
		allUses = context.nameMap.getIdentifiers(importModul);
		for (use in allUses) {
			switch (use.type) {
				case ImportModul:
					var uses:Array<Identifier> = use.file.findAllIdentifiers((i) -> i.name == identifier.name);
					for (u in uses) {
						switch (u.type) {
							case Call(false) | Access:
								changelist.addChange(u.pos.fileName, ReplaceText(context.what.toName, u.pos, NoFormat), u);
							case ScopedLocal(_):
							default:
						}
					}
				default:
			}
		}
		return RenameHelper.replaceStaticExtension(context, changelist, identifier).then(function(_) {
			return changelist.execute();
		});
	}

	static function refactorIdentifier(context:RenameContext, changelist:Changelist, searchName:String, replaceName:String,
			filesWithStaticImport:Array<String>) {
		var allUses:Array<Identifier> = context.nameMap.getIdentifiers(searchName);
		for (use in allUses) {
			if (use.type.match(ImportModul)) {
				filesWithStaticImport.push(use.pos.fileName);
			}
			changelist.addChange(use.pos.fileName, ReplaceText(replaceName, use.pos, NoFormat), use);
		}
		var searchNameDot:String = '$searchName.';
		var replaceNameDot:String = '$replaceName.';
		allUses = context.nameMap.getStartsWith(searchNameDot);
		for (use in allUses) {
			var pos:IdentifierPos = {
				fileName: use.pos.fileName,
				start: use.pos.start,
				end: use.pos.start + searchNameDot.length
			}
			changelist.addChange(use.pos.fileName, ReplaceText(replaceNameDot, pos, NoFormat), use);
		}
	}
}
