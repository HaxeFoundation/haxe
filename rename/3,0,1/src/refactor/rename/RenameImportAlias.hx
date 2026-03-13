package refactor.rename;

import refactor.RefactorResult;
import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.edits.Changelist;
import refactor.rename.RenameContext;

class RenameImportAlias {
	public static function refactorImportAlias(context:RenameContext, file:File, identifier:Identifier):Promise<RefactorResult> {
		var allUses:Array<Identifier> = context.nameMap.getIdentifiers(identifier.name);
		var isImportHx:Bool = (file.getMainModulName() == "import");

		var changelist:Changelist = new Changelist(context);
		for (use in allUses) {
			if (use.file.name == file.name) {
				changelist.addChange(use.pos.fileName, ReplaceText(context.what.toName, use.pos, NoFormat), use);
				continue;
			}
			if (isImportHx) {
				if (use.file.importHxFile.name == file.name) {
					changelist.addChange(use.pos.fileName, ReplaceText(context.what.toName, use.pos, NoFormat), use);
				}
			}
		}
		return Promise.resolve(changelist.execute());
	}
}
