package refactor.rename;

import haxe.io.Path;
import refactor.RefactorResult;
import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.discover.IdentifierPos;
import refactor.edits.Changelist;
import refactor.rename.RenameContext;

class RenamePackage {
	public static function refactorPackageName(context:RenameContext, file:File, identifier:Identifier):Promise<RefactorResult> {
		var changelist:Changelist = new Changelist(context);
		var mainTypeName:String = file.getMainModulName();

		var packageNamePrefix:String = "";
		var packageName:String = file.getPackage();
		if (packageName.length > 0) {
			packageNamePrefix = file.packageIdentifier.name + ".";
			changelist.addChange(file.name, ReplaceText(context.what.toName, file.packageIdentifier.pos, NoFormat), identifier);
		} else {
			changelist.addChange(file.name, InsertText('package ${context.what.toName};\n', {fileName: file.name, start: 0, end: 0}, NoFormat), identifier);
		}

		var newMainModulName:String = context.what.toName + "." + mainTypeName;
		var mainModule:String = packageNamePrefix + mainTypeName;
		var allUses:Array<Identifier> = context.nameMap.getIdentifiers(mainModule);
		for (use in allUses) {
			changelist.addChange(use.pos.fileName, ReplaceText(newMainModulName, use.pos, NoFormat), use);
		}
		for (type in file.typeList) {
			if (mainTypeName == type.name.name) {
				continue;
			}
			var typeName:String = type.name.name;

			var fullModulName:String = packageNamePrefix + typeName;
			var newFullModulName:String = context.what.toName + "." + typeName;
			allUses = context.nameMap.getIdentifiers(fullModulName);
			for (use in allUses) {
				changelist.addChange(use.pos.fileName, ReplaceText(newFullModulName, use.pos, NoFormat), use);
			}

			fullModulName = packageNamePrefix + mainTypeName + "." + typeName;
			newFullModulName = context.what.toName + "." + mainTypeName + "." + typeName;
			allUses = context.nameMap.getIdentifiers(fullModulName);
			for (use in allUses) {
				changelist.addChange(use.pos.fileName, ReplaceText(newFullModulName, use.pos, NoFormat), use);
			}
		}
		var uniqueFiles:Array<String> = [];

		allUses = context.nameMap.getIdentifiers(mainTypeName);
		for (use in allUses) {
			if (use.file.name == file.name) {
				continue;
			}
			if (use.file.getPackage() != packageName) {
				continue;
			}
			if (uniqueFiles.contains(use.pos.fileName)) {
				// only add once per file
				continue;
			}
			switch (use.file.importsModule(packageName, mainTypeName, mainTypeName)) {
				case None:
				case Global | ParentPackage | SamePackage | StarImported:
					var importPos:IdentifierPos = {fileName: use.pos.fileName, start: use.file.importInsertPos, end: use.file.importInsertPos}
					changelist.addChange(use.pos.fileName, InsertText('import $newMainModulName;\n', importPos, NoFormat), use);
					uniqueFiles.push(use.pos.fileName);
				case Imported:
				case ImportedWithAlias(_):
			}
		}

		// TODO remove redundant imports
		moveFileToPackage(context, file, changelist, packageName);
		return Promise.resolve(changelist.execute());
	}

	static function moveFileToPackage(context:RenameContext, file:File, changelist:Changelist, packageName:String) {
		var path:Path = new Path(file.name);
		var mainTypeName:String = file.getMainModulName();

		var dotPath:String = path.dir.replace("/", ".").replace("\\", ".");

		var index:Int = dotPath.lastIndexOf("." + packageName) + 1;
		if (index < 0) {
			index = dotPath.indexOf(packageName);
		}

		var pathParts:Array<String> = context.what.toName.split(".");

		var rootPath:String = Path.join(dotPath.substr(0, index).split("."));
		pathParts.unshift(Path.removeTrailingSlashes(rootPath));
		pathParts.push('${mainTypeName}.hx');
		changelist.addChange(file.name, Move(Path.join(pathParts)), null);
		context.fileList.addRecentlyRenamed(file);
	}
}
