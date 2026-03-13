package refactor.rename;

import haxe.io.Path;
import refactor.RefactorResult;
import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.discover.IdentifierPos;
import refactor.edits.Changelist;
import refactor.rename.RenameContext;
import refactor.typing.TypeHintType;
import refactor.typing.TypingHelper;

class RenameTypeName {
	public static function refactorTypeName(context:RenameContext, file:File, identifier:Identifier):Promise<RefactorResult> {
		var changelist:Changelist = new Changelist(context);
		var packName:String = file.getPackage();
		var mainModuleName:String = file.getMainModulName();
		var path:Path = new Path(file.name);

		if (mainModuleName == identifier.name) {
			// type and filename are identical -> move file
			var newFileName:String = Path.join([path.dir, context.what.toName]) + "." + path.ext;
			changelist.addChange(file.name, Move(newFileName), null);
		}

		// replace self
		changelist.addChange(identifier.pos.fileName, ReplaceText(context.what.toName, identifier.pos, NoFormat), identifier);

		var allUses:Array<Identifier>;
		// find all fully qualified modul names of type
		if (file.packageIdentifier != null) {
			var fullName:String = identifier.defineType.fullModuleName;
			var parts:Array<String> = fullName.split(".");
			parts.pop();
			var prefix:String = parts.join(".") + ".";
			allUses = context.nameMap.getIdentifiers(fullName);
			if (allUses != null) {
				for (use in allUses) {
					RenameHelper.replaceTextWithPrefix(use, prefix, context.what.toName, changelist);
				}
			}
		}
		allUses = context.nameMap.matchIdentifierPart(identifier.name, true);

		var changes:Array<Promise<Void>> = [];
		for (use in allUses) {
			if (use.defineType == null) {
				continue;
			}
			switch (use.file.importsModule(packName, mainModuleName, identifier.name)) {
				case None:
					continue;
				case ImportedWithAlias(alias):
					if (alias != identifier.name) {
						continue;
					}
				case Global | ParentPackage | SamePackage | Imported | StarImported:
			}
			switch (use.type) {
				case Abstract | Class | Enum | Interface | Typedef:
					continue;
				default:
			}
			final searchName:String = if (use.name.startsWith(identifier.name)) identifier.name; else identifier.defineType.fullModuleName;
			changes.push(TypingHelper.findTypeOfIdentifier(context, {
				name: searchName,
				pos: use.pos.start,
				defineType: use.defineType
			}).then(function(typeHint:TypeHintType) {
				switch (typeHint) {
					case null:
					case ClasspathType(type, _):
						if (type.fullModuleName != identifier.defineType.fullModuleName) {
							return;
						}
					case LibType(_) | UnknownType(_) | StructType(_) | FunctionType(_, _) | NamedType(_):
						return;
				}
				if (use.name == identifier.name) {
					changelist.addChange(use.pos.fileName, ReplaceText(context.what.toName, use.pos, NoFormat), use);
					return;
				}
				if (use.name.startsWith('${identifier.name}.')) {
					var newPos:IdentifierPos = {
						fileName: use.pos.fileName,
						start: use.pos.start,
						end: use.pos.start + identifier.name.length
					}
					changelist.addChange(use.pos.fileName, ReplaceText(context.what.toName, newPos, NoFormat), use);
				}
			}));
		}

		return Promise.all(changes).then(function(_) {
			if (mainModuleName == identifier.name) {
				context.fileList.addRecentlyRenamed(file);
			}
			return Promise.resolve(changelist.execute());
		});
	}
}

typedef TypeHintUse = {
	var use:Identifier;
	var promise:Promise<TypeHintType>;
}
