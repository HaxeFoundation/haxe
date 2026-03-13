package refactor.rename;

import refactor.RefactorResult;
import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.discover.IdentifierType.TypedefFieldType;
import refactor.discover.Type;
import refactor.edits.Changelist;
import refactor.rename.RenameContext;

class RenameAnonStructField {
	public static function refactorAnonStructField(context:RenameContext, file:File, identifier:Identifier,
			fields:Array<TypedefFieldType>):Promise<RefactorResult> {
		var changelist:Changelist = new Changelist(context);

		changelist.addChange(identifier.pos.fileName, ReplaceText(context.what.toName, identifier.pos, NoFormat), identifier);

		return renameFieldsOfType(context, changelist, identifier.defineType, fields, identifier.name).then(function(result):RefactorResult {
			return changelist.execute();
		});
	}

	public static function refactorStructureField(context:RenameContext, file:File, identifier:Identifier, fieldNames:Array<String>):Promise<RefactorResult> {
		var allUses:Array<Identifier> = context.nameMap.getIdentifiers(identifier.name);
		for (use in allUses) {
			switch (use.type) {
				case TypedefField(fields):
					fields = fields.concat(findBaseTypes(context, use.defineType));
					if (matchesFields(fields, fieldNames)) {
						return refactorAnonStructField(context, use.file, use, fields);
					}
				default:
					continue;
			}
		}
		return Promise.resolve(Unsupported(identifier.toString()));
	}

	static function renameFieldsOfType(context:RenameContext, changelist:Changelist, type:Type, fields:Array<TypedefFieldType>, fromName:String):Promise<Void> {
		var packName:String = type.file.getPackage();
		var mainModuleName = type.file.getMainModulName();
		fields = fields.concat(findBaseTypes(context, type));

		var allUses:Array<Identifier> = context.nameMap.matchIdentifierPart(fromName, true);
		var promises:Array<Promise<Void>> = [];
		for (use in allUses) {
			switch (use.type) {
				case StructureField(fieldNames):
					if (!matchesFields(fields, fieldNames)) {
						continue;
					}
				case Call(false) | Access:
					promises.push(RenameHelper.replaceSingleAccessOrCall(context, changelist, use, fromName, [type]));
					continue;
				default:
					continue;
			}
			switch (use.file.importsModule(packName, mainModuleName, type.name.name)) {
				case None:
					continue;
				case Global | ParentPackage | SamePackage | Imported | ImportedWithAlias(_) | StarImported:
			}
			changelist.addChange(use.pos.fileName, ReplaceText(context.what.toName, use.pos, NoFormat), use);
		}

		promises.push(findAllExtending(context, changelist, type, fields, fromName));
		return Promise.all(promises).then(null);
	}

	static function findAllExtending(context:RenameContext, changelist:Changelist, type:Type, fields:Array<TypedefFieldType>, fromName:String):Promise<Void> {
		var allUses:Array<Identifier> = context.nameMap.getIdentifiers(type.name.name);
		var promises:Array<Promise<Void>> = [];
		for (use in allUses) {
			switch (use.type) {
				case TypedefBase:
					promises.push(renameFieldsOfType(context, changelist, use.defineType, getFieldsOfTypedef(use.defineType), fromName));
				default:
			}
		}
		return Promise.all(promises).then(null);
	}

	static function matchesFields(fields:Array<TypedefFieldType>, fieldNames:Array<String>):Bool {
		var allowedFieldNames:Array<String> = [];
		for (field in fields) {
			switch (field) {
				case Required(identifier):
					allowedFieldNames.push(identifier.name);
					if (!fieldNames.contains(identifier.name)) {
						return false;
					}
				case Optional(identifier):
					allowedFieldNames.push(identifier.name);
			}
		}
		for (fieldName in fieldNames) {
			if (!allowedFieldNames.contains(fieldName)) {
				return false;
			}
		}
		return true;
	}

	static function findBaseTypes(context:RenameContext, type:Type):Array<TypedefFieldType> {
		var fieldTypes:Array<TypedefFieldType> = [];
		var baseTypes:Array<Identifier> = type.findAllIdentifiers((i) -> i.type.match(TypedefBase));
		for (baseTypeName in baseTypes) {
			var base:Null<Type> = findBase(context, baseTypeName);
			if (base == null) {
				continue;
			}
			fieldTypes = fieldTypes.concat(getFieldsOfTypedef(base));
		}
		return fieldTypes;
	}

	static function getFieldsOfTypedef(type:Type):Array<TypedefFieldType> {
		var allChilds:Array<Identifier> = type.findAllIdentifiers((i) -> true);
		for (child in allChilds) {
			switch (child.type) {
				case TypedefField(fields):
					return fields;
				default:
			}
		}
		return [];
	}

	static function findBase(context:RenameContext, baseTypeName:Identifier):Null<Type> {
		var allUses:Array<Identifier> = context.nameMap.getIdentifiers(baseTypeName.name);
		for (use in allUses) {
			switch (use.type) {
				case Typedef:
				default:
					continue;
			}
			switch (baseTypeName.file.importsModule(use.file.getPackage(), use.file.getMainModulName(), baseTypeName.name)) {
				case None:
					continue;
				case Global | ParentPackage | SamePackage | Imported | ImportedWithAlias(_) | StarImported:
			}
			return use.defineType;
		}
		return null;
	}
}
