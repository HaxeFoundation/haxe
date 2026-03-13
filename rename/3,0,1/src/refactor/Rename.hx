package refactor;

import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.discover.IdentifierPos;
import refactor.rename.CanRenameContext;
import refactor.rename.CanRenameResult;
import refactor.rename.RenameAnonStructField;
import refactor.rename.RenameContext;
import refactor.rename.RenameEnumField;
import refactor.rename.RenameField;
import refactor.rename.RenameImportAlias;
import refactor.rename.RenameModuleLevelStatic;
import refactor.rename.RenamePackage;
import refactor.rename.RenameScopedLocal;
import refactor.rename.RenameTypeName;

class Rename {
	public static function canRename(context:CanRenameContext):Promise<CanRenameResult> {
		var file:Null<File> = context.fileList.getFile(context.what.fileName);
		if (file == null) {
			return Promise.reject(RefactorResult.NotFound.printRenameResult());
		}
		var identifier:Identifier = file.getIdentifier(context.what.pos);
		if (identifier == null) {
			return Promise.reject(RefactorResult.NotFound.printRenameResult());
		}
		return switch (identifier.type) {
			case PackageName | ImportAlias | Abstract | Class | Enum | Interface | Typedef | ModuleLevelStaticVar | ModuleLevelStaticMethod | Property |
				FieldVar(_) | Method(_) | TypedefField(_) | StructureField(_) | InterfaceProperty | InterfaceVar | InterfaceMethod | EnumField(_) |
				ScopedLocal(_, _):
				Promise.resolve({name: identifier.name, pos: identifier.pos});
			case ImportModul | UsingModul | Extends | Implements | AbstractOver | AbstractFrom | AbstractTo | TypeHint | StringConst | TypedParameter |
				TypedefBase | Call(true) | CaseLabel(_) | Meta:
				Promise.reject(RefactorResult.Unsupported(identifier.toString()).printRenameResult());
			case Call(false) | Access | ArrayAccess(_) | ForIterator:
				var candidate:Null<Identifier> = findActualWhat(context, file, identifier);
				if (candidate == null) {
					return Promise.reject(RefactorResult.Unsupported(identifier.toString()).printRenameResult());
				}
				if (identifier.name.startsWith(candidate.name)) {
					var pos:IdentifierPos = {
						fileName: identifier.pos.fileName,
						start: identifier.pos.start,
						end: identifier.pos.start + context.what.toName.length
					}
					return Promise.resolve({name: candidate.name, pos: pos});
				}
				if (identifier.name.endsWith(candidate.name)) {
					var pos:IdentifierPos = {
						fileName: identifier.pos.fileName,
						start: identifier.pos.end - context.what.toName.length,
						end: identifier.pos.end
					}
					return Promise.resolve({name: candidate.name, pos: pos});
				}
				Promise.reject(RefactorResult.Unsupported(identifier.toString()).printRenameResult());
		}
	}

	public static function rename(context:RenameContext):Promise<RefactorResult> {
		var file:Null<File> = context.fileList.getFile(context.what.fileName);
		if (file == null) {
			return Promise.reject(RefactorResult.NotFound.printRenameResult());
		}
		var identifier:Identifier = file.getIdentifier(context.what.pos);
		if (identifier == null) {
			return Promise.reject(RefactorResult.NotFound.printRenameResult());
		}
		if (identifier.name == context.what.toName) {
			return Promise.reject(RefactorResult.NotFound.printRenameResult());
		}
		return switch (identifier.type) {
			case PackageName:
				context.verboseLog('rename package name "${identifier.name}" to "${context.what.toName}"');
				RenamePackage.refactorPackageName(context, file, identifier);
			case ImportModul | UsingModul:
				Promise.reject(RefactorResult.Unsupported(identifier.toString()).printRenameResult());
			case ImportAlias:
				context.verboseLog('rename import alias "${identifier.name}" to "${context.what.toName}"');
				RenameImportAlias.refactorImportAlias(context, file, identifier);
			case Abstract | Class | Enum | Interface | Typedef:
				context.verboseLog('rename type name "${identifier.name}" to "${context.what.toName}"');
				RenameTypeName.refactorTypeName(context, file, identifier);
			case ModuleLevelStaticVar | ModuleLevelStaticMethod:
				context.verboseLog('rename module level static "${identifier.name}" to "${context.what.toName}"');
				RenameModuleLevelStatic.refactorModuleLevelStatic(context, file, identifier);
			case Extends | Implements | AbstractOver | AbstractFrom | AbstractTo | TypeHint | StringConst | Meta:
				Promise.reject(RefactorResult.Unsupported(identifier.toString()).printRenameResult());
			case Property:
				context.verboseLog('rename property "${identifier.name}" to "${context.what.toName}"');
				RenameField.refactorField(context, file, identifier, false);
			case FieldVar(isStatic):
				context.verboseLog('rename ${isStatic ? "static " : ""}field "${identifier.name}" to "${context.what.toName}"');
				RenameField.refactorField(context, file, identifier, isStatic);
			case Method(isStatic):
				context.verboseLog('rename ${isStatic ? "static " : ""}class method "${identifier.name}" to "${context.what.toName}"');
				RenameField.refactorField(context, file, identifier, isStatic);
			case TypedParameter:
				Promise.reject(RefactorResult.Unsupported(identifier.toString()).printRenameResult());
			case TypedefBase:
				Promise.reject(RefactorResult.Unsupported(identifier.toString()).printRenameResult());
			case TypedefField(fields):
				RenameAnonStructField.refactorAnonStructField(context, file, identifier, fields);
			case StructureField(fields):
				RenameAnonStructField.refactorStructureField(context, file, identifier, fields);
			case InterfaceProperty | InterfaceVar | InterfaceMethod:
				context.verboseLog('rename interface field "${identifier.name}" to "${context.what.toName}"');
				RenameField.refactorField(context, file, identifier, false);
			case EnumField(_):
				context.verboseLog('rename enum field "${identifier.name}" to "${context.what.toName}"');
				RenameEnumField.refactorEnumField(context, file, identifier);
			case Call(true):
				Promise.reject(RefactorResult.Unsupported(identifier.toString()).printRenameResult());
			case Call(false) | Access | ArrayAccess(_) | ForIterator:
				context.verboseLog('rename "${identifier.name}" at call/access location - trying to find definition');
				var candidate:Null<Identifier> = findActualWhat(context, file, identifier);
				if (candidate == null) {
					return Promise.reject(RefactorResult.Unsupported(identifier.toString()).printRenameResult());
				}
				context.what.pos = candidate.pos.start;
				rename(context);
			case CaseLabel(_):
				Promise.reject(RefactorResult.Unsupported(identifier.toString()).printRenameResult());
			case ScopedLocal(scopeStart, scopeEnd, type):
				context.verboseLog('rename scoped local "${identifier.name}" (${type.scopeTypeToString()}) to "${context.what.toName}"');
				RenameScopedLocal.refactorScopedLocal(context, file, identifier, scopeStart, scopeEnd);
		}
	}

	static function findActualWhat(context:CanRenameContext, file:File, identifier:Identifier):Null<Identifier> {
		var parts:Array<String> = identifier.name.split(".");
		if (parts.length <= 0) {
			return null;
		}
		var firstPart:String = parts.shift();
		var onlyFields:Bool = false;
		var offset:Int = 0;
		if (firstPart == "this") {
			firstPart = parts.shift();
			onlyFields = true;
			offset = 5;
		}
		if (context.what.pos > identifier.pos.start + firstPart.length + offset) {
			// rename position is not in first part of dotted identifiier
			return null;
		}
		var allUses:Array<Identifier> = file.findAllIdentifiers((i) -> i.name == firstPart);
		var candidate:Null<Identifier> = null;
		for (use in allUses) {
			switch (use.type) {
				case Property | FieldVar(_) | Method(_):
					if (identifier.defineType.name != use.defineType.name) {
						continue;
					}
					if (candidate == null) {
						candidate = use;
					}
				case ScopedLocal(scopeStart, scopeEnd, ForLoop(_)) if (!onlyFields):
					if ((scopeStart < identifier.pos.start) && (identifier.pos.start < scopeEnd)) {
						candidate = use;
					}
				case ScopedLocal(scopeStart, scopeEnd, _) if (!onlyFields):
					if ((scopeStart < identifier.pos.start) && (identifier.pos.start < scopeEnd)) {
						candidate = use;
					}
				default:
			}
		}
		return candidate;
	}
}
