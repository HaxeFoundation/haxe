package refactor;

import refactor.discover.IdentifierType;
import refactor.typing.TypeHintType;
import refactor.typing.TypingHelper.TypeParameterList;

class PrintHelper {
	public static function typeToString(identType:IdentifierType):String {
		return switch (identType) {
			case FieldVar(isStatic):
				'FieldVar(${isStatic})';
			case Method(isStatic):
				'Method(${isStatic})';
			case TypedefField(fields):
				final fieldnames = [
					for (field in fields) {
						return switch (field) {
							case Required(identifier) | Optional(identifier):
								identifier.name;
						}
					}
				];
				'TypedefField(${fieldnames.join(", ")})';
			case StructureField(fieldNames):
				'StructureField(${fieldNames})';
			case EnumField(params):
				'EnumField(${params.map((p) -> '"${p.name}"')})';
			case CaseLabel(switchIdentifier):
				'CaseLabel(${switchIdentifier.name})';
			case ScopedLocal(scopeStart, scopeEnd, scopeType):
				'ScopedLocal($scopeStart - $scopeEnd, ${scopeType.scopeTypeToString()})';
			default:
				'$identType';
		}
	}

	public static function scopeTypeToString(scopeType:ScopedLocalType):String {
		return switch (scopeType) {
			case Parameter(params):
				'Parameter(${params.map((i) -> '"${i.name}"')})';
			case ForLoop(loopIdentifiers):
				'ForLoop(${loopIdentifiers.map((i) -> '"${i.name}"')})';
			default:
				'$scopeType';
		}
	}

	public static function typeHintToString(hintType:Null<TypeHintType>):String {
		if (hintType == null) {
			return "null";
		}
		return switch (hintType) {
			case ClasspathType(type, paramList):
				if (paramList.length > 0) {
					final params = paramList.map(p -> typeHintToString(p));
					return 'ClasspathType(${type.name.name}<${params.join(", ")}>)';
				}
				'ClasspathType(${type?.name.name}, <>)';
			case LibType(name, fullName, paramList):
				if (paramList.length > 0) {
					final params = paramList.map(p -> typeHintToString(p));
					return 'LibType($name, $fullName, <${params.join(", ")}>';
				}
				'LibType($name, $fullName, <>)';
			case FunctionType(argTypes, retVal):
				final args = argTypes.map(f -> typeHintToString(f));
				if (argTypes == null) {
					return 'FunctionType((${args.join(", ")}) -> Void)';
				}
				return 'FunctionType((${args.join(", ")}) -> ${typeHintToString(retVal)})';
			case StructType(fieldTypes):
				final fields = fieldTypes.map(f -> typeHintToString(f));
				'StructType({${fields.join(";")}})';
			case NamedType(name, namedHint):
				'NamedType($name, ${typeHintToString(namedHint)})';
			case UnknownType(name):
				'UnknownType($name)';
		}
	}

	public static function printTypeHint(hintType:TypeHintType):String {
		return switch (hintType) {
			case ClasspathType(type, paramList):
				if (paramList.length > 0) {
					final params = paramList.map(p -> printTypeHint(p));
					return '${type.name.name}<${params.join(", ")}>';
				}
				'${type.name.name}';
			case LibType(name, fullName, paramList):
				if (paramList.length > 0) {
					final params = paramList.map(p -> printTypeHint(p));
					return '${name}<${params.join(", ")}>';
				}
				'$name';
			case FunctionType(argTypes, retVal):
				final args = argTypes.map(f -> printTypeHint(f));
				if (argTypes == null) {
					return '(${args.join(", ")}) -> Void';
				}
				if (argTypes.length == 1) {
					return '${args.join(", ")} -> ${printTypeHint(retVal)}';
				}
				return '(${args.join(", ")}) -> ${printTypeHint(retVal)}';
			case StructType(fieldTypes):
				final fields = fieldTypes.map(f -> printTypeHint(f));
				'{${fields.join(", ")}}';
			case NamedType(name, namedHint):
				'$name:${printTypeHint(namedHint)}';
			case UnknownType(name):
				'$name';
		}
	}

	public static function printRenameResult(result:RefactorResult):String {
		return switch (result) {
			case NoChange:
				"nothing to do";
			case NotFound:
				"could not find identifier to rename";
			case Unsupported(name):
				"renaming not supported for " + name;
			case DryRun:
				"dry run - no changes were made";
			case Done:
				"rename successful";
		}
	}

	public static function printRefactorResult(result:RefactorResult):String {
		return switch (result) {
			case NoChange:
				"nothing to do";
			case NotFound:
				"could not find identifier to refactor";
			case Unsupported(name):
				"refactor not supported for " + name;
			case DryRun:
				"dry run - no changes were made";
			case Done:
				"refactor successful";
		}
	}
}
