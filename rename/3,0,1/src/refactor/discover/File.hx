package refactor.discover;

import haxe.io.Path;
import sys.FileStat;
import sys.FileSystem;

class File {
	public var name:String;
	public var packageIdentifier:Null<Identifier>;
	public var importHxFile:Null<File>;
	public var importList:Array<Import>;
	public var typeList:Array<Type>;
	public var importInsertPos:Int;
	public var fileDate:Float;
	public var fileSize:Int;

	public function new(name:String) {
		this.name = name;

		var stat:FileStat = FileSystem.stat(name);
		fileDate = stat.mtime.getTime();
		fileSize = stat.size;
		importList = [];
		typeList = [];
	}

	public function initHeader(packageIdent:Null<Identifier>, imports:Array<Import>, posForImport:Int) {
		packageIdentifier = packageIdent;
		importList = imports;
		importInsertPos = posForImport;
	}

	public function setTypes(types:Array<Type>) {
		typeList = types;
	}

	public function getPackage():String {
		if (packageIdentifier != null) {
			return packageIdentifier.name;
		}
		return "";
	}

	public function importsModule(packName:String, moduleName:String, typeName:String):ImportStatus {
		if (packName.length <= 0) {
			return Global;
		}
		var fullModule:String = '$packName.$moduleName';
		var fullSubModule:Null<String> = null;
		var isMainModule:Bool = true;
		if (moduleName != typeName) {
			fullSubModule = '$fullModule.$typeName';
			isMainModule = false;
		}
		for (importEntry in importList) {
			if (importEntry.moduleName.name == fullModule) {
				if (importEntry.alias != null) {
					return ImportedWithAlias(importEntry.alias.name);
				}
				return Imported;
			}
			if (importEntry.moduleName.name == fullSubModule) {
				if (importEntry.alias != null) {
					return ImportedWithAlias(importEntry.alias.name);
				}
				return Imported;
			}
			if (isMainModule && importEntry.starImport) {
				if (importEntry.moduleName.name == packName) {
					return StarImported;
				}
			}
		}
		final parentPackage = '$packName.';
		if (getPackage().startsWith(parentPackage)) {
			return ParentPackage;
		}

		if (importHxFile == null) {
			if (packName == getPackage()) {
				return SamePackage;
			}
			if (packName == getPackage()) {
				return SamePackage;
			}
			return None;
		}
		var result:ImportStatus = importHxFile.importsModule(packName, moduleName, typeName);
		if (result == None) {
			if (packName == getPackage()) {
				return SamePackage;
			}
		}
		return result;
	}

	public function getMainModulName():String {
		var path:Path = new Path(name);

		return path.file;
	}

	public function getType(typeName:String):Null<Type> {
		for (type in typeList) {
			if (type.name.name == typeName) {
				return type;
			}
		}
		return null;
	}

	public function getIdentifier(pos:Int):Null<Identifier> {
		if (packageIdentifier != null && packageIdentifier.containsPos(pos)) {
			return packageIdentifier;
		}
		for (imp in importList) {
			if (imp.alias != null && imp.alias.containsPos(pos)) {
				return imp.alias;
			}
			if (imp.moduleName.containsPos(pos)) {
				return imp.moduleName;
			}
		}
		for (type in typeList) {
			var identifier:Identifier = type.findIdentifier(pos);
			if (identifier != null) {
				return identifier;
			}
		}
		return null;
	}

	public function findAllIdentifiers(matcher:IdentifierMatcher):Array<Identifier> {
		var results:Array<Identifier> = [];
		if (packageIdentifier != null && matcher(packageIdentifier)) {
			results.push(packageIdentifier);
		}
		for (imp in importList) {
			if (imp.alias != null && matcher(imp.alias)) {
				results.push(imp.alias);
			}
			if (matcher(imp.moduleName)) {
				results.push(imp.moduleName);
			}
		}
		for (type in typeList) {
			results = results.concat(type.findAllIdentifiers(matcher));
		}
		results.sort(Identifier.sortIdentifier);
		return results;
	}

	public function clear() {
		packageIdentifier = null;
		importHxFile = null;
		importList = [];
		typeList = [];
	}
}

typedef Import = {
	var moduleName:Identifier;
	@:optional var alias:Null<Identifier>;
	var starImport:Bool;
}

typedef ImportAlias = {
	var name:String;
	var pos:IdentifierPos;
}

enum ImportStatus {
	None;
	Global;
	SamePackage;
	ParentPackage;
	Imported;
	ImportedWithAlias(alias:String);
	StarImported;
}
