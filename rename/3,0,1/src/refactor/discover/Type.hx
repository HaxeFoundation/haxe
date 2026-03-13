package refactor.discover;

class Type {
	var nameMap:NameMap;

	public var uses:Array<Identifier>;
	public var file:File;
	public var name:Null<Identifier>;
	public var fullModuleName(get, null):String;

	public function new(file:File) {
		this.file = file;
		nameMap = new NameMap();
		uses = [];
	}

	public function get_fullModuleName():String {
		if (fullModuleName == null) {
			fullModuleName = makeFullModuleName();
		}
		return fullModuleName;
	}

	function makeFullModuleName():String {
		var modulName:String = '${file.getMainModulName()}.';
		if (file.getMainModulName() == name.name) {
			modulName = "";
		}
		var packageName:String = file.getPackage();
		if (packageName.length <= 0) {
			return modulName + name.name;
		}
		return '$packageName.$modulName${name.name}';
	}

	public function addIdentifier(identifier:Identifier) {
		nameMap.addIdentifier(identifier);
		uses.push(identifier);
	}

	public function getIdentifiers(search:String):Array<Identifier> {
		return nameMap.getIdentifiers(search);
	}

	public function findIdentifier(offset:Int):Null<Identifier> {
		var identifier:Null<Identifier> = name.findIdentifier(offset);
		if (identifier != null) {
			return identifier;
		}
		for (use in uses) {
			identifier = use.findIdentifier(offset);
			if (identifier != null) {
				return identifier;
			}
		}
		return null;
	}

	public function findAllIdentifiers(matcher:IdentifierMatcher):Array<Identifier> {
		var results:Array<Identifier> = [];
		if (matcher(name)) {
			results = [name];
		}
		for (use in uses) {
			if (matcher(use)) {
				results.push(use);
			}
		}
		results.sort(Identifier.sortIdentifier);
		return results;
	}

	public function getStartsWith(prefix:String):Array<Identifier> {
		return nameMap.getStartsWith(prefix);
	}
}
