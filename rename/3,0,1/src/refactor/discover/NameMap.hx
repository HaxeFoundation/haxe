package refactor.discover;

class NameMap {
	var names:IdentifierMap;
	var parts:IdentifierMap;

	public function new() {
		names = new IdentifierMap();
		parts = new IdentifierMap();
	}

	public function getIdentifiers(name:String):Array<Identifier> {
		var results:Null<Array<Identifier>> = names.get(name);
		if (results == null) {
			return [];
		}
		results.sort(Identifier.sortIdentifier);
		return results;
	}

	public function getIdentifier(name:String, file:String, pos:Int):Null<Identifier> {
		var results:Null<Array<Identifier>> = names.get(name);
		if (results == null) {
			return null;
		}
		for (ident in results) {
			if (ident.file.name != file) {
				continue;
			}
			if (ident.pos.start == pos) {
				return ident;
			}
		}
		return null;
	}

	public function addIdentifier(identifier:Identifier) {
		function addToMap(map:IdentifierMap, key:String) {
			var list:Null<Array<Identifier>> = map.get(key);
			if (list == null) {
				map.set(key, [identifier]);
			} else {
				for (id in list) {
					if (id.pos.fileName == identifier.pos.fileName && id.pos.start == identifier.pos.start) {
						return;
					}
				}
				list.push(identifier);
			}
		}
		addToMap(names, identifier.name);
		var nameParts:Array<String> = identifier.name.split(".");
		for (part in nameParts) {
			addToMap(parts, part);
		}
	}

	public function getStartsWith(prefix:String):Array<Identifier> {
		var results:Array<Identifier> = [];
		for (name => list in names) {
			if (name.startsWith(prefix)) {
				results = results.concat(list);
			}
		}
		results.sort(Identifier.sortIdentifier);
		return results;
	}

	public function matchIdentifierPart(name:String, unused:Bool):Array<Identifier> {
		var results:Null<Array<Identifier>> = parts.get(name);
		if (results == null) {
			return [];
		}
		if (unused) {
			results.sort(Identifier.sortIdentifier);
		}
		return results;
	}

	public function removeFile(fileName:String) {
		var newNames:IdentifierMap = new IdentifierMap();
		for (key => idents in names) {
			newNames.set(key, idents.filter(id -> id.pos.fileName != fileName));
		}
		names = newNames;

		var newParts:IdentifierMap = new IdentifierMap();
		for (key => idents in parts) {
			newParts.set(key, idents.filter(id -> id.pos.fileName != fileName));
		}
		parts = newParts;
	}

	public function clear() {
		names.clear();
		parts.clear();
	}
}

typedef IdentifierMap = Map<String, Array<Identifier>>;
