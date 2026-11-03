class Macro {
	static var id = 0;
	static function build(i:String) {
		if (haxe.macro.Context.getLocalClass().get().isInterface) return null;

		var hasMacros = false;
		var fields = haxe.macro.Context.getBuildFields();
		for (f in fields) {
			if (f.name == "macros") {
				hasMacros = true;
				break;
			}
		}

		if (!hasMacros)
			fields = (macro class A {
				public static var macros = [];
			}).fields.concat(fields);

		var id = '_' + id++;
		fields.push((macro class A {
			 static var $id = {macros.push($v{i}); 0;};
		}).fields[0]);

		return fields;
	}
}
