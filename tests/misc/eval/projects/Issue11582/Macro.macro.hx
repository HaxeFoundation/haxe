class Macro {
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
				public static var macros:Array<String> = [];
			}).fields.concat(fields);

		// one field per build of the class (macro statics are reset for every expansion)
		var id = '_' + [for (f in fields) if (StringTools.startsWith(f.name, "_")) f].length;
		fields.push((macro class A {
			 static var $id = {macros.push($v{i}); 0;};
		}).fields[0]);

		return fields;
	}
}
