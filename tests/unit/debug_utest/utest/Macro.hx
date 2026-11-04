package utest;

class Macro {

	public static var GENERIC = false;

	public static function buildTests() {
		var fields = haxe.macro.Context.getBuildFields();
		var count = 0;
		var run = [];
		var isRoot = haxe.macro.Context.getLocalModule() == "unit.Test";
		var pos = haxe.macro.Context.currentPos();
		for( f in fields ) {
			if( StringTools.startsWith(f.name,"test") ) {
				if( f.access.indexOf(AStatic) >= 0 ) continue;
				switch (f.kind) {
				case FFun(ff) if (ff.args.length == 0):
					run.push(macro $i{f.name}());
				default:
				}
			} else if( isRoot ) {
				switch( f.kind ) {
				case FFun(ff) if( ff.args.length > 0 ):
					var a = ff.args[ff.args.length-1];
					a.type = null; // remove PosInfos
					if( ff.params.length > 0 && GENERIC )
						f.meta.push({name: ":generic", pos: pos});
				default:
				}
			}
		}
		fields.push({
			name : "runTests",
			access: isRoot ? [APublic] : [APublic,AOverride],
			kind : FFun({
				expr : { expr : EBlock(run), pos : pos },
				args : [],
			}),
			pos : pos,
		});

		return fields;
	}

}