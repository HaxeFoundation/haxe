package cases.display.issues;

class Issue13062 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				switch( Other.v ) {
					case Dev(name):
						trace(na{-1-}me);
				}
			}
		}
	**/
	function testHoverInEnumCaseAfterCompile(_) {
		vfs.putContent("PlatformID.hx", "enum PlatformID {
	Dev(name:String);
}");
		vfs.putContent("Other.hx", "class Other {
	public static var v:PlatformID;
}");
		var args = ["-main", "Main", "--no-output", "-js", "no.js"];
		runHaxe(args);
		assertSuccess();
		var result = runHaxeJson(args, DisplayMethods.Hover, {file: file, offset: offset(1)});
		eq("String", printer.printType(result.item.type));
	}
}
