import haxe.macro.Context;
import sys.FileSystem;
import sys.io.File;

class Macro {
	macro static function build():haxe.macro.Expr {
		return macro null;
	}
}