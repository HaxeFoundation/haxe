package;

import haxe.macro.*;
class MacroErrors {
	public static function build1 () {
		return Context.getBuildFields();
	}

	public static function build2 () {
		return Context.getBuildFields();
	}

	public static function build3 () {
		return Context.getBuildFields();
	}
}