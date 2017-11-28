package pack;

import haxe.macro.*;
class Foo {
	public static function Build () {
		return Context.getBuildFields();
	}
}