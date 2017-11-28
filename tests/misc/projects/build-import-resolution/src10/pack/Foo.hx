package pack;

import haxe.macro.*;
class Foo {
	public static function notBuild () {
		return Context.getBuildFields();
	}
}