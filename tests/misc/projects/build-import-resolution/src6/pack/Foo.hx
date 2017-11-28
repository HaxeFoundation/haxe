package pack;

import haxe.macro.*;
class Foo {
	public static function build () {
		return Context.getBuildFields();
	}
}