package unit.issues;

class Issue7332 extends Test {
	public static function main() {
		forEach([new FlxSprite()], sprite -> trace(sprite));
	}

	static function forEach<T:FlxBasic>(l:Array<T>, f:T->Void) {}
}

class FlxBasic {}

class FlxSprite extends FlxBasic {
	public function new() {}
}
