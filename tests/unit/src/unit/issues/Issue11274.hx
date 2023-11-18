package unit.issues;

using unit.issues.Issue11274.SpriteTools;

private class Sprite {
	public function new() {}
}

private class SpriteTools {
	public static function setName(sprite:Sprite, name:String):Void {}
}

class Issue11274 extends unit.Test {
	static var mark = false;

	function test() {
		sprite(foo -> {
			foo.setName("foo");
			add(foo); // err
		});
		t(mark);
	}

	extern inline overload static function sprite(callback:(sprite:Sprite) -> Void) {
		callback(new Sprite());
	}

	extern inline overload static function sprite(name:String, callback:(sprite:Sprite) -> Void) {}

	static function add(sprite:Sprite):Void {
		mark = true;
	}
}
