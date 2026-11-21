enum abstract Anim(String) {
	var Crouch;
	var Walk;
	var Jump;
	public inline function isCrouching() {
		return switch (abstract) {
			case Crouch: true;
			default: false;
		}
	}
	public static function blend(a:Anim):String {
		return switch (a) {
			case Jump: "jump";
			default: null;
		}
	}
	public static function isLoop(a:Anim):Bool {
		return switch (a) {
			case Jump: false;
			default: !a.isCrouching();
		}
	}
}
