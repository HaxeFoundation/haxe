class Main {
	static function main() {}
}

class C1 {
	public function a() {}
}

@:keep
class C2 extends C1 {
	@:native("z") override public function a() {}
}