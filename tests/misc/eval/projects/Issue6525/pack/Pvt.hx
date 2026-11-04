package pack;

@:allow(pack)
class Pvt1 {
	static function testPack() {}
}

class Pvt2 {
	@:allow(pack)
	static function testPack() {}

	@:allow(pack.BasePack)
	static function testClass() {}

	@:allow(pack.IPack)
	static function testInterface() {}
}