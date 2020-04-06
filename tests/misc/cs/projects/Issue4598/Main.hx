class Main {
	@:readOnly
	public var a:Int = 10;

	static function main() {
		var m = new Main();

		try Reflect.setProperty(m, 'a', 999)
		catch(e:cs.system.MemberAccessException) {}
		if(m.a != 10) {
			throw "Main.a should not be writable via reflection";
		}
	}

	public function new() {}
}