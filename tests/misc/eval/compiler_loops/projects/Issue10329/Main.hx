import haxe.MainLoop;

class Main {
	static function main() {
		var e1 = null;
		var e2 = null;
		e1 = MainLoop.add(() -> {
			e1.stop();
		});
		e2 = MainLoop.add(() -> {
			e2.stop();
		});
	}
}