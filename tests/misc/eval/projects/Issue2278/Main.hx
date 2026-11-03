using Macro;

class Main {
	static function main() {
		write(Macro.call(function(x) return x * 3, 3));
		write(Macro.call(MyTools.double, 3));
		write(MyTools.double.call(3));
		write(Macro.call(MyTools.double, Macro.call(MyTools.double, 3)));

		write(Macro.call2(function(x) return x * 3, 3));
		write(Macro.call2(MyTools.double, 3));
		write(MyTools.double.call2(3));
		write(Macro.call3(MyTools.double, Macro.call(MyTools.double, 3)));
	}

	static function write(i:Int) {
		Sys.stderr().writeString(i + "\n");
	}
}