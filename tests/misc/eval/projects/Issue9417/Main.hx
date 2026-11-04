using Main.Ext;

abstract D(Dynamic) to Dynamic {}

class Ext {
	public static function round(f:Float) {}
}

class Main {
	static function main() {
		var d:D = null;
		d.round();
	}
}