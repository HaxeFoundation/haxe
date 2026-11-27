enum E {
	A;
}

@:build(Gen.build())
class Features {
	public static function enumName(e:Enum<Dynamic>):String {
		return Type.getEnumName(e);
	}
}

class Main {
	static function main() {
		trace(Features.enumName(E));
		Features.useAll();
	}
}
