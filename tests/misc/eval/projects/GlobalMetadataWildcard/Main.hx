import haxe.rtti.Meta;
import pack.A;
import pack.M;
import pack.M.B;
import pack.sub.C;

class Main {
	static function hasType(c:Class<Dynamic>, name:String):Bool {
		var m = Meta.getType(c);
		return m != null && Reflect.hasField(m, name);
	}

	static function hasField(c:Class<Dynamic>, field:String, name:String):Bool {
		var m = Meta.getFields(c);
		if (m == null)
			return false;
		var fm = Reflect.field(m, field);
		return fm != null && Reflect.hasField(fm, name);
	}

	static function report(name:String, c:Class<Dynamic>) {
		Sys.println(name
			+ " onestar=" + hasType(c, "onestar")
			+ " twostar=" + hasType(c, "twostar")
			+ " foo.fonestar=" + hasField(c, "foo", "fonestar")
			+ " foo.ftwostar=" + hasField(c, "foo", "ftwostar"));
	}

	static function main() {
		report("A", A);
		report("M", M);
		report("B", B);
		report("C", C);
	}
}
