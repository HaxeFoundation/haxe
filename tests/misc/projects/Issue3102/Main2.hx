import pack1.pack2.Test;

class Main2 {
	static function main() {
		var test = Type.createInstance(Type.resolveClass("pack1.pack2.Test"), []);
		test.get12();
	}
}