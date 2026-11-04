class ObjectMap1 {
	static var tmp:Any;

	static public function main() {
		var inst = {};
		js.Syntax.code('global.inst = {0}', inst);
		tmp = [inst => 'first'];
	}
}