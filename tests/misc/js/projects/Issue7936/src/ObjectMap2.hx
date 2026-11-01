class ObjectMap2 {
	static public function main() {
		var inst:{} = js.Syntax.code('global.inst');

		var map:Map<{},String> = [{field:1} => 'second'];
		if(map.exists(inst)) {
			throw 'Cross-module ObjectMap failed.';
		}
	}
}