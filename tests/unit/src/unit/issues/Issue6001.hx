package unit.issues;
class Issue6001 extends Test {
#if lua
	public function test() {
		var t = lua.Table.create({a : 1});
		eq("{ a : 1 }", t + '');

		var t = lua.Table.create([1,2,3]);
		eq("{ 1 : 1, 2 : 2, 3 : 3 }", t + '');

		var t = lua.Table.create([1,2,3], {a : 1});
		eq("{ 1 : 1, 2 : 2, 3 : 3, a : 1 }", t + '');
	}
#end
}
