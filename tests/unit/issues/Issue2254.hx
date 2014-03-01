package unit.issues;
import unit.Test;
import haxe.Template;

class Issue2254 extends Test {
	function test()
	{
		var str = 'HI, IT IS ::if !isIt::NOT::end::';
		var t = new haxe.Template(str);
		eq("HI, IT IS ", t.execute({ isIt:true }));
		eq("HI, IT IS NOT", t.execute({ isIt:false }));
	}
}
