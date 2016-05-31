package unit.issues;

class Issue5293 extends unit.Test {
	function test() {
		var template = new haxe.Template("::foreach names:: hello ::__current__:: ::end::");
		eq(" hello wwx2016 ", template.execute({names:["wwx2016"]}));
	}
}