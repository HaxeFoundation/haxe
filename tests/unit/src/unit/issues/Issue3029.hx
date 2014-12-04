package unit.issues;

private abstract TemplateData({}) from {} to {} { }

class Issue3029 extends Test {
	function test() {
        var t1:TemplateData = {};
        var t2:TemplateData = { name: "jason" };
		eq("jason", Reflect.field(t2,"name"));
	}
}