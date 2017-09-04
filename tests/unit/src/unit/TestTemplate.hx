package unit;

class TestTemplate extends Test {

	function testTemplate() {

		var tpl = new haxe.Template("My names are ::foreach names::::if (name != null)::<strong>::name.first::</strong>::end::::end:: and I'm <em>::age::</em> years old.");

		var output = tpl.execute( { names : [new TemplateTestName ("John"), new TemplateTestName (null)], age : 33 } );
		eq(output, "My names are <strong>John</strong><strong>null</strong> and I'm <em>33</em> years old.");

		output = tpl.execute( { names : [{ name : { first : "John" }}, { name : { first : null }}], age : 33 } );
		eq(output, "My names are <strong>John</strong><strong>null</strong> and I'm <em>33</em> years old.");

		// php7's Reflect class tried to resolve null in "name != null" as a field of object: ErrorException: Undefined property: unit\Name::$null
		var nullName = new TemplateTestName (null);
		nullName.name = null;

		output = tpl.execute( { names : [new TemplateTestName("John"), nullName], age : 33 } );
		eq(output, "My names are <strong>John</strong> and I'm <em>33</em> years old.");

		// worked fine with anonymous objects
		output = tpl.execute( { names : [{ name : { first : "John" }}, { name : null }], age : 33 } );
		eq(output, "My names are <strong>John</strong> and I'm <em>33</em> years old.");
	}
}

class TemplateTestName {
	public var name : TemplateTestPart;
	public function new (first : String) { name = new TemplateTestPart(first); }
}

class TemplateTestPart {
	public var first : String;
	public function new (first : String) {this.first = first; }
}
