// normal
var tpl = new haxe.Template("My name is <strong>::name::</strong> and I'm <em>::age::</em> years old.");
var output = tpl.execute( { name : "John", age : 33 } );
output == "My name is <strong>John</strong> and I'm <em>33</em> years old.";

// not existent
var output2 = tpl.execute( { } );
output2 == "My name is <strong>null</strong> and I'm <em>null</em> years old.";

// globals
haxe.Template.globals = { name : "John", age : 33 };
var output2 = tpl.execute( { } );
output2 == "My name is <strong>John</strong> and I'm <em>33</em> years old.";
haxe.Template.globals = { };

// foreach
tpl = new haxe.Template("My names are ::foreach names::<strong>::__current__::</strong>::end:: and I'm <em>::age::</em> years old.");
var output3 = tpl.execute( { names : ["John", "James", "Jimmy"], age : 33 } );
output3 == "My names are <strong>John</strong><strong>James</strong><strong>Jimmy</strong> and I'm <em>33</em> years old.";

output3 = tpl.execute( { names : [], age : 33 } );
output3 == "My names are  and I'm <em>33</em> years old.";

tpl = new haxe.Template("My names are ::foreach names::<strong>::name::</strong>::end:: and I'm <em>::age::</em> years old.");
output3 = tpl.execute( { names : [{ name : "John" }, { name : "James"} ], age : 33 } );
output3 == "My names are <strong>John</strong><strong>James</strong> and I'm <em>33</em> years old.";

// if
tpl = new haxe.Template("My names are ::foreach names::::if (name != null)::<strong>::name::</strong>::end::::end:: and I'm <em>::age::</em> years old.");
output3 = tpl.execute( { names : [{ name : "John" }, { name : "James" }], age : 33 } );
output3 == "My names are <strong>John</strong><strong>James</strong> and I'm <em>33</em> years old.";

output3 = tpl.execute( { names : [{ name : null }, { name : "James" }], age : 33 } );
output3 == "My names are <strong>James</strong> and I'm <em>33</em> years old.";

output3 = tpl.execute( { names : [{ name : "James" }], age : 33 } );
output3 == "My names are <strong>James</strong> and I'm <em>33</em> years old.";

// nested foreach
tpl = new haxe.Template("::foreach persons::My names are ::foreach names::::if (name != null)::<strong>::name.part::</strong>::end::::end:: and I'm <em>::age::</em> years old.<br />::end::");
output3 = tpl.execute( { persons : [{ names : [{ name : null }, { name : { part : "James" }}], age : 33 }]} );
output3 == "My names are <strong>James</strong> and I'm <em>33</em> years old.<br />";

// macros
function myfun( resolve : String -> Dynamic, title : String, p : Int ) {
	return "["+title+"="+(p * resolve("mult"))+"]";
}
var t1 = new haxe.Template("Call macro : $$myfun(Hello,::param::)");
var str = t1.execute({ param : 55, mult : 2 },{ myfun : myfun });
str == "Call macro : [Hello=110]";

var mcr = {
	a : function (resolver, s) { return "#" + s; },
	b : function (resolver, s1, s2) { return "_" + s1 + ":" + s2; }
};

tpl = new haxe.Template( "$$b(a,$$a(b))" );
var out3 = tpl.execute({}, mcr);
out3 == "_a:#b";

tpl = new haxe.Template( "$$a($$b(::a::,b))" );
var out4 = tpl.execute({a:"abc"}, mcr);
out4 == "#_abc:b";
