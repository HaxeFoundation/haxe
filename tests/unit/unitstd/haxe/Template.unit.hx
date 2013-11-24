#if !flash8
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

// macros
function myfun( resolve : String -> Dynamic, title : String, p : Int ) {
	return "["+title+"="+(p * resolve("mult"))+"]";
}
var t1 = new haxe.Template("Call macro : $$myfun(Hello,::param::)");
var str = t1.execute({ param : 55, mult : 2 },{ myfun : myfun });
str == "Call macro : [Hello=110]";

// foreach Array
var tpl = new haxe.Template("<ul>::foreach list::<li>::__current__::</li>::end::</ul>");
var output = tpl.execute({list:[0,1,2,3]});
output == "<ul><li>0</li><li>1</li><li>2</li><li>3</li></ul>";

// foreach IntIterator
var output = tpl.execute({list:0...3});
output == "<ul><li>0</li><li>1</li><li>2</li></ul>";
#end