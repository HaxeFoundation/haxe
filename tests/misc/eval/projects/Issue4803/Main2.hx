extern inline overload function test(a:String) {}
extern inline overload function test(a:Int) {}

function main() {
	test(function() {
		var x:String = 1;
	});
}
