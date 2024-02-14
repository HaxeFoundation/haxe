function main() {
	utest.UTest.run([
		new TestBasic(),
		new TestControlFlow(),
		#if js
		new TestGenerator(),
		new TestJsPromise(),
		#end
	]);
}