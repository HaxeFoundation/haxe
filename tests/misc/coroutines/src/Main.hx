function main() {
	utest.UTest.run([
		new TestBasic(),
		new TestControlFlow(),
		new TestGenerator(),
		#if js
		new TestJsPromise(),
		#end
	]);
}