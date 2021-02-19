function main() {
	utest.UTest.run([
		new TestStaticFields(),
		new TestGenerator(),
		#if js
		new TestJsPromise(),
		#end
	]);
}