function method<T=String>():Array<T> return [];

function main() {
	var a = method();
	a.push("default");
}
