function main() foo();

inline function foo() {
	static var count = 5;
	trace(--count);
}