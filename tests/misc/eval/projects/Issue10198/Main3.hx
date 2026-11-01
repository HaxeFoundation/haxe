function fn1<T1,R1:T1>(v:R1):T1
	return null;

function fn2<T2,R2:T2>(v:R2):T2
	return null;

function fn3<T3,R3:T3>(v:R3):T3
	return null;

function main() {
	var b:Array<Int> = fn1(fn2(fn3(['s'])));
}