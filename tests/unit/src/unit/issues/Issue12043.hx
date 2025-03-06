package unit.issues;

#if hl
private class Parent {
	@:packed public var st : St;
	@:packed public var st1 : St;
	public function new() {
	}
}

@:struct private class St {
	public var x : Single;
	public var y : Single;
	public var z : Single;
	public function new( x : Single, y : Single, z : Single ) {
		this.x = x;
		this.y = y;
		this.z = z;
	}
}
#end

class Issue12043 extends Test {
	#if hl
	var p : Parent;
	@:packed var st2 : St;
	var st3 : St;
	function test() {
		st3 = new St(16.3, 17.3, 18.3);
		st2 = new St(13.3, 14.3, 15.3);
		p = new Parent();
		p.st1 = st2;
		p.st = new St(10.3, 11.3, 12.3);
		feq(10.3, p.st.x);
		feq(11.3, p.st.y);
		feq(12.3, p.st.z);
		feq(13.3, p.st1.x);
		feq(14.3, p.st1.y);
		feq(15.3, p.st1.z);
		feq(13.3, st2.x);
		feq(14.3, st2.y);
		feq(15.3, st2.z);
		feq(16.3, st3.x);
		feq(17.3, st3.y);
		feq(18.3, st3.z);

		p.st.y = 7.7;
		st2.y = 8.8;
		st3.y = 9.9;
		feq(10.3, p.st.x);
		feq(7.7, p.st.y);
		feq(12.3, p.st.z);
		feq(13.3, p.st1.x);
		feq(14.3, p.st1.y);
		feq(15.3, p.st1.z);
		feq(13.3, st2.x);
		feq(8.8, st2.y);
		feq(15.3, st2.z);
		feq(16.3, st3.x);
		feq(9.9, st3.y);
		feq(18.3, st3.z);
	}
	#end
}
