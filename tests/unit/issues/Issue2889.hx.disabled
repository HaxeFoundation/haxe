package unit.issues;

class Issue2889 extends Test {
	public function test()
	{
		function mapMappable <A,B>(m:Mappable<A>, f:A->B):Mappable<B> {
			return m.map(f);
		}
		var r = mapMappable([1], function (y) return y+1);
		var r:Array<Int> = cast r;
		eq(r.length,1);
		eq(2,r[0]);
	}
}

private typedef Mappable<Y> = {
	public function map <X>(f:Y->X):Array<X>;
}

