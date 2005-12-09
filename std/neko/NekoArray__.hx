
class NekoArray__<T> implements Array<T> {

	public var length : Int;

	private function new() {
	}

	private static function new1(a) {
		untyped {
			if( __dollar__typeof(a) != __dollar__tarray )
				__dollar__throw(a);
			var arr = new Array<Dynamic>();
			arr.__a = a;
			arr.length = __dollar__asize(a);
			return arr;
		}
	}

	public function concat(arr) {
		untyped {
			var a1 = this.__a;
			var a2 = arr.__a;
			var s1 = __dollar__asize(a1);
			var s2 = __dollar__asize(a2);
			var a = __dollar__amake(s1+s2);
			__dollar__ablit(a,0,a1,0,s1);
			__dollar__ablit(a,s1,a2,0,s2);
			return Array.new1(a);
		}
	}

	public function copy() {
		return untyped Array.new1(__dollar__acopy(this.__a));
	}

	public function indexes() {
		return untyped {
			a : this.__arr,
			p : 0,
			l : this.length,
			hasNext : function() {
				while( this.p < this.l ) {
					if( this.a[this.p] != null )
						return true;
					this.p += 1;
				}
				return false;
			},
			next : function() {
				var i = this.p;
				this.p += 1;
				return i;
			}
		};
	}

	public function iterator() {
		return untyped {
			a : this.__arr,
			p : 0,
			l : this.length,
			hasNext : function() {
				while( this.p < this.l ) {
					if( this.a[this.p] != null )
						return true;
					this.p += 1;
				}
				return false;
			},
			next : function() {
				var i = this.a[this.p];
				this.p += 1;
				return i;
			}
		};
	}

	public function insert( pos, x ) {
		//
	}

}