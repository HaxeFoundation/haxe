mod HxObject;
use rust::Lib;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		(rust::Lib::unwrap(Test))::testTuple();
	}
	priv fn testTuple() -> () {
		let mut t: Option<@(f64, i32, bool)> = Some((371.235f64, 38i32, false));
		let mut a: Option<~[Option<@(f64, i32, bool)>]> = [];
		let mut s: f32 = 23.663124f64;
		s;
		{
			a.grow(a.len() + 1i32,None);
			{
				a[a.len() - 1i32] = t;
				a[a.len() - 1i32]
			}
			a.len();
		}
		match t {
			(a, b, c) => a,
			_ => ()
		}
		match t {
			(a, b, c) => b,
			_ => ()
		}
		match t {
			(a, b, c) => c,
			_ => ()
		}
		}
}
impl HxObject for Test {
}
