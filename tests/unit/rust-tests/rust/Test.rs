mod lib;
pub struct Test;
priv static num:i32 = 0i32;
pub impl Test {
	pub fn main() -> () {
		Test::testTuple();
		{
			let mut _g: i32 = 0i32;
			while (_g < 4i32) {
				let mut i: i32 = {
					_g += 1;
					_g - 1
				};
				{
					num += i;
					num
				}
			}
		}
	}
	priv fn testTuple() -> () {
		let mut t: Option<@(f64, i32, bool)> = Some(@(371.235f64, 38i32, false));
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
		match (lib::unwrap(t)) {
			(a, b, c) => a,
			_ => ()
		}
		match (lib::unwrap(t)) {
			(a, b, c) => b,
			_ => ()
		}
		match (lib::unwrap(t)) {
			(a, b, c) => c,
			_ => ()
		}
		}
}
impl lib::HxObject for Test {
}
