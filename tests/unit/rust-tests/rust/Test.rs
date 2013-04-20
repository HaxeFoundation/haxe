mod lib;
pub struct Test;
priv static num:i32 = 0i32;
pub impl Test {
	pub fn main() -> () {
		let mut b: f64 = 8.9f64;
		let mut c: f32 = f32::NaN;
		{
			c = 8i32;
			c
		}
		let mut d: f32 = 0.8678f64;
		{
			d = None;
			d
		}
		Test::new();
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
		let mut p: Option<@(Option<@str>, i32)> = Some(@(Some(@"Nope"), 22i32));
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
		{
			let mut _r:Option<@A> = None;
			match *(t).unwrap() {
				(a, _, _) => _r = a
			}
			_r
		}
		{
			let mut _r2:Option<@B> = None;
			match *(t).unwrap() {
				(_, b, _) => _r2 = b
			}
			_r2
		}
		{
			let mut _r3:Option<@C> = None;
			match *(t).unwrap() {
				(_, _, c) => _r3 = c
			}
			_r3
		}
		{
			let mut _r4:Option<@A> = None;
			match *(p).unwrap() {
				(a, _) => _r4 = a
			}
			_r4
		} + {
			let mut _r5:Option<@B> = None;
			match *(p).unwrap() {
				(_, b) => _r5 = b
			}
			_r5
		}
	}
	pub fn new() -> Option<@Test> {
		let mut self = Test {}
		return Some(@self);
	}
}
impl lib::HxObject for Test {
}
