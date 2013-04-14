mod HxObject;
mod HxEnum;
pub struct Test<T> {
	value: Option<@T>
}
pub impl<T> Test<T> {
	pub fn main() -> () {
		let mut cl: Option<@HxObject> = Test;
		Test::new(78.533567f32);
		(rust::Lib::unwrap(Test))::triangular(20i32);
		(rust::Lib::unwrap(Test))::reltest();
	}
	priv fn reltest() -> i32 {
		return 8i32;
	}
	priv fn triangular(n: i32) -> i32 {
		{
			let mut _g: i32 = 1i32;
			while (_g < n) {
				let mut i: i32 = {
					_g += 1;
					_g - 1
				};
				n += i;
			}
		}
		return n;
	}
	pub fn new(val: Option<@T>) -> Option<@Test> {
		let mut self = Test {value: None}
		(rust::Lib::unwrap(self)).value = val;
		return @Some(self);
	}
}
impl HxObject for Option<@Test> {
}
