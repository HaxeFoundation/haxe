mod HxObject;
mod HxEnum;
pub struct Test<T> {
	value: Option<@T>
}
pub impl<T> Test<T> {
	pub fn get(&mut self) -> Option<@T> {
		return (rust::Lib::unwrap(self)).value;
	}
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
			while ({
				let v:i32 = _g < n;
				v
			}) {
				let mut i: i32 = {
					_g += 1;
					_g - 1
				};
				{
					let v:i32 = n += i;
					v
				}
			}
		}
		return n;
	}
	pub fn new(val: Option<@T>) -> Option<@Test> {
		let mut self = Test {value: None}
		{
			let v:Option<@T> = (rust::Lib::unwrap(self)).value = val;
			v
		}
		return @Some(self);
	}
}
impl TestInterface<T> for Test<T> {
	pub fn get() -> Option<@Test> {
		let mut self = Test {value: None}
		return (rust::Lib::unwrap(self)).value;
		return @Some(self);
	}
}
impl HxObject for Option<@Test> {
}
