pub struct Test<T> {
	value: Item
}
pub impl<T> Test<T> {
	pub fn main() -> () {
		SubTest::new();
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
				{
					let _v:i32 = n;
					_v += i;
					_v
				}
			}
		}
		return n;
	}
	pub fn new(val: Item) -> Option<@Test> {
		let mut self = Test {value: None}
		{
			let _v:Item = (rust::Lib::unwrap(self)).value;
			_v = val;
			_v
		}
		return @Some(self);
	}
}
impl TestInterface<Item> for Test<T> {
	pub fn get(&mut self) -> Item {
		return (rust::Lib::unwrap(self)).value;
	}
}
impl HxObject for Option<@Test> {
}
