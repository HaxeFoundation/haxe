mod HxObject;
mod SubTest;
mod Item;
mod Std;
pub struct Test<T> {
	value: Item
}
pub impl<T> Test<T> {
	pub fn get(&mut self) -> Item {
		return (rust::Lib::unwrap(self)).value;
	}
	pub fn main() -> () {
		let mut b: Option<~[Option<@str>]> = [];
		{
			b.grow(b.len() + 1i32,None);
			{
				b[b.len() - 1i32] = Some(@"8989");
				b[b.len() - 1i32]
			}
		}
		let mut a: Option<~[i32]> = Some(~[56i32,78i32,42i32,35i32]);
		{
			b.grow(b.len() + 1i32,None);
			{
				b[b.len() - 1i32] = (rust::Lib::unwrap(Std))::string(a);
				b[b.len() - 1i32]
			}
		}
		(rust::Lib::unwrap(Std))::string(b.len());
		{
			let mut _g: i32 = 0i32;
			while (_g < b.len()) {
				let mut i: Option<@str> = b[_g];
				{
					_g += 1;
					_g
				};
				i;
			}
		}
		SubTest::new();
	}
	pub fn new(val: Item) -> Option<@Test> {
		let mut self = Test {value: None}
		{
			(rust::Lib::unwrap(self)).value = val;
			(rust::Lib::unwrap(self)).value
		}
		return @Some(self);
	}
}
impl TestInterface<Item> for Test<T> {
	pub fn get(&mut self) -> Item {
		return (rust::Lib::unwrap(self)).value;
	}
}
impl HxObject for Test<T> {
}
