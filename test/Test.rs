mod HxObject;
mod HxEnum;
pub struct Test {
	c: Option<@str>, b: i32
}
pub impl Test {
	pub fn main() -> () {
		{
			let mut _g: i32 = 5i32;
			while (_g < 500i32) {
				let mut i: i32 = {_g += 1;
				_g - 1
				}
				io::println(i.toString());
			}
		}
		let mut a: @vec = [5i32,6i32,7i32,8i32];
		{
			let mut _g1: i32 = 0i32;
			while (_g1 < a.length) {
				let mut i1: i32 = a[_g1];
				{_g1 += 1;
				_g1
				}
				io::println((i1 * i1).toString());
			}
		}
		let mut v: i32 = 0i32;
		while (true) {
			if ({v += 1;
			v
			}
			 > 2i32) {
				break;
			}
		}
		let mut func: @fn(Option<@str>, i32)->Option<@str> = fn(|a1: Option<@str>, b: i32| -> Option<@str> {
			return Some(@"") + a1 + Some(@" ") + b * 100i32;
		}
		io::println(func(Some(@"Hello"),3i32));
		let $err = do task::try {
			fail!(Some(@"Random error"));
		}
		if Std::is($err, Option<@str>) {
			let mut error = $err{
				io::println(error);
			}
		}
	}
	pub fn _new() -> Option<@Test> {
		let mut self = {c: Some(@"No idea"), b: 23i32}
		return Some(self);
	}
}
impl HxObject for Option<@Test> {
	pub fn __get_field(&self, &field:str) {
		return match(field) {
			"c" => Some(self.c),
			"b" => Some(self.b),
			_ => None
		}
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"c" => self.c = value,
			"b" => self.b = value,
			_ => None
		}
	}
}
