mod lib;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		let mut a: Option<@str> = Some(@"Hellooo world!");
		io::println(a.unwrap());
		{
			let mut _g: i32 = 0i32;
			while (_g < 5i32) {
				let mut i: i32 = {
					_g += 1;
					_g - 1
				};
				io::println(Some(i.to_str().to_owned()).unwrap());
				}
			}
		io::println(if (a.is_none()) {
			Some(@"Yup")
		} else {
			Some(@"Nope")
		}.unwrap());
		}
	}
impl lib::HxObject for Test {
	}
impl ToStr for Test {
	pub fn to_str(&self) -> ~str {
		return ~"Test";
	}}
