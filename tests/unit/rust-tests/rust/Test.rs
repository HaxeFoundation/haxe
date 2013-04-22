mod lib;
mod TestEnum;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		io::println(Some(@"Hello, world!"));
		let mut a: i32 = i32::from_str(Some(@"8"));
		let mut b: i32 = 2i32;
		let mut c: i32 = (((a).unwrap() / (b).unwrap()) as i32);
		Test::useless(78.9f64);
		io::println(@(TestEnum::c.to_str()));
		}
	
	priv fn useless<T>(v: Option<@T>) -> Option<@T> {
		return v;
		}
	}
impl lib::HxObject for Test {
}
