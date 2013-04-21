mod lib;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		io::println(Some(@"Hello, world!"));
		Test::isCool(78.9f64);
		}
	
	priv fn isCool<T>(v: Option<~T>) -> bool {
		return true;
		}
	}
impl lib::HxObject for Test {
}
