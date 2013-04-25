mod lib;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		let mut func: @fn(i32)->() = (||{()});
		func = (|n: i32| -> () {
			io::println(Some((n + 1i32).to_str().to_owned()).unwrap())
		});
		func(34i32);
		}
	}
impl lib::HxObject for Test {
	}
impl ToStr for Test {
	pub fn to_str(&self) -> ~str {
		return ~"Test";
	}
}
