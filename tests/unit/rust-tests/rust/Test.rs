mod lib;
mod TestEnum;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		io::println((Some(@"Hello, world!")).unwrap());
		let mut a: i32 = i32::from_str((Some(@"8")).unwrap());
		let mut b: i32 = 2i32;
		let mut c: i32 = ((a / b) as i32);
		Test::useless(78.9f64);
		io::println((Some(@(TestEnum::c.to_str()))).unwrap());
		while (true) {
			io::println((Some(@({
				b += 1;
				b - 1
			};
			.to_str()))).unwrap());
			}
		}
	
	priv fn useless<T>(v: Option<@T>) -> Option<@T> {
		return v;
		}
	}
impl lib::HxObject for Test {
}
