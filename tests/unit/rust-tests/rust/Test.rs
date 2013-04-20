mod lib;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		/*Package io => yes*/io::io::println(Some(@"Hello, world!"));
		}
	}
/*Package Test => no*/impl lib::HxObject for Test {
}
