pub trait HxObject {
	pub fn __get_field(&self, field:@str) -> Option<HxObject>;
	pub fn __set_field(&self, field:@str, value:Option<@HxObject>) -> Option<@HxObject>;
	pub fn toString(&self) -> @str;
}
impl ToStr for HxObject {
	pub fn to_str(&self) -> ~str {
		return self.toString();
	}
}
impl HxObject for @BaseIter {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @vec {
	pub fn __get_field(&self, &field:str) {
		return match(field) {
			"length" => Some(self.length),
			_ => None
		}
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"length" => self.length = value,
			_ => None
		}
	}
}
impl HxObject for @ArrayAccess {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for Option<@str> {
	pub fn __get_field(&self, &field:str) {
		return match(field) {
			"length" => Some(self.length),
			_ => None
		}
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"length" => self.length = value,
			_ => None
		}
	}
}
impl HxObject for @Type {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @haxe::EnumTools {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @haxe::EnumValueTools {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @Clone {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @ToStr {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @hashmap::linear::LinearMap {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @io {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @io::Reader {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @io::Writer {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @os {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @os::Pipe {
	pub fn __get_field(&self, &field:str) {
		return match(field) {
			"pout" => Some(self.pout),
			"pin" => Some(self.pin),
			_ => None
		}
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"pout" => self.pout = value,
			"pin" => self.pin = value,
			_ => None
		}
	}
}
impl HxObject for @path::Path {
	pub fn __get_field(&self, &field:str) {
		return match(field) {
			"components" => Some(self.components),
			"is_absolute" => Some(self.is_absolute),
			_ => None
		}
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"components" => self.components = value,
			"is_absolute" => self.is_absolute = value,
			_ => None
		}
	}
}
