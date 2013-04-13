pub trait HxObject {
	pub fn __get_field(&self, field:@str) -> Option<HxObject>;
	pub fn __set_field(&mut self, field:@str, value:Option<@HxObject>) -> Option<@HxObject>;
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
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @vec {
	pub fn __get_field(&self, &field:str) {
		return match(field) {
			"length" => Some(self.length),
			_ => None
		}
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
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
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @str {
	pub fn __get_field(&self, &field:str) {
		return match(field) {
			"length" => Some(self.length),
			_ => None
		}
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
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
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @haxe::EnumTools {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		}
}
impl HxObject for @haxe::EnumValueTools {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		}
}
