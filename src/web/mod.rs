// File web/mod.rs

use crate::macros::Macros;
use crate::macros::MacroData;

use wasm_bindgen::JsCast;
use web_sys::Window;
use web_sys::Document;
use web_sys::HtmlDocument;

pub fn window() -> Window {
	web_sys::window().expect("web_sys::failed to get the window")
}
pub fn document() -> Document {
	window()
		.document().expect("web_sys::failed to get the document")
}
pub fn html_document() -> HtmlDocument {
	document()
		.dyn_into::<HtmlDocument>().expect("web_sys::failed to cast document to HtmlDocument")
}

pub mod id {
	pub mod macros {
		pub fn id(name: &str) -> String {
			name.replace(" ","-")
		}

		pub mod create_mod {
			pub fn name() -> String {
				String::from("create-macro-name")
			}
			pub fn source() -> String {
				String::from("create-macro-text")
			}
			pub fn shortcut_checkbox() -> String {
				String::from("create-macro-add-to-macros-bar")
			}
		}

		pub fn table() -> String {
			String::from("macro-table")
		}
		pub mod table_mod {
			pub fn row(name: &str) -> String {
				format!("macro-table-row-{}", super::id(name))
			}
			pub fn checkbox(name: &str) -> String {
				format!("place-in-bar-{}", super::id(name))
			}
			pub fn delete(name: &str) -> String {
				format!("delete-{}", super::id(name))
			}
		}

		pub fn shortcuts_bar() -> String {
			String::from("macro-shortcuts")
		}
		pub mod shortcuts_bar_mod {
			pub fn shortcut(name: &str) -> String {
				format!("macro-shortcut-{}", super::id(name))
			}
		}
	}
}

pub mod element {
	use web_sys::Element;
	pub fn get_element(id: &str) -> Element {
		super::document()
			.get_element_by_id(&id)
			.expect(&format!("web_sys::failed to finde element with id \"{}\"", id))
	}
	pub fn create_element(element_type: &str) -> Element {
		super::document()
			.create_element(element_type).expect(&format!("web_sys::Failed to create element of type {}", element_type))
	}
	pub mod macros {
		use web_sys::Element;
		use super::super::id;
		pub fn table() -> Element {
			super::get_element(&id::macros::table())
		}
		pub mod table_mod {
			use web_sys::Element;
			use crate::web::id;
			use super::super::get_element;

			pub fn row(name: &str) -> Element {
				get_element(&id::macros::table_mod::row(name))
			}
			pub fn check_box(name: &str) -> Element {
				get_element(&id::macros::table_mod::checkbox(name))
			}
			pub fn delete(name: &str) -> Element {
				get_element(&id::macros::table_mod::delete(name))
			}
		}
		pub fn shortcuts_bar() -> Element {
			super::get_element(&id::macros::shortcuts_bar())
		}
	}
}

pub mod get_value {
	pub mod macros {
		pub mod create {
			use crate::web::*;
			use wasm_bindgen::JsCast;
			use web_sys::HtmlInputElement;
			use web_sys::HtmlTextAreaElement;
			
			pub fn name() -> String {
				element::get_element(&id::macros::create_mod::name())
					.dyn_ref::<HtmlInputElement>().unwrap()
					.value()
			}
			pub fn source() -> String {
				element::get_element(&id::macros::create_mod::source())
					.dyn_ref::<HtmlTextAreaElement>().unwrap()
					.value()	
			}
			pub fn checkbox() -> bool {
				element::get_element(&id::macros::create_mod::shortcut_checkbox())
					.dyn_ref::<HtmlInputElement>().unwrap()
					.checked()	
			}
		}
	}
}

pub mod cookies {
	use crate::web::*;
	pub fn add_macro(name: &str, data: &MacroData) {
		let time = "Mon, 01 Jan 2024 00:00:00 GMT";
		let _result = html_document()
			.set_cookie(
				&format!("macro:{}={}:{}; SameSite=Strict; expires={};",
					name,
					in_bar_2_string(data.in_bar),
					data.source,
					time
				)
			);
	}
	pub fn remove_macro(name: &str) {
		let _result = html_document()
			.set_cookie(
				&format!("macro:{}=\"\"; SameSite=Strict; expires=Thur, 01 Jan 1970 00:00:00: UTC; path=/;",
					name
				)
			);
	}
	pub fn in_bar_2_string(in_bar: bool) -> String {
		match in_bar {
			true => "InBar",
			false => "OutOfBar",
		}.to_owned()
	}
	pub fn string_2_in_bar(text: &str) -> Option<bool> {
		match text {
			"InBar" => Some(true),
			"OutOfBar" => Some(false),
			_ => None,
		}
	}
}

pub fn macros_from_cookies() -> Macros {
	let mut macros = Macros::new();

	let raw_cookies = html_document().cookie().unwrap();
	for cookie in raw_cookies.split(";") {
		let mut key_value = cookie.split("=");
		let (key, value) = match key_value.next() {
			Some(key) => match key_value.next() {
				Some(value) => (key, value),
				_ => continue,
			},
			_ => continue,
		};

		let mut cookie_types = key.split(":");
		let name = match cookie_types.next() {
			Some("macro") | Some(" macro") => match cookie_types.next() {
				Some(name) => name.to_string(),
				_ => continue,
			},
			_ => continue,
		};

		let mut data = value.split(":");
		let macro_data = match data.next() {
			Some(bar_str) => match cookies::string_2_in_bar(bar_str) {
				Some(bar_bool) => match data.next() {
					Some(source) => MacroData::new(bar_bool, source),
					None => continue,
				},
				None => continue,
			},
			None => continue,
		};

		macros.insert(name.to_string(), macro_data);
	}

	macros
}

pub fn add_macro_to_bar(name: &str) {
	let button = element::create_element("button");
	let _result = button.set_attribute("id", &id::macros::shortcuts_bar_mod::shortcut(name));
	let _result = button.set_attribute(
		"onclick",
		&format!("RollLang.runMacro(\"{}\");", name)
	);
	button.set_inner_html(name);
	let _result = element::macros::shortcuts_bar().append_child(&button);
}
pub fn add_macro_to_table(name: &str, in_bar: bool) {
	let row = element::create_element("tr");
	let _result = row.set_attribute("id", &id::macros::table_mod::row(name));

	let name_cell = element::create_element("td");
	name_cell.set_inner_html(name);
	let _result = name_cell.set_attribute(
		"onclick",
		&format!("RollLang.handleMacroSelect(\"{}\");", name)
	);

	let place_in_bar_cell = element::create_element("td");
	let place_in_bar = element::create_element("input");
	let _result = place_in_bar.set_attribute("id", &id::macros::table_mod::checkbox(name));
	let _result = place_in_bar.set_attribute(
		"onchange",
		&format!("RollLang.handleMacroChangeInBar(\"{}\");", name)
	);
	let _result = place_in_bar.set_attribute("type", "checkbox");
	if in_bar {
		let _result = place_in_bar.set_attribute("checked", "");
	}
	let _result = place_in_bar_cell.append_child(&place_in_bar);


	let delete_cell = element::create_element("td");
	delete_cell.set_inner_html("delete");
	let _result = delete_cell.set_attribute("id", &format!("delete-macro-{}", id::macros::id(name)));
	let _result = delete_cell.set_attribute(
		"onclick",
		&format!("RollLang.handleMacroDelete(\"{}\");", name)
	);

	let _result = row.append_child(&name_cell);
	let _result = row.append_child(&place_in_bar_cell);
	let _result = row.append_child(&delete_cell);

	let table = element::macros::table();
	let _result = table.append_child(&row);
}
// fn update_macro_table_row(name: &str, in_bar: bool) {
// 	Web::document()
// 		.get_element_by_id(&ID::row_check_box(name)).unwrap()
// 		.dyn_ref::<HtmlInputElement>().unwrap()
// 		.set_checked(in_bar);
// }
pub fn remove_macro_from_bar(name: &str) {
	document()
		.get_element_by_id(&id::macros::shortcuts_bar_mod::shortcut(name)).unwrap()
		.remove();
}
pub fn remove_macro_from_table(name: &str) {
	document()
		.get_element_by_id(&id::macros::table_mod::row(name)).unwrap()
		.remove();
}

// fn macro_cookie(name: &str) -> Option<MacroData> {
// 	let raw_cookies = Web::html_document().cookie().unwrap();
// 	for cookie in raw_cookies.split(";") {
// 		let mut key_value = cookie.split("=");
// 		let (key, source) = match key_value.next() {
// 			Some(key) => match key_value.next() {
// 				Some(value) => (key, value),
// 				_ => continue,
// 			},
// 			_ => continue,
// 		};

// 		let mut cookie_types = key.split(":");
// 		match cookie_types.next() {
// 			Some("macro") | Some(" macro") => match cookie_types.next() {
// 				Some("InBar") => match cookie_types.next() {
// 					Some(c_name) => if c_name == name {
// 						return Some(MacroData::new(true, source));
// 					},
// 					_ => continue,
// 				},
// 				Some("OutOfBar") => match cookie_types.next() {
// 					Some(c_name) => if c_name == name {
// 						return Some(MacroData::new(false, source));
// 					},
// 					_ => continue,
// 				},
// 				_ => continue,
// 			},
// 			_ => continue,
// 		};
// 	}

// 	None
// }