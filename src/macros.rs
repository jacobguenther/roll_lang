// macros.rs

use std::collections::HashMap;

use web_sys::Window;
use web_sys::window;
use web_sys::Document;
use web_sys::Element;
use wasm_bindgen::JsCast;
use web_sys::HtmlDocument;
use web_sys::HtmlTextAreaElement;
use web_sys::HtmlInputElement;

pub type Name = String;
#[derive(Debug, Clone)]
pub struct MacroData {
	in_bar: bool,
	source: String,
}
impl MacroData {
	fn new(in_bar: bool, source: &str) -> MacroData {
		MacroData {
			in_bar: in_bar,
			source: source.to_string(),
		}
	}
}
pub type Macros = HashMap<Name, MacroData>;

pub trait MacrosT {
	fn init() -> Macros;

	fn handle_macro_update_create(&mut self);
	fn handle_macro_change_in_bar(&mut self, name: &str);
	fn handle_macro_delete(&mut self, name: &str);

	fn handle_macro_select(&self, name: &str);
	fn source(&self, name: &str) -> Option<String>;
}
impl MacrosT for Macros {
	fn init() -> Macros {
		let mut macros = Web::macros_from_cookies();
		for (name, data) in &macros {
			Web::add_macro_to_table(name, data.in_bar);
			if data.in_bar {
				Web::add_macro_to_bar(name);
			}
		}
		macros
	}
	fn handle_macro_update_create(&mut self) {
		let name = Web::macro_name_from_input();
		let in_bar = Web::macro_in_bar_from_checkbox();
		let source = Web::macro_text_from_textarea();

		if self.contains_key(&name) {
			// FIX ME replace with update macros row
			Web::remove_macro_from_table(&name);

			let currently_in_bar = self.get(&name).unwrap().in_bar;
			if in_bar && !currently_in_bar {
				Web::add_macro_to_bar(&name);
			} else if !in_bar && currently_in_bar {
				Web::remove_macro_from_bar(&name);
			}
		} else {
			if in_bar {
				Web::add_macro_to_bar(&name);
			}
		}
		Web::add_macro_to_table(&name, in_bar);

		let data = MacroData::new(in_bar, &source);
		Web::add_macro_cookie(&name, &data);
		self.insert(name, data);
	}
	fn handle_macro_change_in_bar(&mut self, name: &str) {
		if self.contains_key(name) {
			let in_bar = Web::get_element(&ID::row_check_box(name))
				.dyn_ref::<HtmlInputElement>().unwrap()
				.checked();
			let mut data = self.get(name).unwrap().clone();
			data.in_bar = in_bar;
			self.insert(name.to_string(), data);
			if in_bar {
				Web::add_macro_to_bar(name);
			} else {
				Web::remove_macro_from_bar(name);
			}
		}
	}
	fn handle_macro_delete(&mut self, name: &str) {
		if self.contains_key(name) {
			Web::remove_macro_cookie(name);
			Web::remove_macro_from_table(name);
			if self.get(name).unwrap().in_bar {
				Web::remove_macro_from_bar(name);
			}
			self.remove(name);
		}
	}

	fn handle_macro_select(&self, name: &str) {
		Web::get_element(&ID::create_macro_name())
			.dyn_ref::<HtmlInputElement>().unwrap()
			.set_value(name);

		let data = self.get(name).unwrap();
		Web::get_element(&ID::create_macro_text())
			.dyn_ref::<HtmlTextAreaElement>().unwrap()
			.set_value(&data.source);

		Web::get_element(&ID::create_macro_checkbox())
			.dyn_ref::<HtmlInputElement>().unwrap()
			.set_checked(data.in_bar);	
	}
	fn source(&self, name: &str) -> Option<String> {
		let data = self.get(name)?;
		Some(data.source.clone())
	}
}

struct ID {}
impl ID {
	fn macro_id(name: &str) -> String {
		name.replace(" ","-")
	}

	fn macros_table() -> String {
		String::from("macros-table")
	}
	fn row(name: &str) -> String {
		format!("row-{}", ID::macro_id(name))
	}
	fn row_check_box(name: &str) -> String {
		format!("place-in-bar-{}", ID::macro_id(name))
	}

	fn macros_bar() -> String {
		String::from("macros-bar")
	}
	fn bar_element(name: &str) -> String {
		format!("bar-{}", ID::macro_id(name))
	}

	fn create_macro_name() -> String {
		String::from("create-macro-name")
	}
	fn create_macro_text() -> String {
		String::from("create-macro-text")
	}
	fn create_macro_checkbox() -> String {
		String::from("create-macro-add-to-macros-bar")
	}
}
struct Web {}
impl Web {
	fn macros_from_cookies() -> Macros {
		let mut macros = Macros::new();

		let raw_cookies = Web::html_document().cookie().unwrap();
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
				Some(bar_str) => match Web::string_2_in_bar(bar_str) {
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
	fn window() -> Window {
		window().expect("web_sys::failed to get the window")
	}
	fn document() -> Document {
		Web::window()
			.document().expect("web_sys::failed to get the document")
	}
	fn html_document() -> HtmlDocument {
		Web::document()
			.dyn_into::<HtmlDocument>().expect("web_sys::failed to cast document to HtmlDocument")
	}
	fn create_element(element_type: &str) -> Element {
		Web::document()
			.create_element(element_type).expect(&format!("web_sys::Failed to create element of type {}", element_type))
	}

	fn get_element(id: &str) -> Element {
		Web::document()
			.get_element_by_id(&id)
			.expect(&format!("web_sys::failed to finde element with id \"{}\"", id))
	}
	fn macros_table() -> Element {
		Web::get_element(&ID::macros_table())
	}
	fn macros_bar() -> Element {
		Web::get_element(&ID::macros_bar())
	}

	fn macro_name_from_input() -> String {
		Web::get_element(&ID::create_macro_name())
			.dyn_ref::<HtmlInputElement>().unwrap()
			.value()
	}
	fn macro_text_from_textarea() -> String {
		Web::get_element(&ID::create_macro_text())
			.dyn_ref::<HtmlTextAreaElement>().unwrap()
			.value()	
	}
	fn macro_in_bar_from_checkbox() -> bool {
		Web::get_element(&ID::create_macro_checkbox())
			.dyn_ref::<HtmlInputElement>().unwrap()
			.checked()	
	}

	fn add_macro_to_bar(name: &str) {
		let button = Web::document().create_element("button").unwrap();
		let _result = button.set_attribute("id", &ID::bar_element(name));
		let _result = button.set_attribute(
			"onclick",
			&format!("RollLang.runMacro(\"{}\");", name)
		);
		button.set_inner_html(name);
		let _result = Web::macros_bar().append_child(&button);
	}
	fn add_macro_to_table(name: &str, in_bar: bool) {
		let row = Web::create_element("tr");
		let _result = row.set_attribute("id", &ID::row(name));

		let name_cell = Web::create_element("td");
		name_cell.set_inner_html(name);
		let _result = name_cell.set_attribute(
			"onclick",
			&format!("RollLang.handleMacroSelect(\"{}\");", name)
		);

		let place_in_bar_cell = Web::create_element("td");
		let place_in_bar = Web::create_element("input");
		let _result = place_in_bar.set_attribute("id", &ID::row_check_box(name));
		let _result = place_in_bar.set_attribute(
			"onchange",
			&format!("RollLang.handleMacroChangeInBar(\"{}\");", name)
		);
		let _result = place_in_bar.set_attribute("type", "checkbox");
		if in_bar {
			let _result = place_in_bar.set_attribute("checked", "");
		}
		let _result = place_in_bar_cell.append_child(&place_in_bar);


		let delete_cell = Web::create_element("td");
		delete_cell.set_inner_html("delete");
		let _result = delete_cell.set_attribute("id", &format!("delete-macro-{}", ID::macro_id(name)));
		let _result = delete_cell.set_attribute(
			"onclick",
			&format!("RollLang.handleMacroDelete(\"{}\");", name)
		);

		let _result = row.append_child(&name_cell);
		let _result = row.append_child(&place_in_bar_cell);
		let _result = row.append_child(&delete_cell);

		let table = Web::macros_table();
		let _result = table.append_child(&row);
	}
	fn update_macro_table_row(name: &str, in_bar: bool) {
		let checkbox = Web::document()
			.get_element_by_id(&ID::row_check_box(name)).unwrap()
			.dyn_ref::<HtmlInputElement>().unwrap()
			.set_checked(in_bar);
	}
	fn remove_macro_from_bar(name: &str) {
		Web::document()
			.get_element_by_id(&ID::bar_element(name)).unwrap()
			.remove();
	}
	fn remove_macro_from_table(name: &str) {
		Web::document()
			.get_element_by_id(&ID::row(name)).unwrap()
			.remove();
	}

	fn macro_cookie(name: &str) -> Option<MacroData> {
		let raw_cookies = Web::html_document().cookie().unwrap();
		for cookie in raw_cookies.split(";") {
			let mut key_value = cookie.split("=");
			let (key, source) = match key_value.next() {
				Some(key) => match key_value.next() {
					Some(value) => (key, value),
					_ => continue,
				},
				_ => continue,
			};

			let mut cookie_types = key.split(":");
			match cookie_types.next() {
				Some("macro") | Some(" macro") => match cookie_types.next() {
					Some("InBar") => match cookie_types.next() {
						Some(c_name) => if c_name == name {
							return Some(MacroData::new(true, source));
						},
						_ => continue,
					},
					Some("OutOfBar") => match cookie_types.next() {
						Some(c_name) => if c_name == name {
							return Some(MacroData::new(false, source));
						},
						_ => continue,
					},
					_ => continue,
				},
				_ => continue,
			};
		}

		None
	}
	fn add_macro_cookie(name: &str, data: &MacroData) {
		let time = "Mon, 01 Jan 2024 00:00:00 GMT";
		let _result = Web::html_document()
			.set_cookie(
				&format!("macro:{}={}:{}; SameSite=Strict; expires={};",
					name,
					Web::in_bar_2_string(data.in_bar),
					data.source,
					time
				)
			);
	}
	fn remove_macro_cookie(name: &str) {
		let _result = Web::html_document()
			.set_cookie(
				&format!("macro:{}=\"\"; SameSite=Strict; expires=Thur, 01 Jan 1970 00:00:00: UTC; path=/;",
					name
				)
			);
	}

	fn in_bar_2_string(in_bar: bool) -> String {
		match in_bar {
			true => "InBar",
			false => "OutOfBar",
		}.to_owned()
	}
	fn string_2_in_bar(text: &str) -> Option<bool> {
		match text {
			"InBar" => Some(true),
			"OutOfBar" => Some(false),
			_ => None,
		}
	}
}