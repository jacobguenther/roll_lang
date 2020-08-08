# Roll Lang

Roll Lang is a domain specific language for interpreting common RPG and tabletop game dice rolls and math. You can check out the demo page and find out more at [Comming Soon]().
Roll Lang is primarily written in Rust and is meant to be compiled to WebAssembly for use client side use in the browser.

## Build Dependencies

* [rustup and Cargo](https://www.rust-lang.org/tools/install)
* [npm]()

## Building

In the project folder run the following commands to install the build dependencies

> npm install --save-dev webpack
> npm install --save-dev webpack-cli
> npm install --save-dev webpack-dev-server
> npm install --save-dev @wasm-tool/wasm-pack-plugin
> npm install --save-dev html-webpack-plugin
> npm install --save-dev text-encoding

Then build the project with
> npm run build

And run the demo page with
> npm run serve

You can check it out at localhost:8080 in your favorite browser.

## Authors

* Jacob Guenther - Initial work <jacobrwguenther@protonmail.com>

## License

This project is licensed under the MIT License - see the LICENSE.txt file for details.

