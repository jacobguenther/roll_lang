const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const webpack = require('webpack');
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

module.exports = {
	mode: "development", // "production" | "none"
	entry: {
		main: './www/index.js'
	},
	output: {
		path: path.resolve(__dirname, 'dist'),
		filename: 'index.js',
		libraryTarget: 'var',
		library: 'RollLang',
	},
	plugins: [
		new HtmlWebpackPlugin({
			title: 'Roll Lang',
			filename: './index.html',
			template: './www/index.html',
			scriptLoading: 'defer',
		}),
        new CopyWebpackPlugin({
            patterns: [
                { from: 'www/css', to: 'css' },
				{ from: 'www/html', to: 'pages' },
				{ from: 'www/assets', to: 'assets' },
			]
        }),
		new webpack.ProvidePlugin({
			TextDecoder: ['text-encoding', 'TextDecoder'],
			TextEncoder: ['text-encoding', 'TextEncoder']
		}),
		new WasmPackPlugin({
			crateDirectory: path.resolve(__dirname, "."),
			outName: "roll_lang",
		})
	],
	module: {
		rules: [

		],
	},
};