const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = {
	mode: "development",
	entry: {
		main: './index.js'
	},
	output: {
		path: path.resolve(__dirname, 'dist'),
		filename: 'index.js',
		libraryTarget: 'var',
		library: 'RollLang',
	},
	module: {
		rules: [
			{
				test: /\.css$/i,
				use: [MiniCssExtractPlugin.loader, 'css-loader'],
			},
		],
	},
	plugins: [
		new HtmlWebpackPlugin({
			title: 'Roll Lang',
			template: 'index.html',
			scriptLoading: 'defer',
		}),
		new MiniCssExtractPlugin(),
		new WasmPackPlugin({
			crateDirectory: path.resolve(__dirname, "."),
		}),
		new webpack.ProvidePlugin({
		  TextDecoder: ['text-encoding', 'TextDecoder'],
		  TextEncoder: ['text-encoding', 'TextEncoder']
		})
	],
};