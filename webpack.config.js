const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

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
			minify: {
				collapseWhitespace: true,
				removeComments: true,
				removeEmptyAttributes: true,
				removeRedundantAttributes: true,
			}
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
	module: {
		rules: [
			{
				test: /\.css$/i,
				use: [MiniCssExtractPlugin.loader, 'css-loader'],
			},
		],
	},
};