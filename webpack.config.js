const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

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
    plugins: [
        new HtmlWebpackPlugin({
            title: 'Roll Lang',
            filename: 'index.html',
            template: 'index.html',
            scriptLoading: 'defer',
        }),
        new WasmPackPlugin({
            crateDirectory: path.resolve(__dirname, "."),
            // args: "",
            // extraArgs: "",
            // outDir: "pkg",
            // outName: "index",
            // forceWatch: true,
        }),

        new webpack.ProvidePlugin({
          TextDecoder: ['text-encoding', 'TextDecoder'],
          TextEncoder: ['text-encoding', 'TextEncoder']
        })
    ],
};