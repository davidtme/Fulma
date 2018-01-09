var path = require("path");
var webpack = require("webpack");
var fableUtils = require("fable-utils");
var HtmlWebpackPlugin = require('html-webpack-plugin');

function resolve(filePath) {
    return path.join(__dirname, filePath)
}

var babelOptions = fableUtils.resolveBabelOptions({
    presets: [["env", { "modules": false }]]
});

var isProduction = process.argv.indexOf("-p") >= 0;
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

module.exports = {
    devtool: isProduction ? false : "source-map",
    entry: [
        "babel-polyfill",
        resolve('./docs.fsproj'),
    ],
    output: {
        path: resolve('./public'),
        filename: 'bundle.js'
    },
    plugins: [
        new HtmlWebpackPlugin({
            filename: resolve('./public/index.html'),
            template: resolve('./index.html'),
            hash: true,
            minify: isProduction ? {} : false
        })
    ],
    resolve: {
        modules: [resolve("../node_modules/")]
    },
    devServer: {
        contentBase: resolve('./public/'),
        port: 8080
    },
    module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/,
                use: {
                    loader: "fable-loader",
                    options: {
                        babel: babelOptions,
                        define: isProduction ? [] : ["DEBUG"],
                        fableCore: resolve("../../Fable/build/fable-core")
                    }
                }
            },
            {
                test: /\.js$/,
                exclude: /node_modules/,
                use: {
                    loader: 'babel-loader',
                    options: babelOptions
                },
            },
            {
                test: /\.s(a|c)ss$/,
                use: ["style-loader", "css-loader", "sass-loader"]
            },
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            }
        ]
    }
};
