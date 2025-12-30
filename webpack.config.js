const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const dist = path.resolve(__dirname, "dist");

module.exports = {
  mode: "development",
  devtool: "source-map",
  entry: {
    index: "./js/index.js"
  },
  output: {
    path: dist,
    filename: "[name].js"
  },
  devServer: {
    static: {
      directory: dist,
    },
    hot: true,
    open: true,
  },
  experiments: {
    asyncWebAssembly: true,
  },
  plugins: [
    new CopyPlugin({
      patterns: [{
        from: path.resolve(__dirname, "static"),
        to: "."
      }]
    }),

    new WasmPackPlugin({
      crateDirectory: __dirname,
      watchDirectories: [
        path.resolve(__dirname, "crates"),
      ],
    }),
  ],
};
