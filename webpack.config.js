const dotEnv = require("dotenv");
const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

dotEnv.config();

const debug = process.env.npm_lifecycle_event === 'build' ? false : true;
const mode = debug ? "development" : "production";
const entryPath = path.join(__dirname, "src/static/index.js");
const outputPath = path.join(__dirname, "dist");
const outputFilename = !debug ? "[name]-[hash].js" : "[name].js";

console.log("Webpack mode: " + mode);

module.exports = (env) => {
  return {
    mode: mode,
    entry: entryPath,
    output: {
      path: outputPath,
      filename: `static/js/${outputFilename}`,
    },
    resolve: {
      extensions: [".js", ".elm"],
      modules: ["node_modules"],
    },
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: {
            loader: "elm-webpack-loader",
            options: {
              pathToElm: "node_modules/.bin/elm",
            },
          },
        },
      ],
    },
    plugins: [
      new HtmlWebpackPlugin({
        template: "src/static/index.html",
        inject: "body",
        filename: "index.html",
      }),
    ],
  };
};
