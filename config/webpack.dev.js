const path = require("path");
const autoprefixer = require("autoprefixer");
const tailwindcss = require("tailwindcss")("./config/tailwind.config.js");

const Dotenv = require("dotenv-webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");

const servePath = path.join(__dirname, "../dist");
const entryPath = path.join(__dirname, "../src/static/index.js");
const outputPath = path.join(__dirname, "../dist");
const outputFilename = "[name].js";

module.exports = (env) => {
  return {
    mode: "development",
    entry: entryPath,
    output: {
      path: outputPath,
      filename: `static/js/${outputFilename}`,
      clean: true,
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
        {
          test: /\.(sa|sc|c)ss$/i,
          use: [
            "style-loader",
            "css-loader",
            {
              loader: "postcss-loader",
              options: {
                postcssOptions: {
                  plugins: [tailwindcss, autoprefixer],
                },
              },
            },
          ],
        },
      ],
    },
    plugins: [
      new HtmlWebpackPlugin({
        template: "src/static/index.html",
        inject: "body",
        filename: "index.html",
      }),
      new CopyWebpackPlugin({
        patterns: [
          {
            from: "src/static/img/",
            to: "static/img/",
          },
          {
            from: "src/static/fonts/",
            to: "static/fonts/",
          },
        ],
      }),
      new Dotenv(),
    ],
    devServer: {
      historyApiFallback: true,
      static: {
        directory: servePath,
      },
      port: 3000,
      hot: true,
    },
  };
};
