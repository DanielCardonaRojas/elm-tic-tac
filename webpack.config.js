const path = require('path')
const Dotenv = require('dotenv-webpack');
require('dotenv').config();
const StringReplacePlugin = require('string-replace-webpack-plugin');
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');

const DIST_DIR = path.join(__dirname, "dist");
const CLIENT_DIR = path.join(__dirname, "src");

module.exports = function(env) {
  let dotenv =  new Dotenv();

  return {
    entry: './js/app.js',
    mode: 'development',
    output: {
      //path: DIST_DIR,
      //filename: 'bundle.js'
      path: path.resolve(__dirname),
      filename: 'dist/bundle.js'
    },
    module: {
      rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: 
          [
            StringReplacePlugin.replace({
              replacements: [
                { pattern: /\%SOCKETIO_URL\%/g, replacement: () => process.env.SOCKETIO_URL }
              ]
            }),
            {
              loader: 'elm-webpack-loader',
              options: {
                  debug: env ? env.development : false, 
                  warn: env ? env.development : false
              }
            }
        ]
      }, {
        test: /\.css$/,
        use: ["style-loader", "css-loader"]
      },
      {
        test: /\.html$/,
        use: [
          { loader: 'babel-loader' },
        ]
      }
    ]
    },
    plugins: [
      dotenv,
      new StringReplacePlugin(),
      new UglifyJsPlugin({
        test: /\.js($|\?)/i,
        parallel: true,
        cache: true
      })
    ]
  }
}

