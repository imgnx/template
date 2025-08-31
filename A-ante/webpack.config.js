const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './src/index.jsx',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js',
    clean: true,
    publicPath: '/',
  },
  resolve: { extensions: ['.js', '.jsx'] },
  module: {
    rules: [
      { test: /\.(js|jsx)$/i, exclude: /node_modules/, use: 'babel-loader' }
    ]
  },
  devServer: {
    host: '0.0.0.0',
    port: 8080,
    hot: true,
    allowedHosts: 'all',
    historyApiFallback: true,
    proxy: { '/api': { target: 'http://localhost:8000', changeOrigin: true } }
  },
  plugins: [
    new HtmlWebpackPlugin({ template: 'in/index.html' })
  ]
};
